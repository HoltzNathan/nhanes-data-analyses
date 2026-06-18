# NHANES Data Analysis
# Author: Nathan Holtz
# Description: Regression analysis to identify predictors of C-reactive protein (CRP)
#
# How to run:
# 1. Install required packages listed below.
# 2. Run this script from the main project folder.
# 3. The script will download required NHANES .xpt files into /data if they are not already present.
# 4. Tables and figures will be saved into /outputs.

# -----------------------------
# 1. Setup
# -----------------------------

required_packages <- c("haven", "dplyr", "ggplot2", "openxlsx", "car")

installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

library(haven)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(car)

data_dir <- "data"
output_dir <- "outputs"

dir.create(data_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)

# NHANES August 2021-August 2023 public-use files
nhanes_files <- c(
  ALQ    = "ALQ_L.xpt",
  DPQ    = "DPQ_L.xpt",
  HSCRP  = "HSCRP_L.xpt",
  SMQ    = "SMQ_L.xpt",
  TRIGLY = "TRIGLY_L.xpt",
  BPXO   = "BPXO_L.xpt"
)

base_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/"

download_nhanes_file <- function(file_name) {
  local_path <- file.path(data_dir, file_name)
  file_url <- paste0(base_url, file_name)

  if (!file.exists(local_path)) {
    message("Downloading: ", file_name)
    download.file(file_url, destfile = local_path, mode = "wb")
  } else {
    message("File already exists: ", file_name)
  }

  local_path
}

# Download files if needed
file_paths <- lapply(nhanes_files, download_nhanes_file)

# -----------------------------
# 2. Load NHANES data
# -----------------------------

ALQ    <- read_xpt(file_paths$ALQ)
DPQ    <- read_xpt(file_paths$DPQ)
HSCRP  <- read_xpt(file_paths$HSCRP)
SMQ    <- read_xpt(file_paths$SMQ)
TRIGLY <- read_xpt(file_paths$TRIGLY)
BPXO   <- read_xpt(file_paths$BPXO)

# Helper function:
# Uses variable names when available, with fallback column positions matching the original course project code.
extract_variable <- function(df, possible_names, fallback_index) {
  matched_name <- possible_names[possible_names %in% names(df)][1]

  if (!is.na(matched_name)) {
    return(df[[matched_name]])
  }

  return(df[[fallback_index]])
}

# -----------------------------
# 3. Select and recode variables
# -----------------------------
# The project focuses on CRP as the inflammatory biomarker and behavioral/clinical predictors.
# Non-drinkers and non-smokers are coded as 0 for the respective consumption variables.

hscrp_sub <- data.frame(
  SEQN = HSCRP$SEQN,
  HSCRP_var = extract_variable(HSCRP, c("LBXHSCRP"), 3)
)

alq_sub <- data.frame(
  SEQN = ALQ$SEQN,
  ALQ_var = ifelse(
    extract_variable(ALQ, c("ALQ111"), 2) == 2,
    0,
    extract_variable(ALQ, c("ALQ130"), 4)
  )
)

smq_sub <- data.frame(
  SEQN = SMQ$SEQN,
  SMQ_var = ifelse(
    extract_variable(SMQ, c("SMQ020"), 2) == 2,
    0,
    extract_variable(SMQ, c("SMD650"), 5)
  )
)

dpq_sub <- data.frame(
  SEQN = DPQ$SEQN,
  DPQ_var = extract_variable(DPQ, c("DPQ010"), 3)
)

trigly_sub <- data.frame(
  SEQN = TRIGLY$SEQN,
  TRIGLY_var = extract_variable(TRIGLY, c("LBXTR"), 5)
)

bpxo_sub <- data.frame(
  SEQN = BPXO$SEQN,
  BPXO_var = extract_variable(BPXO, c("BPXOSY1"), 8)
)

# -----------------------------
# 4. Merge datasets
# -----------------------------
# SEQN is the NHANES respondent identifier used to join files across components.

main_df_raw <- hscrp_sub %>%
  left_join(alq_sub, by = "SEQN") %>%
  left_join(smq_sub, by = "SEQN") %>%
  left_join(dpq_sub, by = "SEQN") %>%
  left_join(trigly_sub, by = "SEQN") %>%
  left_join(bpxo_sub, by = "SEQN")

saveRDS(main_df_raw, file.path(output_dir, "main_df_raw.rds"))

message("Raw merged dataset:")
print(summary(main_df_raw))
print(colSums(is.na(main_df_raw)))

# -----------------------------
# 5. Exploratory visualizations
# -----------------------------

plot_histogram <- function(data, variable, title, x_label, file_name, subtitle = NULL) {
  p <- ggplot(data, aes(x = .data[[variable]])) +
    geom_histogram(bins = 30, color = "black", fill = "skyblue") +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = "Frequency"
    ) +
    theme_minimal()

  ggsave(file.path(output_dir, file_name), plot = p, width = 7, height = 5)
  print(p)
}

plot_density <- function(data, variable, title, x_label, file_name) {
  p <- ggplot(data, aes(x = .data[[variable]])) +
    geom_density(fill = "skyblue", alpha = 0.5) +
    labs(
      title = title,
      x = x_label,
      y = "Density"
    ) +
    theme_minimal()

  ggsave(file.path(output_dir, file_name), plot = p, width = 7, height = 5)
  print(p)
}

plot_qq <- function(data, variable, title, file_name) {
  p <- ggplot(data, aes(sample = .data[[variable]])) +
    stat_qq() +
    stat_qq_line() +
    labs(title = title) +
    theme_minimal()

  ggsave(file.path(output_dir, file_name), plot = p, width = 7, height = 5)
  print(p)
}

plot_boxplot <- function(data, variable, title, y_label, file_name) {
  p <- ggplot(data, aes(y = .data[[variable]])) +
    geom_boxplot(fill = "skyblue", color = "black") +
    labs(
      title = title,
      y = y_label,
      x = ""
    ) +
    theme_minimal()

  ggsave(file.path(output_dir, file_name), plot = p, width = 7, height = 5)
  print(p)
}

variables <- list(
  HSCRP_var  = list(title = "C-Reactive Protein", label = "C-Reactive Protein (mg/L)"),
  ALQ_var    = list(title = "Alcoholic Drinks Consumed per Day", label = "Alcoholic Drinks Consumed per Day"),
  SMQ_var    = list(title = "Cigarettes Smoked per Day", label = "Cigarettes Smoked per Day"),
  DPQ_var    = list(title = "Depression Symptom Score", label = "Depression Symptom Score"),
  TRIGLY_var = list(title = "Triglycerides", label = "Triglycerides (mg/dL)"),
  BPXO_var   = list(title = "Systolic Blood Pressure", label = "Systolic Blood Pressure (mmHg)")
)

for (var in names(variables)) {
  plot_histogram(
    data = main_df_raw,
    variable = var,
    title = variables[[var]]$title,
    x_label = variables[[var]]$label,
    file_name = paste0("hist_", var, "_raw.png")
  )
}

# -----------------------------
# 6. Clean improbable values / outliers
# -----------------------------
# Extreme values are set to NA before modeling. These thresholds follow the original course project workflow.

main_df_clean <- main_df_raw %>%
  mutate(
    ALQ_var   = ifelse(ALQ_var > 100, NA, ALQ_var),
    SMQ_var   = ifelse(SMQ_var > 100, NA, SMQ_var),
    HSCRP_var = ifelse(HSCRP_var > 30, NA, HSCRP_var)
  )

saveRDS(main_df_clean, file.path(output_dir, "main_df_clean.rds"))

message("Cleaned dataset:")
print(summary(main_df_clean))
print(colSums(is.na(main_df_clean)))
message("Complete cases: ", sum(complete.cases(main_df_clean)))

for (var in c("ALQ_var", "SMQ_var", "HSCRP_var")) {
  plot_histogram(
    data = main_df_clean,
    variable = var,
    title = variables[[var]]$title,
    subtitle = "Outliers Removed",
    x_label = variables[[var]]$label,
    file_name = paste0("hist_", var, "_clean.png")
  )
}

for (var in names(variables)) {
  plot_density(
    data = main_df_clean,
    variable = var,
    title = paste("Density Plot:", variables[[var]]$title),
    x_label = variables[[var]]$label,
    file_name = paste0("density_", var, ".png")
  )

  plot_qq(
    data = main_df_clean,
    variable = var,
    title = paste("Q-Q Plot:", variables[[var]]$title),
    file_name = paste0("qq_", var, ".png")
  )

  plot_boxplot(
    data = main_df_clean,
    variable = var,
    title = variables[[var]]$title,
    y_label = variables[[var]]$label,
    file_name = paste0("boxplot_", var, ".png")
  )
}

# -----------------------------
# 7. Transform skewed variables
# -----------------------------
# CRP, alcohol use, and smoking variables are right-skewed, so log transformations are applied.

analysis_df <- main_df_clean %>%
  mutate(
    log_HSCRP = log(HSCRP_var + 0.1),
    log_ALQ   = log1p(ALQ_var),
    log_SMQ   = log1p(SMQ_var)
  )

saveRDS(analysis_df, file.path(output_dir, "analysis_df.rds"))

message("Analysis dataset:")
print(summary(analysis_df))
print(colSums(is.na(analysis_df)))
message("Complete cases: ", sum(complete.cases(analysis_df)))

plot_histogram(
  data = analysis_df,
  variable = "log_HSCRP",
  title = "C-Reactive Protein",
  subtitle = "Log Transformation Applied",
  x_label = "log(CRP + 0.1)",
  file_name = "hist_log_HSCRP.png"
)

plot_histogram(
  data = analysis_df,
  variable = "log_ALQ",
  title = "Alcoholic Drinks Consumed per Day",
  subtitle = "log(1 + x) Transformation Applied",
  x_label = "log(1 + Alcoholic Drinks)",
  file_name = "hist_log_ALQ.png"
)

plot_histogram(
  data = analysis_df,
  variable = "log_SMQ",
  title = "Cigarettes Smoked per Day",
  subtitle = "log(1 + x) Transformation Applied",
  x_label = "log(1 + Cigarettes)",
  file_name = "hist_log_SMQ.png"
)

# -----------------------------
# 8. Simple regression models
# -----------------------------

simple_models <- list(
  Alcohol       = lm(log_HSCRP ~ log_ALQ, data = analysis_df),
  Smoking       = lm(log_HSCRP ~ log_SMQ, data = analysis_df),
  Depression    = lm(log_HSCRP ~ DPQ_var, data = analysis_df),
  Triglycerides = lm(log_HSCRP ~ TRIGLY_var, data = analysis_df),
  BloodPressure = lm(log_HSCRP ~ BPXO_var, data = analysis_df)
)

for (model_name in names(simple_models)) {
  message("\nSimple model: ", model_name)
  print(summary(simple_models[[model_name]]))
}

plot_regression <- function(data, x_var, y_var, title, x_label, y_label, file_name) {
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(alpha = 0.4, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal()

  ggsave(file.path(output_dir, file_name), plot = p, width = 7, height = 5)
  print(p)
}

plot_regression(
  analysis_df, "log_ALQ", "log_HSCRP",
  "Simple Linear Regression: Alcohol vs CRP",
  "log(1 + Alcoholic Drinks per Day)",
  "log(CRP + 0.1)",
  "regression_alcohol_crp.png"
)

plot_regression(
  analysis_df, "log_SMQ", "log_HSCRP",
  "Simple Linear Regression: Smoking vs CRP",
  "log(1 + Cigarettes per Day)",
  "log(CRP + 0.1)",
  "regression_smoking_crp.png"
)

plot_regression(
  analysis_df, "DPQ_var", "log_HSCRP",
  "Simple Linear Regression: Depression vs CRP",
  "Depression Symptom Score",
  "log(CRP + 0.1)",
  "regression_depression_crp.png"
)

plot_regression(
  analysis_df, "TRIGLY_var", "log_HSCRP",
  "Simple Linear Regression: Triglycerides vs CRP",
  "Triglycerides (mg/dL)",
  "log(CRP + 0.1)",
  "regression_triglycerides_crp.png"
)

plot_regression(
  analysis_df, "BPXO_var", "log_HSCRP",
  "Simple Linear Regression: Systolic Blood Pressure vs CRP",
  "Systolic Blood Pressure (mmHg)",
  "log(CRP + 0.1)",
  "regression_blood_pressure_crp.png"
)

# -----------------------------
# 9. Descriptive statistics
# -----------------------------

get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

get_stats <- function(x) {
  x_nonmiss <- x[!is.na(x)]

  data.frame(
    N = length(x_nonmiss),
    Mean = mean(x_nonmiss),
    Median = median(x_nonmiss),
    Mode = get_mode(x_nonmiss),
    Min = min(x_nonmiss),
    Max = max(x_nonmiss),
    Range = max(x_nonmiss) - min(x_nonmiss),
    Variance = var(x_nonmiss),
    SD = sd(x_nonmiss)
  )
}

descriptive_results <- bind_rows(
  cbind(Variable = "ALQ_var",    Transformation = "Before",      get_stats(analysis_df$ALQ_var)),
  cbind(Variable = "ALQ_var",    Transformation = "After log1p", get_stats(analysis_df$log_ALQ)),
  cbind(Variable = "SMQ_var",    Transformation = "Before",      get_stats(analysis_df$SMQ_var)),
  cbind(Variable = "SMQ_var",    Transformation = "After log1p", get_stats(analysis_df$log_SMQ)),
  cbind(Variable = "DPQ_var",    Transformation = "None",        get_stats(analysis_df$DPQ_var)),
  cbind(Variable = "TRIGLY_var", Transformation = "None",        get_stats(analysis_df$TRIGLY_var)),
  cbind(Variable = "BPXO_var",   Transformation = "None",        get_stats(analysis_df$BPXO_var))
)

write.csv(
  descriptive_results,
  file.path(output_dir, "descriptive_statistics.csv"),
  row.names = FALSE
)

print(descriptive_results)

# -----------------------------
# 10. Nested multiple regression models
# -----------------------------
# Predictors are added in stages to evaluate how model fit changes as additional variables are included.

models <- list(
  "Model 1" = lm(log_HSCRP ~ log_SMQ, data = analysis_df),
  "Model 2" = lm(log_HSCRP ~ log_SMQ + log_ALQ, data = analysis_df),
  "Model 3" = lm(log_HSCRP ~ log_SMQ + log_ALQ + DPQ_var, data = analysis_df),
  "Model 4" = lm(log_HSCRP ~ log_SMQ + log_ALQ + DPQ_var + TRIGLY_var, data = analysis_df),
  "Model 5" = lm(log_HSCRP ~ log_SMQ + log_ALQ + DPQ_var + TRIGLY_var + BPXO_var, data = analysis_df)
)

predictors <- c("log_SMQ", "log_ALQ", "DPQ_var", "TRIGLY_var", "BPXO_var")

predictor_labels <- c(
  "Smoking",
  "Alcohol",
  "Depression",
  "Triglycerides",
  "Blood_Pressure"
)

get_f_p <- function(model) {
  f <- summary(model)$fstatistic
  pf(f[1], f[2], f[3], lower.tail = FALSE)
}

regression_results <- data.frame(
  Model = names(models),
  Smoking_Coefficient = NA,
  Smoking_p_value = NA,
  Alcohol_Coefficient = NA,
  Alcohol_p_value = NA,
  Depression_Coefficient = NA,
  Depression_p_value = NA,
  Triglycerides_Coefficient = NA,
  Triglycerides_p_value = NA,
  Blood_Pressure_Coefficient = NA,
  Blood_Pressure_p_value = NA,
  R2 = NA,
  F_Significance = NA
)

for (i in seq_along(models)) {
  model_summary <- summary(models[[i]])
  coefs <- model_summary$coefficients

  for (j in seq_along(predictors)) {
    pred <- predictors[j]

    if (pred %in% rownames(coefs)) {
      regression_results[i, paste0(predictor_labels[j], "_Coefficient")] <- coefs[pred, "Estimate"]
      regression_results[i, paste0(predictor_labels[j], "_p_value")] <- coefs[pred, "Pr(>|t|)"]
    }
  }

  regression_results$R2[i] <- model_summary$r.squared
  regression_results$F_Significance[i] <- get_f_p(models[[i]])
}

regression_results_rounded <- regression_results
regression_results_rounded[, -1] <- round(regression_results_rounded[, -1], 4)

write.csv(
  regression_results_rounded,
  file.path(output_dir, "regression_summary_table.csv"),
  row.names = FALSE
)

print(regression_results_rounded)

# Also save a formatted Excel version for presentation/use outside R.
wb <- createWorkbook()
addWorksheet(wb, "Regression Summary")

writeData(wb, "Regression Summary", "Multiple Linear Regression Summary", startRow = 1, startCol = 1)
mergeCells(wb, "Regression Summary", cols = 1:13, rows = 1)

writeData(wb, "Regression Summary", x = regression_results_rounded, startRow = 3, startCol = 1)

header_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)

title_style <- createStyle(
  textDecoration = "bold",
  fontSize = 14,
  halign = "center"
)

addStyle(wb, "Regression Summary", title_style, rows = 1, cols = 1)
addStyle(wb, "Regression Summary", header_style, rows = 3, cols = 1:13, gridExpand = TRUE)
setColWidths(wb, "Regression Summary", cols = 1:13, widths = "auto")

saveWorkbook(
  wb,
  file = file.path(output_dir, "regression_summary_table.xlsx"),
  overwrite = TRUE
)

# -----------------------------
# 11. Final model diagnostics
# -----------------------------

model_final <- models[["Model 5"]]
print(summary(model_final))

residuals_final <- residuals(model_final)
fitted_final <- fitted(model_final)

print(summary(residuals_final))

png(file.path(output_dir, "diagnostic_residual_histogram.png"), width = 800, height = 600)
hist(
  residuals_final,
  breaks = 30,
  main = "Histogram of Residuals",
  xlab = "Residuals"
)
dev.off()

png(file.path(output_dir, "diagnostic_residuals_vs_fitted.png"), width = 800, height = 600)
plot(
  fitted_final,
  residuals_final,
  main = "Residuals vs Fitted",
  xlab = "Fitted Values",
  ylab = "Residuals"
)
abline(h = 0, lty = 2)
dev.off()

png(file.path(output_dir, "diagnostic_qq_residuals.png"), width = 800, height = 600)
qqnorm(residuals_final)
qqline(residuals_final, lty = 2)
dev.off()

png(file.path(output_dir, "diagnostic_model_plots.png"), width = 900, height = 900)
par(mfrow = c(2, 2))
plot(model_final)
par(mfrow = c(1, 1))
dev.off()

cooks_d <- cooks.distance(model_final)

png(file.path(output_dir, "diagnostic_cooks_distance.png"), width = 800, height = 600)
plot(
  cooks_d,
  type = "h",
  main = "Cook's Distance",
  ylab = "Cook's distance"
)
abline(h = 4 / nobs(model_final), lty = 2)
dev.off()

influential_cases <- which(cooks_d > 4 / nobs(model_final))
write.csv(
  data.frame(SEQN = analysis_df$SEQN[influential_cases], Cooks_Distance = cooks_d[influential_cases]),
  file.path(output_dir, "influential_cases.csv"),
  row.names = FALSE
)

# -----------------------------
# 12. Multicollinearity and correlations
# -----------------------------

predictor_data <- analysis_df[, c("log_SMQ", "log_ALQ", "DPQ_var", "TRIGLY_var", "BPXO_var")]

correlation_matrix <- round(cor(predictor_data, use = "complete.obs"), 3)
write.csv(
  correlation_matrix,
  file.path(output_dir, "predictor_correlation_matrix.csv")
)

vif_results <- data.frame(
  Predictor = names(vif(model_final)),
  VIF = as.numeric(vif(model_final))
)

write.csv(
  vif_results,
  file.path(output_dir, "vif_results.csv"),
  row.names = FALSE
)

print(correlation_matrix)
print(vif_results)

# -----------------------------
# 13. Interaction screening
# -----------------------------
# Pairwise interaction terms are screened to identify predictors that may have non-additive associations with CRP.

interaction_vars <- c("log_SMQ", "log_ALQ", "DPQ_var", "TRIGLY_var", "BPXO_var")

interaction_results <- data.frame()

for (i in 1:(length(interaction_vars) - 1)) {
  for (j in (i + 1):length(interaction_vars)) {

    v1 <- interaction_vars[i]
    v2 <- interaction_vars[j]

    formula_text <- paste(
      "log_HSCRP ~",
      v1, "+", v2, "+", paste0(v1, ":", v2)
    )

    mod <- lm(as.formula(formula_text), data = analysis_df)
    s <- summary(mod)

    interaction_name <- paste0(v1, ":", v2)

    pval <- coef(s)[interaction_name, "Pr(>|t|)"]
    beta <- coef(s)[interaction_name, "Estimate"]

    interaction_results <- rbind(
      interaction_results,
      data.frame(
        Var1 = v1,
        Var2 = v2,
        Interaction_Beta = beta,
        P_Value = pval
      )
    )
  }
}

interaction_results <- interaction_results[order(interaction_results$P_Value), ]

write.csv(
  interaction_results,
  file.path(output_dir, "interaction_results.csv"),
  row.names = FALSE
)

print(interaction_results)

message("Analysis complete. Outputs saved in the /outputs folder.")
