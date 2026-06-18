# NHANES Health Data Analysis

## Overview

This project analyzes publicly available NHANES data to identify key drivers of inflammation, measured by C-reactive protein (CRP). The goal was to explore relationships between behavioral, physiological, and clinical variables using statistical modeling.

This project demonstrates an end-to-end data analysis workflow, including data acquisition, cleaning, statistical modeling, and interpretation.

## Dataset

* Source: National Health and Nutrition Examination Survey (NHANES)
* Sample size: ~thousands of observations
* Variables include alcohol use, smoking behavior, depression scores, triglycerides, and blood pressure

## Methods

* Data cleaning and preprocessing (handling missing data, recoding variables, outlier removal)
* Multiple linear regression modeling
* Log transformation to address skewed distributions
* Evaluation of interaction effects and multicollinearity
* Exploratory data analysis and visualization

## Tools

* R
* ggplot2
* Statistical modeling techniques

## Key Findings

## Key Findings
* Identified significant relationships between CRP and smoking, triglycerides, and depression
* Demonstrated the importance of data transformation and interaction effects in improving model performance

## Repository Structure

* nhanes-data-analyses/
* ├── nhanes_regression_analysis.R
* ├── README.md
* ├── data/
* └── outputs/

## How to Run
* 1. Open `nhanes_regression_analysis.R` in R or RStudio.
* 2. Run the script from the project folder.
* 3. Required NHANES data files will download into `/data`.
* 4. Figures and tables will save into `/outputs`.

## Author

Nathan Holtz, PhD

