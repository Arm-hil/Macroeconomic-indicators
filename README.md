# Current Indicator (more will be added later)
# GDP Prediction & Model Enhancement using Multiple Linear Regression

## Overview

This project aims to forecast GDP by leveraging multiple economic indicators obtained from FRED, along with additional market variables such as GPR, SP500, and VIX prices. We began with a multiple linear regression (MLR) model, thoroughly checking for linearity and correlation before refining our variable set. After establishing a baseline model with good R² and random residuals (but high RMSE), we ventured into predictive analytics to boost accuracy—experimenting with interaction models, lasso regression, and PCA. Ultimately, the interaction model emerged as the top performer in reducing RMSE.

## Motivation

Forecasting GDP is essential for policymakers, economists, and investors. With diverse data sources and a rigorous modeling approach, our objectives were to:

- **Integrate Multiple Data Sources:** Combine economic indicators from FRED with market sentiment variables (SP500 and VIX) and additional indicators like GPR.
- **Refine the Model:** Use statistical diagnostics (linearity checks, correlation analysis, and P-values) to hone the variable set.
- **Enhance Predictive Accuracy:** Address high RMSE by exploring advanced techniques such as interaction models, lasso regression, and PCA.
- **Validate Robustness:** Employ k-fold cross-validation to ensure model stability and generalizability.

## Data Sources

- **FRED:** Economic indicators (e.g., employment, inflation, industrial production, etc.).
- **GPR:** Supplementary economic performance indicators.
- **SP500 & VIX:** Market performance and volatility indices that potentially influence GDP trends.

## Methodology

1. **Data Preparation & EDA:**
   - Merged data from FRED, GPR, SP500, and VIX.
   - Conducted exploratory data analysis to assess linearity and variable correlation.
   - Removed variables based on correlation analysis and domain expertise.

2. **Baseline MLR Workflow:**
   - Developed a multiple linear regression model using a recipe-based workflow.
   - Assessed variable significance via P-values.
   - Applied k-fold cross-validation for robust performance evaluation.
   - Observed that, despite a high R² and random residuals, the RMSE was higher than desired.

3. **Predictive Analytics Enhancements:**
   - **Interaction Models:** Introduced interaction terms to capture synergistic effects between predictors.
   - **Lasso Regression:** Utilized regularization to improve model generalizability.
   - **PCA:** Reduced dimensionality to streamline the model.
   - **Outcome:** The interaction model achieved the best RMSE, offering a marked improvement over the baseline.
