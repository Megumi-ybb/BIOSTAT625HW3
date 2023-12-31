# Linear Regression Package

[![R-CMD-check](https://github.com/Megumi-ybb/BIOSTAT625HW3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Megumi-ybb/BIOSTAT625HW3/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/Megumi-ybb/BIOSTAT625HW3/graph/badge.svg?token=H57VKF3AWP)](https://codecov.io/gh/Megumi-ybb/BIOSTAT625HW3)
## Installation

You can install the development version of ybLN from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Megumi-ybb/BIOSTAT625HW3")
```


## Introduction
This package comprises two primary functions: `yblm` and `ybanova`. The `yblm` function is meticulously engineered to conduct simple linear regression analysis, a pivotal statistical approach for deciphering the dynamics between an independent variable (explanatory) and a dependent variable (response). Simple linear regression fundamentally aims to identify the most fitting linear connection among observed data points. This is typically represented by the equation $\hat{y} = b_0 + b_1X$, where $\hat{y}$ symbolizes the predicted response.

Executing simple linear regression analysis in R entails several critical steps. Initially, data is imported into R, and it's paramount to verify that this data adheres to the prerequisites for linear regression, including linear association, independence, homoscedasticity (uniform error variance), and error normality. Following the preliminary data setup, linear regression analysis is usually executed using R's `lm()` function, which ascertains the correlation between variables.

R's `lm()` function is a conventional utility for linear regression, applied to model the relationship between a dependent variable and one or more independent variables. Specifically for simple linear regression, with a sole independent variable, `lm()` determines the optimal line or curve that minimizes the vertical discrepancies between the actual data points and the projected regression line, effectively minimizing the residuals or the differences between observed and model-predicted values.

The `yblm` function is another version of this methodology, offering enhanced flexibility and a more extensive array of outputs compared to the regular `lm()` function. It includes a feature to optionally integrate an intercept into the model (controlled by the `inter` parameter) and conducts checks to confirm the compatibility in dimensions of the independent variable `X` and the dependent variable `Y`. Post model fitting, `yblm` computes an array of statistical measures, including the model residuals, mean squared error (MSE), model and residual degrees of freedom, standard errors of coefficients, t-values, p-values, as well as R-squared and adjusted R-squared values. Additionally, it calculates the F-statistic and its corresponding p-value. Such an extensive output spectrum serves to facilitate a thorough analysis and diagnostic evaluation of the linear model.

The `ybanova` function is another version of `anova()` function in R. This function will generate an ANOVA table, which is similar to the R's regular function.

Further more, this package includes a sample dataset called `sampledata`, which includes `X = (X1,X2,X3), Y` where X_i,Y follows standard normal distribution and they are independent with each other. In this dataset, we have 100 X and Ys as a sample data for the function. Also, in the `testthat`, we used `iris` as a real world sample dataset to help justify the correctness of the functions in this package.



