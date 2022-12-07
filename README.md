
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis620.2022

<!-- badges: start -->

[![R-CMD-check](https://github.com/nokkvidan/bis620.2022/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nokkvidan/bis620.2022/actions/workflows/R-CMD-check.yaml)
[![Test
coverage](https://github.com/nokkvidan/bis620.2022/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/nokkvidan/bis620.2022/actions/workflows/test-coverage.yaml)
[![Lint](https://github.com/nokkvidan/bis620.2022/actions/workflows/lint.yaml/badge.svg)](https://github.com/nokkvidan/bis620.2022/actions/workflows/lint.yaml)
[![Codecov](https://codecov.io/gh/nokkvidan/bis620.2022/branch/main/graph/badge.svg?token=U4USARWKRM)](https://codecov.io/gh/nokkvidan/bis620.2022)
<!-- badges: end -->

The diabetes risk prediction R project aims to help healthcare
professionals determine if a patient has or will have diabetes based on
various health indicators such as age and BMI to name a few. This is
accomplished through machine learning algorithms that analyze a large
dataset of patients with known diabetes status and health indicators.
They use that information to create a model that can accurately predict
the likelihood of a patient having diabetes. The machine learning
algorithms that are available to model, optimize and evaluate
performance are generalized linear model, XGBoost and random forests.

By using this project, healthcare professionals can quickly and
accurately determine the likelihood of a patient’s diabetes, allowing
for earlier detection and treatment. This can ultimately improve patient
outcomes and reduce the overall burden of diabetes on the healthcare
system.

Additionally, this project can help researchers study the significant
correlation between specific health indicators and the likelihood of
developing diabetes. By using the functions provided in this project,
researchers can quickly analyze the relationship between different
variables and diabetes, potentially uncovering new insights into the
causes and risk factors of the condition.

The dataset `diabetes` from Kaggle is part of the package and is
available for use in examples as seen here below in the example section.

### Badge overview

[Coverage page](https://app.codecov.io/gh/nokkvidan/bis620.2022)

[Lint
results](https://github.com/nokkvidan/bis620.2022/actions/workflows/lint.yaml)

## Installation

You can install the development version of bis620.2022 from
[GitHub](https://github.com/nokkvidan/bis620.2022) with:

``` r
# install.packages("devtools")
devtools::install_github("nokkvidan/bis620.2022")
```

## Usage

To use this package, you must first load it using the following command:

``` r
library(bis620.2022)
```

Once the package is loaded, you can use the function `data(diabetes)` to
get the diabetes data provided with the package.

The package includes the following functions:

-   `vis_2vars()`: Visualize any two variables in the dataset.
-   `vis_dist()`: Visualize the distributions of the non categorical
    variables in the data set
-   `vis_num()`: Visualize the non categorical variables in the dataset
    with boxplots. Shows both scaled and non scaled variables.
-   `glm_model()`: Fits a GLM to the input data and returns the model
    object.
-   `boost_model()`: Fits an XGBoost model to the input data and returns
    the model object.
-   `rf_model()`: Fits a random forest model to the input data and
    returns the model object.

## EDA

The diabetes risk prediction R project also includes a suite of
visualization functions that allow for the exploratory analysis of the
dataset used to train the prediction model. These functions provide an
interactive and intuitive way to explore the relationship between
different variables and the likelihood of a patient having diabetes.

``` r
# vis_2vars()
# vis_dist()
# vis_num()
```

Overall, these visualization functions provide a powerful tool for the
exploration and analysis of the dataset, and can help researchers gain a
better understanding of the factors that influence the likelihood of a
patient having diabetes.

## Modeling and Evaluation

The R package allows health care professionals to easily model and
evaluate generalized linear models (GLM), XGBoost, and random forests
for identifying significant variables correlated with diabetes. With
these functions, health care professionals can quickly fit these models
to their data and assess the performance of each model to determine the
best approach for their specific use case. In addition, the package
offers a range of evaluation metrics and visualization tools to help
users interpret the results of the model and make informed decisions
about the factors associated with diabetes. Overall, the functions
provide a convenient and user-friendly solution for health care
professionals looking to use machine learning methods to analyze
diabetes data.

``` r
# # Fit a GLM to the diabetes data
# glm_model() 
# # Fit a random forest model to the diabetes data
# rf_model()
# # Fit an XGBoost model to the diabetes data
# boost_model()

# # Fit and evaluate the performance of the GLM model
# glm_model() 
# # Fit and evaluate the performance of the XGBoost model
# boost_model()
# # Fit and evaluate the performance of the random forest model
# rf_model()
```

# Contributing

We welcome contributions to the `bis620.2022` package. If you would like
to contribute, please follow these steps:

1.  Fork the repository and clone it to your local machine.
2.  Create a new branch for your changes.
3.  Make your changes and push them to your fork.
4.  Open a pull request and describe your changes.

# License

The `bis620.2022` package is licensed under the MIT License.
