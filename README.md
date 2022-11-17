
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis620.2022

<!-- badges: start -->

[![R-CMD-check](https://github.com/nokkvidan/bis620.2022/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nokkvidan/bis620.2022/actions/workflows/R-CMD-check.yaml)
[![Test
coverage](https://github.com/nokkvidan/bis620.2022/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/nokkvidan/bis620.2022/actions/workflows/test-coverage.yaml)
[![Lint](https://github.com/nokkvidan/bis620.2022/actions/workflows/lint.yaml/badge.svg)](https://github.com/nokkvidan/bis620.2022/actions/workflows/lint.yaml)
<!-- badges: end -->

The goal of `bis620.2022` is to create an accelerometry graphic for a
dataset with time and three-dimensional coordinates. Additionally, it
has a feature for creating spectral signature plots. The dataset
`ukb_accel` from UKBiobank is part of the package and is available for
use in examples as seen here below in the example section.

## Installation

You can install the development version of bis620.2022 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nokkvidan/bis620.2022")
```

## Example

Here is an example to use the bis620.2022 package to visualize the data
at hand:

``` r
library(bis620.2022)

data(ukb_accel)
ukb_accel[1:100, ] |>
      spectral_signature(take_log = TRUE) |>
      accel_plot()
```

<img src="man/figures/README-example-1.png" width="100%" />
