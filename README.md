
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bis620.2022

<!-- badges: start -->

[![R-CMD-check](https://github.com/nokkvidan/bis620.2022/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nokkvidan/bis620.2022/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of bis620.2022 is to

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
