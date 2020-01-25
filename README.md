
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bfrr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/debruine/bfrr.svg?branch=master)](https://travis-ci.org/debruine/bfrr)
[![Codecov test
coverage](https://codecov.io/gh/debruine/bfrr/branch/master/graph/badge.svg)](https://codecov.io/gh/debruine/bfrr?branch=master)
<!-- badges: end -->

Calculate Bayes Factors and robustness regions from summary statistics.

## Installation

You can install the development version from
[GitHub](https://github.com/debruine/bfrr) with:

``` r
# install.packages("devtools")
devtools::install_github("debruine/bfrr")
```

## Example

``` r
library(bfrr)
```

First, we’ll simulate 50 data points from a normal distribution with a
mean of 0.25 and SD of 1 and conduct a one-sample t-test.

``` r
simdat <- rnorm(50, 0.25, 1)
t.test(simdat)
#> 
#>  One Sample t-test
#> 
#> data:  simdat
#> t = 2.9009, df = 49, p-value = 0.005558
#> alternative hypothesis: true mean is not equal to 0
#> 95 percent confidence interval:
#>  0.1083681 0.5970156
#> sample estimates:
#> mean of x 
#> 0.3526918
```

Set up the test using `bfrr()`.

``` r
rr <- bfrr(
  sample_mean = mean(simdat), # mean of the sample
  sample_se = sd(simdat) / sqrt(length(simdat)), # SE of the sample
  sample_df = length(simdat) - 1, # degrees of freedom
  model = "normal",
  mean = 0, # mean of the H1 distribution
  sd = 0.25, # SD of the H1 distribution
  tail = 1, # is the test 1-tailed or 2-tailed
  criterion = 6, # BF against which to test for support for H1/H0
  rr_interval = c(0, 1), # theory_sd over which to explore robustness range
  precision = 2 # number of decimal places to calculate RR
)
```

Use `summary(rr)` to output a summary paragraph.

``` r
summary(rr)
```

The likelihood of your data under the theoretical distribution HN(0,
0.25) is 0.16. The likelihood of your data under the null distribution
T(49) is 0.01. The Bayes Factor is 20.6; this test finds evidence for H1
with a criterion of 6. The region of theoretical SDs that give the same
conclusion is RR = \[0.10, 1.00\].

Use `plot(rr)` to view a plot of your data.

``` r
plot(rr)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />
