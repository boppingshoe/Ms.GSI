
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ms.GSI <a href="https://boppingshoe.github.io/Ms.GSI/"><img src="man/figures/logo.png" align="right" height="139" alt="Ms.GSI website" /></a>

<!-- badges: start -->
<!-- badges: end -->

*Ms.GSI* is here to help you conducting multistage genetic stock
identification. This package includes functions to setup input data, run
the multistage model, and make summary statistics and convergence
diagnostics. It also includes a function for making trace plots.

## Installation

You can install the development version of *Ms.GSI* from
[GitHub](https://github.com/boppingshoe/Ms.GSI) with:

``` r
# install.packages("devtools")
devtools::install_github("boppingshoe/Ms.GSI", build_vignettes = TRUE)
```

## Example

This example shows the basic workflows for running a multistage model.
First thing first, the background: we made up a scenario where we have
samples for Chinook salmon bycatch from Bering Sea groundfish fisheries.
The mixture sample contains Chinook from all over the North Pacific, but
we are interested in contribution from the Yukon River. We will conduct
GSI using a broad-scale baseline (`base_templin`) in combination with a
regional baseline (`base_yukon`) in a multistage framework.

The fake Chinook data sets are pre-loaded in the *Ms.GSI* package. Here
we prepare the input data:

``` r

library(Ms.GSI)

msgsi_dat <-
  prep_msgsi_data(mixture_data = mix,
  baseline1_data = base_templin, baseline2_data = base_yukon,
  pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5)
#> Compiling input data, may take a minute or two...
#> Time difference of 9.442187 secs
```

Using the prepared input data, we run the model with four chains of 150
iterations. In reality, you should of course run it with more
iterations. We set the first 50 iterations in each chain as the warm-ups
(not kept in the final output). Here’s the summary for the estimates and
convergence diagnostics.

``` r

msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 150, nburn = 50, thin = 1, nchains = 4)
#> Running model (and the category is... Snow Ball!)
#> Time difference of 23.62524 secs
#> June-09-2023 13:14

msgsi_out$summ_comb
#> # A tibble: 12 × 8
#>    group                       mean  median      sd    ci.05   ci.95    GR n_eff
#>    <chr>                      <dbl>   <dbl>   <dbl>    <dbl>   <dbl> <dbl> <dbl>
#>  1 Russia                   2.90e-2 2.63e-2 0.0212  7.31e- 5 0.0702   1.13  92.8
#>  2 Coastal West Alaska      1.34e-1 1.05e-1 0.135   1.23e-12 0.352    3.31  43.1
#>  3 North Alaska Peninsula   1.95e-2 5.95e-3 0.0256  2.27e-13 0.0712   1.49  44.6
#>  4 Northwest Gulf of Alaska 3.56e-1 3.54e-1 0.0792  2.12e- 1 0.480    1.35  89.6
#>  5 Copper                   1.15e-3 2.53e-6 0.00330 6.33e-19 0.00686  1.05 272. 
#>  6 Northeast Gulf of Alaska 8.12e-4 1.31e-6 0.00352 1.41e-20 0.00308  1.14 213. 
#>  7 Coastal Southeast Alaska 2.17e-3 1.71e-5 0.00519 2.33e-15 0.0122   1.08 162. 
#>  8 British Columbia         7.01e-4 1.46e-6 0.00203 6.21e-19 0.00481  1.01 355. 
#>  9 WA/OR/CA                 6.22e-4 8.87e-7 0.00211 7.36e-18 0.00310  1.03 349. 
#> 10 Lower Yukon              2.07e-1 2.17e-1 0.148   2.14e- 2 0.423    3.67 176. 
#> 11 Middle Yukon             7.01e-2 6.68e-2 0.0224  3.93e- 2 0.111    1.09 436. 
#> 12 Upper Yukon              1.79e-1 1.78e-1 0.0338  1.19e- 1 0.235    1.04 384.
```

There’s a function in the package to make trace plots and inspect mixing
of chains.

``` r

tr_plot(obj = msgsi_out$trace_comb)
```

<img src="man/figures/README-example_trace_plot-1.png" width="100%" />

Details of the mathematical model of integrated multistage framework and
instructions for using *Ms.GSI* package can be found in the “articles”
tab of the package website. Or, once you installed *Ms.GSI*, you can
call the article using `vignette("msgsi_vignette")`.
