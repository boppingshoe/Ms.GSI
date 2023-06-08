
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ms.GSI <a href="https://boppingshoe.github.io/Ms.GSI/"><img src="man/figures/logo.png" align="right" height="139" /></a>

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
#> Time difference of 11.39952 secs
```

Using the prepared input data, we run the model with four chains of 150
iterations. In reality, you should of course run it with more
iterations. We set the first 50 iterations in each chain as the warm-ups
(not kept in the final output). Here’s the summary for the estimates and
convergence diagnostics.

``` r

msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 150, nburn = 50, thin = 1, nchains = 4)
#> Running model (and the category is... Butch Queen Body!)
#> Time difference of 13.52093 secs
#> June-08-2023 15:50

msgsi_out$summ_comb
#> # A tibble: 12 × 8
#>    group                       mean  median      sd    ci.05   ci.95    GR n_eff
#>    <chr>                      <dbl>   <dbl>   <dbl>    <dbl>   <dbl> <dbl> <dbl>
#>  1 Russia                   2.54e-2 2.14e-2 0.0176  3.43e- 3 0.0602   1.01 121. 
#>  2 Coastal West Alaska      2.91e-1 3.01e-1 0.0949  1.04e- 1 0.429    1.29  50.1
#>  3 North Alaska Peninsula   2.51e-2 1.95e-2 0.0258  2.40e-14 0.0723   1.22  60.7
#>  4 Northwest Gulf of Alaska 3.57e-1 3.50e-1 0.0936  2.18e- 1 0.546    1.31  57.7
#>  5 Copper                   5.81e-4 1.72e-6 0.00169 6.22e-19 0.00348  1.05 309. 
#>  6 Northeast Gulf of Alaska 2.76e-3 3.06e-6 0.00946 5.57e-19 0.0168   1.16 139. 
#>  7 Coastal Southeast Alaska 1.31e-3 7.94e-7 0.00415 2.12e-19 0.00914  1.09 218. 
#>  8 British Columbia         7.41e-4 1.80e-6 0.00234 4.36e-17 0.00489  1.07 294. 
#>  9 WA/OR/CA                 6.46e-4 1.15e-6 0.00170 9.89e-19 0.00364  1.03 400  
#> 10 Lower Yukon              6.41e-2 5.44e-2 0.0375  1.87e- 2 0.136    1.13  94.1
#> 11 Middle Yukon             6.39e-2 6.01e-2 0.0221  3.19e- 2 0.103    1.00 574. 
#> 12 Upper Yukon              1.68e-1 1.66e-1 0.0321  1.16e- 1 0.223    1.02 335.
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
