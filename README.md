
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ms.GSI

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
#> Time difference of 11.47322 secs
```

Using the prepared input data, we run the model with four chains of 150
iterations. In reality, you should of course run it with more
iterations. We set the first 50 iterations in each chain as the warm-ups
(not kept in the final output). Here’s the summary for the estimates and
convergence diagnostics.

``` r

msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 150, nburn = 50, thin = 1, nchains = 4)
#> Running model (and the category is... Femme Queen Vogue!)
#> Time difference of 23.30967 secs
#> February-13-2023 11:29

msgsi_out$summ_comb
#> # A tibble: 12 × 8
#>    group                       mean  median      sd    ci.05   ci.95    GR n_eff
#>    <chr>                      <dbl>   <dbl>   <dbl>    <dbl>   <dbl> <dbl> <dbl>
#>  1 Russia                   3.66e-2 3.49e-2 0.0205  9.07e- 3 0.0745   1.02  94.4
#>  2 Coastal West Alaska      1.19e-1 6.95e-2 0.132   1.20e-13 0.369    3.81  45.5
#>  3 North Alaska Peninsula   1.04e-2 2.95e-4 0.0194  4.33e-14 0.0562   1.04  68.6
#>  4 Northwest Gulf of Alaska 2.74e-1 2.69e-1 0.0710  1.59e- 1 0.399    1.17  90.9
#>  5 Copper                   8.05e-4 3.00e-6 0.00252 1.15e-17 0.00459  1.02 318. 
#>  6 Northeast Gulf of Alaska 1.09e-3 2.41e-6 0.00370 2.16e-17 0.00590  1.04 181. 
#>  7 Coastal Southeast Alaska 2.29e-3 5.20e-6 0.00584 9.36e-17 0.0149   1.02 168. 
#>  8 British Columbia         6.66e-4 8.01e-7 0.00210 1.79e-21 0.00407  1.11 283. 
#>  9 WA/OR/CA                 4.54e-4 1.79e-7 0.00184 9.78e-20 0.00222  1.09 400  
#> 10 Lower Yukon              3.05e-1 3.50e-1 0.160   4.15e- 2 0.517    3.40  97.5
#> 11 Middle Yukon             7.13e-2 6.96e-2 0.0214  3.94e- 2 0.110    1.03 400  
#> 12 Upper Yukon              1.79e-1 1.79e-1 0.0322  1.28e- 1 0.234    1.01 339.
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
