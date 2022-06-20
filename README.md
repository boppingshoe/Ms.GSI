
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ms.GSI

<!-- badges: start -->
<!-- badges: end -->

Ms.GSI is a package for conducting multistage genetic stock
identification. This package includes functions to setup input data, run
the multistage model, and make summary statistics and convergence
diagnostics. It also includes a function for making trace plots.

## Installation

You can install the development version of Ms.GSI from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("boppingshoe/Ms.GSI")
```

## Example

This example shows a basic work flow for running a multistage model.
First thing first, the background: we made up a scenario where we have
samples for Chinook salmon by-catch from a Bering Sea fishery. The
mixture sample contains fish from all over the north Pacific, but we are
interested in the contribution from the Yukon River. We will conduct GSI
using a broad-scale baseline (Templin) in combination with a regional
baseline (Yukon) in a multistage framework.

The fake Chinook data sets are pre-loaded and included in the package.
Here we prepare the input data:

``` r
# library(Ms.GSI)
devtools::load_all()
#> ℹ Loading Ms.GSI

msgsi_dat <-
  prep_msgsi_data(mixture_data = mix,
  baseline1_data = base_templin, baseline2_data = base_yukon,
  pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5)
#> Compiling input data, may take a minute or two...
#> Time difference of 6.435127 secs
```

Using the prepared input data, we run the model with 150 iterations. In
reality, you should of course run it with more iterations. We set the
first 50 iterations as the warm-ups (not kept in the final output).
Here’s the summary for the estimates and convergence diagnostics.

``` r
msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 150, nburn = 50, thin = 1, nchains = 4)
#> Running model (and the category is... Realness!)
#> Time difference of 2.617285 secs
#> June-20-2022 15:26

msgsi_out$summ_comb
#> # A tibble: 12 × 8
#>    group                       mean  median      sd    ci.05   ci.95    GR n_eff
#>    <chr>                      <dbl>   <dbl>   <dbl>    <dbl>   <dbl> <dbl> <dbl>
#>  1 Russia                   5.14e-2 4.98e-2 0.0212  2.16e- 2 0.0890   1.01 269. 
#>  2 Coastal West Alaska      7.86e-2 6.92e-3 0.102   7.62e-15 0.288    2.46 136. 
#>  3 North Alaska Peninsula   1.99e-2 1.47e-2 0.0206  6.04e-11 0.0572   1.02  66.0
#>  4 Northwest Gulf of Alaska 3.38e-1 3.37e-1 0.0672  2.31e- 1 0.453    1.19 101. 
#>  5 Copper                   5.49e-4 1.13e-6 0.00200 8.28e-19 0.00303  1.07 400  
#>  6 Northeast Gulf of Alaska 6.34e-4 8.35e-7 0.00228 4.75e-19 0.00336  1.09 263. 
#>  7 Coastal Southeast Alaska 1.43e-3 2.96e-6 0.00409 1.58e-18 0.00898  1.09 135. 
#>  8 British Columbia         9.14e-4 4.80e-7 0.00342 3.09e-19 0.00521  1.10 267. 
#>  9 WA/OR/CA                 3.49e-4 1.95e-6 0.00101 4.78e-23 0.00194  1.05 362. 
#> 10 Lower Yukon              2.47e-1 2.55e-1 0.0851  1.00e- 1 0.373    1.67 104. 
#> 11 Middle Yukon             7.63e-2 7.53e-2 0.0225  4.36e- 2 0.114    1.01 400  
#> 12 Upper Yukon              1.85e-1 1.86e-1 0.0321  1.32e- 1 0.238    1.01 621.
```

There’s a function to make trace plots and inspect mixing of chains.

``` r
tr_plot(obj = msgsi_out$trace_comb)
```

<img src="man/figures/README-example_trace_plot-1.png" width="100%" />

We also have a detailed document on the model and instructions (work in
progress). Once you install the package, you can call the document using
`vignette("Ms.GSI")`.
