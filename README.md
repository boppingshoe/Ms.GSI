
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
#> Working on it, may take a minute or two...
#> Time difference of 6.340437 secs
```

Using the prepared input data, we run the model with 150 iterations. In
reality, you should of course run it with more iterations. We set the
first 50 iterations as the warm-ups (not kept in the final output).
Here’s the summary for the estimates and convergence diagnostics.

``` r
msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 150, nburn = 50, thin = 1, nchains = 4)
#> Running model (and the category is... Working Girl!)
#> Time difference of 2.57392 secs
#> 2022-06-17 15:14:48

msgsi_out$summ_comb
#> # A tibble: 12 × 8
#>    group                       mean  median      sd    ci.05   ci.95    GR n_eff
#>    <chr>                      <dbl>   <dbl>   <dbl>    <dbl>   <dbl> <dbl> <dbl>
#>  1 Russia                   4.67e-2 4.51e-2 0.0205  1.51e- 2 0.0828   1.03 223. 
#>  2 Coastal West Alaska      1.75e-1 1.92e-1 0.107   2.20e- 7 0.325    2.04  57.0
#>  3 North Alaska Peninsula   3.34e-2 3.04e-2 0.0273  3.14e- 8 0.0850   1.42  99.2
#>  4 Northwest Gulf of Alaska 3.34e-1 3.33e-1 0.0682  2.19e- 1 0.453    1.44 118. 
#>  5 Copper                   6.28e-4 9.97e-7 0.00182 5.78e-18 0.00368  1.00 400  
#>  6 Northeast Gulf of Alaska 9.16e-4 1.48e-6 0.00302 6.11e-17 0.00446  1.03 304. 
#>  7 Coastal Southeast Alaska 1.22e-3 1.87e-6 0.00355 2.02e-19 0.00720  1.06 262. 
#>  8 British Columbia         9.82e-4 4.43e-7 0.00371 1.09e-18 0.00572  1.28 223. 
#>  9 WA/OR/CA                 4.43e-4 6.59e-7 0.00176 2.46e-17 0.00207  1.06 400  
#> 10 Lower Yukon              1.51e-1 1.14e-1 0.106   3.08e- 2 0.350    1.83  39.1
#> 11 Middle Yukon             7.08e-2 6.87e-2 0.0231  3.74e- 2 0.110    1.02 322. 
#> 12 Upper Yukon              1.84e-1 1.83e-1 0.0314  1.37e- 1 0.237    1.03 442.
```

There’s a function to make trace plots and inspect mixing of chains.

``` r
tr_plot(obj = msgsi_out$trace_comb)
```

<img src="man/figures/README-example trace plot-1.png" width="100%" />

We also have a detailed document on the model and instructions (work in
progress). Once you install the package, you can call the document using
`vignette("Ms.GSI")`.
