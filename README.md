
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ms.GSI

<!-- badges: start -->
<!-- badges: end -->

Ms.GSI is here to help you conducting multistage genetic stock
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
samples for Chinook salmon bycatch from Bering Sea groundfish fisheries.
The mixture sample contains Chinook from all over North Pacific, but we
are interested in contribution from the Yukon River. We will conduct GSI
using a broad-scale baseline (`base_templin`) in combination with a
regional baseline (`base_yukon`) in a multistage framework.

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
#> Time difference of 9.934254 secs
```

Using the prepared input data, we run the model with four chains of 150
iterations. In reality, you should of course run it with more
iterations. We set the first 50 iterations in each chain as the warm-ups
(not kept in the final output). Here’s the summary for the estimates and
convergence diagnostics.

``` r
msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 150, nburn = 50, thin = 1, nchains = 4)
#> Running model (and the category is... Femme Queen Runway!)
#> Time difference of 4.101635 secs
#> June-28-2022 11:09

msgsi_out$summ_comb
#> # A tibble: 12 × 8
#>    group                       mean  median      sd    ci.05   ci.95    GR n_eff
#>    <chr>                      <dbl>   <dbl>   <dbl>    <dbl>   <dbl> <dbl> <dbl>
#>  1 Russia                   4.88e-2 4.54e-2 0.0208  1.89e- 2 0.0863   1.01 252. 
#>  2 Coastal West Alaska      2.13e-2 2.92e-4 0.0404  2.26e-16 0.122    1.69  52.2
#>  3 North Alaska Peninsula   2.66e-2 1.83e-2 0.0278  1.56e-13 0.0815   1.20  37.9
#>  4 Northwest Gulf of Alaska 3.19e-1 3.17e-1 0.0584  2.31e- 1 0.418    1.07  97.0
#>  5 Copper                   6.57e-4 1.05e-6 0.00223 6.82e-19 0.00373  1.05 312. 
#>  6 Northeast Gulf of Alaska 9.45e-4 1.07e-6 0.00303 9.45e-20 0.00578  1.16 274. 
#>  7 Coastal Southeast Alaska 1.02e-3 2.71e-6 0.00334 1.23e-18 0.00583  1.04 233. 
#>  8 British Columbia         8.42e-4 2.68e-6 0.00264 1.42e-17 0.00457  1.04 186. 
#>  9 WA/OR/CA                 5.27e-4 3.23e-7 0.00189 4.83e-19 0.00349  1.04 281. 
#> 10 Lower Yukon              3.13e-1 3.10e-1 0.0702  1.90e- 1 0.424    1.37 112. 
#> 11 Middle Yukon             7.94e-2 7.87e-2 0.0229  4.44e- 2 0.117    1.00 349. 
#> 12 Upper Yukon              1.88e-1 1.85e-1 0.0339  1.35e- 1 0.246    1.01 630.
```

There’s a function in the package to make trace plots and inspect mixing
of chains.

``` r
tr_plot(obj = msgsi_out$trace_comb)
```

<img src="man/figures/README-example_trace_plot-1.png" width="100%" />

We also have a detailed document on the model and instructions (work in
progress). Once you install the package, you can call the document using
`vignette("Ms.GSI")`.
