
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
#> Time difference of 11.19722 secs
```

Using the prepared input data, we run the model with four chains of 150
iterations. In reality, you should of course run it with more
iterations. We set the first 50 iterations in each chain as the warm-ups
(not kept in the final output). Here’s the summary for the estimates and
convergence diagnostics.

``` r
msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 150, nburn = 50, thin = 1, nchains = 4)
#> Running model (and the category is... Best Mother!)
#> Time difference of 4.707205 secs
#> July-06-2022 11:18

msgsi_out$summ_comb
#> # A tibble: 12 × 8
#>    group                       mean  median      sd    ci.05   ci.95    GR n_eff
#>    <chr>                      <dbl>   <dbl>   <dbl>    <dbl>   <dbl> <dbl> <dbl>
#>  1 Russia                   4.68e-2 4.27e-2 0.0206  1.98e- 2 0.0865   1.01 242. 
#>  2 Coastal West Alaska      1.22e-1 1.03e-1 0.125   3.51e-13 0.334    3.47  66.5
#>  3 North Alaska Peninsula   4.34e-2 4.04e-2 0.0275  3.38e- 3 0.0935   1.08 120. 
#>  4 Northwest Gulf of Alaska 3.39e-1 3.39e-1 0.0771  2.19e- 1 0.468    1.61  85.9
#>  5 Copper                   6.25e-4 1.14e-6 0.00191 1.08e-18 0.00419  1.08 362. 
#>  6 Northeast Gulf of Alaska 7.74e-4 6.46e-7 0.00250 1.96e-17 0.00587  1.02 335. 
#>  7 Coastal Southeast Alaska 9.97e-4 1.42e-6 0.00366 1.49e-17 0.00523  1.10 259. 
#>  8 British Columbia         9.77e-4 2.74e-6 0.00347 2.69e-19 0.00534  1.05 252. 
#>  9 WA/OR/CA                 6.36e-4 2.20e-6 0.00191 6.32e-16 0.00373  1.04 301. 
#> 10 Lower Yukon              1.89e-1 1.96e-1 0.110   2.26e- 2 0.363    2.49  89.7
#> 11 Middle Yukon             7.11e-2 6.94e-2 0.0229  3.72e- 2 0.111    1.02 374. 
#> 12 Upper Yukon              1.85e-1 1.84e-1 0.0333  1.34e- 1 0.242    1.01 523.
```

There’s a function in the package to make trace plots and inspect mixing
of chains.

``` r
tr_plot(obj = msgsi_out$trace_comb)
```

<img src="man/figures/README-example_trace_plot-1.png" width="100%" />

We also documented in details on the mathematical model of multistage
framework and instructions for using *Ms.GSI* package. Once you
installed *Ms.GSI*, you can call the document using
`vignette("msgsi_vignette")`.
