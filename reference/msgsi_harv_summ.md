# Summarize trace output for stock-specific harvest

Summarize trace output for stock-specific harvest

## Usage

``` r
msgsi_harv_summ(mdl_out, mdl_dat)
```

## Arguments

- mdl_out:

  Output of GSI model run

- mdl_dat:

  Input data for GSI model

## Value

A tibble of harvest estimates for each reporting group as columns and
MCMC iterations as rows.

## Examples

``` r
# prep input data
msgsi_dat <- prep_msgsi_data(mixture_data = mix, baseline1_data = base_templin, baseline2_data = base_yukon, pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5, harvest_mean = 500, harvest_cv = 0.05)
#> Compiling input data, may take a minute or two...
#> Time difference of 9.88166 secs

# run multistage model
msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 25, nburn = 15, thin = 1, nchains = 1)
#> Running model... and surround yourself with only Stone Cold Face!
#> Time difference of 1.505454 secs
#> April-02-2026 19:29

# summarize individual assignments
harv_summ <- msgsi_harv_summ(msgsi_out, msgsi_dat)
```
