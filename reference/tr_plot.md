# Plot MCMC trace

Plot MCMC trace

## Usage

``` r
tr_plot(mdl_out, trace_obj, pop_info = NULL)
```

## Arguments

- mdl_out:

  Model output object name.

- trace_obj:

  Trace from the model output.

- pop_info:

  Population information. A tibble with columns collection (collection
  names), repunit (reporting unit names), and grpvec (group numbers).

## Value

Trace plot in ggplot

## Examples

``` r
# set up input data and run multistage model
msgsi_dat <-
  prep_msgsi_data(mixture_data = mix,
  baseline1_data = base_templin, baseline2_data = base_yukon,
  pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5)
#> Compiling input data, may take a minute or two...
#> Time difference of 9.582769 secs

msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 25, nburn = 15, thin = 1, nchains = 1)
#> Running model... and gradtitude turns what we have into Weather Girl!
#> Time difference of 1.464024 secs
#> March-26-2026 21:27

# trace plot
tr_plot(mdl_out = msgsi_out, trace_obj = "trace_comb", pop_info = msgsi_out$comb_groups)
```
