# Plot MCMC trace

Plot MCMC trace

## Usage

``` r
tr_plot(obj, nburn = 0, thin = 1, pop_info = NULL)
```

## Arguments

- obj:

  Trace from the model output.

- nburn:

  Number of burn-in you set up when you ran the model. Default is 0 if
  you didn't save the burn-ins (keep_burn = FALSE).

- thin:

  Number of thinning you set up when you ran the model. Default is 1 (no
  thinning).

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
#> Time difference of 9.22858 secs

msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 25, nburn = 15, thin = 1, nchains = 1)
#> Running model... and gradtitude turns what we have into Stone Cold Face!
#> Time difference of 1.341786 secs
#> January-30-2026 23:03

# trace plot
tr_plot(obj = msgsi_out$trace_comb, pop_info = msgsi_out$comb_groups)
```
