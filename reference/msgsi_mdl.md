# Run multistage GSI model

Run multistage GSI model

## Usage

``` r
msgsi_mdl(
  dat_in,
  nreps,
  nburn,
  thin,
  nchains,
  nadapt = 0,
  keep_burn = FALSE,
  cond_gsi = TRUE,
  file_path = NULL,
  seed = NULL,
  iden_output = TRUE,
  p1_prior_weight = NULL,
  p2_prior_weight = NULL
)
```

## Arguments

- dat_in:

  Name of the input data.

- nreps:

  Total number of iterations (includes burn-ins).

- nburn:

  Number of warm-up runs.

- thin:

  Frequency to thin the output.

- nchains:

  Number of independent MCMC processes.

- nadapt:

  Number of adaptation run (default is 0). Only available when running
  model in fully Bayesian mode.

- keep_burn:

  To save the burn-ins or not (default is FALSE).

- cond_gsi:

  To run the model in conditional GSI mode (default is TRUE).

- file_path:

  File path to save the output. Leave it empty is you don't want to save
  the output.

- seed:

  Random seed for reproducibility. Default is NULL (no random seed).

- iden_output:

  Option to have trace history for individual assignments included in
  the final output. Default is TRUE.

- p1_prior_weight:

  An optional tibble to specify weight for each broad-scale reporting
  group. Columns are `repunit`, `grpvec`, and `weight`.

- p2_prior_weight:

  An optional tibble to specify weight for each regional reporting
  group. Columns are `repunit`, `grpvec`, and `weight`.

## Value

A list contains reporting group proportion summary and trace for tier 1
(summ_t1, trace_t1), tier 2 (summ_t2, trace_t2) and two tiers combined
(summ_comb, trace_comb), a tibble of combined collections (comb_groups),
records of stock-specific total catch (sstc_trace_t1, sstc_trace_t2),
records of individual assignment for each individual (idens_t1,
idens_t2), and model run specifications (specs).

## Examples

``` r
# setup input data
msgsi_dat <-
  prep_msgsi_data(mixture_data = mix,
  baseline1_data = base_templin, baseline2_data = base_yukon,
  pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5,
  harvest_mean = 500, harvest_cv = 0.05)
#> Compiling input data, may take a minute or two...
#> Time difference of 9.632432 secs

# run multistage model
msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 25, nburn = 15, thin = 1, nchains = 1)
#> Running model... and gradtitude turns what we have into Weather Girl!
#> Time difference of 1.423561 secs
#> April-02-2026 19:30
```
