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
  p2_prior_weight = NULL,
  harvest = NULL
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

- harvest:

  An optional harvest number for calculating the probability of p = 0. A
  proportion is considered as 0 if it's less than 5e-7 by default. If
  harvest number is provided, p = 0 is calculated as 0.5 / harvest of
  that stock.

## Value

A list contains reporting group proportion summary and trace for tier 1
(summ_t1, trace_t1), tier 2 (summ_t2, trace_t2) and two tiers combined
(summ_comb, trace_comb), and record of individual assignment during
first tier for each individual (idens).

## Examples

``` r
# setup input data
msgsi_dat <-
  prep_msgsi_data(mixture_data = mix,
  baseline1_data = base_templin, baseline2_data = base_yukon,
  pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5)
#> Compiling input data, may take a minute or two...
#> Time difference of 9.005001 secs

# run multistage model
msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 25, nburn = 15, thin = 1, nchains = 1)
#> Running model (and the category is... Femme Queen Runway!)
#> Time difference of 1.43145 secs
#> December-02-2025 01:41
```
