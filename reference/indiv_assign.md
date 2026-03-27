# Individual assignment summary for Ms.GSI

Individual assignment summary for Ms.GSI

## Usage

``` r
indiv_assign(mdl_out, mdl_dat, show_t1_grps = TRUE)
```

## Arguments

- mdl_out:

  Model output object name.

- mdl_dat:

  Input data object name.

- show_t1_grps:

  Set it to be `FALSE` if you want the proportions of broad-scale groups
  to be combined. Default = `TRUE`.

## Value

Individual assignment summary

## Examples

``` r
# set up input data and run multistage model
msgsi_dat <-
  prep_msgsi_data(mixture_data = mix,
  baseline1_data = base_templin, baseline2_data = base_yukon,
  pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5)
#> Compiling input data, may take a minute or two...
#> Time difference of 9.924488 secs

# run model
msgsi_out <- msgsi_mdl(msgsi_dat, nreps = 25, nburn = 15, thin = 1, nchains = 1)
#> Running model... and good things come to Femme Queen Vogue!
#> Time difference of 1.428463 secs
#> March-27-2026 22:33

# individual assignment summary
ind_iden <- indiv_assign(msgsi_out, msgsi_dat)
```
