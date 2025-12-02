# Stratified estimator for Ms.GSI

Stratified estimator for Ms.GSI

## Usage

``` r
stratified_estimator_msgsi(
  mdl_out = NULL,
  path = NULL,
  mixvec,
  catchvec,
  cv = NULL,
  new_pop_info = NULL
)
```

## Arguments

- mdl_out:

  Optional. Ms.GSI output object name for combining group proportions
  and harvest of a single mixture.

- path:

  Where to find output from each mixture as a folder.

- mixvec:

  Character vector of mixture sillies that are used to locate the
  folders where output .csv files lives, if `mdl_out` is not provided.

- catchvec:

  Numeric vector of harvest for each mixture/stratum, must be in the
  same order as `mixvec`.

- cv:

  Numeric vector of harvest estimate coefficients of variation for each
  stratum, must be the same order as `mixvec`.

- new_pop_info:

  Population information for the new grouping. A tibble with columns
  `repunit` and `new_repunit`. `repunit` is the names of the original
  reporting groups.

## Value

A tibble of proportions and harvest numbers by reporting group for
combined mixtures/strata.

## Examples

``` r
if (FALSE) { # \dontrun{
new_groups <- mdl_out$summ_comb %>% dplyr::select(group) %>%
 dplyr::mutate(new_repunit = c(rep("broad", 9), rep("regional", 3))) %>%
 dplyr::rename(repunit = group)

stratified_estimator_msgsi(path = "test", mixvec = c("m1", "m2", "m3"), catchvec = c(4500, 5000, 3000), cv = c(0.5, 0.1 ,0.3), new_pop_info = new_groups)
} # }
```
