# Stratified estimator for Ms.GSI

Stratified estimator for Ms.GSI

## Usage

``` r
stratified_estimator_msgsi(
  mdl_out = NULL,
  path = NULL,
  mixvec,
  new_pop_info = NULL,
  new_pop_by = "repunit"
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

- new_pop_info:

  Population information for the new grouping. A tibble with columns
  `repunit` and `new_repunit`. `repunit` is the names of the original
  reporting groups. Can include a column for `collection` if
  reorganizing using collections.

- new_pop_by:

  Option to reorganize the reporting groups by "repunit" or
  "collection". Default is "repunit".

## Value

A tibble of proportions and harvest numbers by reporting group for
combined mixtures/strata.

## Examples

``` r
if (FALSE) { # \dontrun{
new_groups <- mdl_out$summ_comb %>% dplyr::select(group) %>%
 dplyr::mutate(new_repunit = c(rep("broad", 9), rep("regional", 3))) %>%
 dplyr::rename(repunit = group)

stratified_estimator_msgsi(path = "test", mixvec = c("m1", "m2", "m3"), new_pop_info = new_groups)
} # }
```
