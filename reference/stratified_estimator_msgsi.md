# Stratified estimator for Ms.GSI

Combine the stock group estimates of multiple mixtures (i.e., strata)
weighted by harvest numbers or fishing efforts. Summary can be done by
extracting the stock-specific total catch/harvest output from the model
runs or by multiplying harvest (provided as input) by stock proportions.
Reporting groups can stay in the same format or be reorganized by
combining old reporting groups or reorganizing collections. See vignette
for details.

## Usage

``` r
stratified_estimator_msgsi(
  mdl_out = NULL,
  path = NULL,
  mixvec,
  new_pop_info = NULL,
  new_pop_by = "repunit",
  naive = FALSE,
  catchvec = NULL,
  cv = NULL
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

- naive:

  TRUE if you want the summary done by the old way (stock-specific
  harvets = harvest \* stock proportion), or you are using fishing
  effort instead of catch number.

- catchvec:

  If `naive = TRUE`, manually input harvest or fishing effort means with
  the same order as `mixvec`.

- cv:

  If `naive = TRUE`, manually input harvest or fishing effort cv's with
  the same order as `mixvec`.

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
