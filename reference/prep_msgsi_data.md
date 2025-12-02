# Preparing multistage GSI input data

Preparing multistage GSI input data

## Usage

``` r
prep_msgsi_data(
  mixture_data,
  baseline1_data,
  baseline2_data,
  pop1_info,
  pop2_info,
  sub_group,
  file_path = NULL,
  loci1 = NULL,
  loci2 = NULL
)
```

## Arguments

- mixture_data:

  Individual fish with loci for both tier 1 and tier 2. Mixture data in
  GCL or *rubias* format.

- baseline1_data:

  Tier 1 baseline data in GCL or *rubias* format.

- baseline2_data:

  Tier 2 baseline data in GCL or *rubias* format.

- pop1_info:

  Population information for tier 1. A tibble with columns collection
  (collection names), repunit (reporting unit names), grpvec (group
  numbers), origin (wild/hatchery).

- pop2_info:

  Population information for tier 2. A tibble with columns collection
  (collection names), repunit (reporting unit names), grpvec (group
  numbers).

- sub_group:

  Group numbers for groups of interest. Group id numbers in tier 1 that
  identify groups in tier 2.

- file_path:

  Where you want to save a copy of input data. Leave it empty if you
  don't want to save a copy.

- loci1:

  Optional. Provide loci (for tier 1) as a fail-safe check.

- loci2:

  Optional. Provide loci (for tier 2) as a fail-safe check.

## Value

A list objects as the input data for msgsi_mdl()

## Examples

``` r
msgsi_dat <-
  prep_msgsi_data(mixture_data = mix,
  baseline1_data = base_templin, baseline2_data = base_yukon,
  pop1_info = templin_pops211, pop2_info = yukon_pops50, sub_group = 3:5)
#> Compiling input data, may take a minute or two...
#> Time difference of 9.135107 secs
```
