# Sample total harvest numbers from a lognormal distribution

Generate harvest number using a lognormal distribution. This function is
used in
[`prep_msgsi_data()`](https://boppingshoe.github.io/Ms.GSI/reference/prep_msgsi_data.md)
to generate a vector of harvest numbers.

## Usage

``` r
harv_func(x, n = 5000, seed = NULL)
```

## Arguments

- x:

  A vector consists of harvest mean and CV.

- n:

  Number of sample draws.

- seed:

  Optional random seed.

## Value

A vector of harvest numbers drawn from a lognormal distribution with
specified mean and CV.

## Examples

``` r
tot_harv <- harv_func(c(500, 0.05))
```
