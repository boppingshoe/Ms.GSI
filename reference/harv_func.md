# Sample total harvest numbers from a lognormal distribution

Sample total harvest numbers from a lognormal distribution

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
