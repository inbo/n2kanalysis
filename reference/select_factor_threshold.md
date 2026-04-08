# Select the observations based on the average of a factor

The negative binomial average of the `Count` variable is calculated for
each level of `variable`. Only the levels which are equal or larger than
`threshold` times the maximal average (in the original scale) are
retained.

## Usage

``` r
select_factor_threshold(observation, variable, threshold)
```

## Arguments

- observation:

  the `data.frame` with observations

- variable:

  the name of the `factor`

- threshold:

  the minimal threshold

## Examples

``` r
observation <- data.frame(
  Count = c(100, 101, 50, 51, 1, 0, 0, 0),
  LocationID = factor(rep(1:4, each = 2))
)
select_factor_threshold(observation, "LocationID", threshold = 0.05)
#> Warning: iteration limit reached
#> Warning: iteration limit reached
#>   Count LocationID
#> 1   100          1
#> 2   101          1
#> 3    50          2
#> 4    51          2
```
