# Select data based on the number of presences per category

Presences have `Count > 0`.

## Usage

``` r
select_factor_count_non_zero(
  observation,
  variable,
  threshold,
  relative = FALSE,
  dimension = 1
)
```

## Arguments

- observation:

  the `data.frame` with observations

- variable:

  the name of the `factor`

- threshold:

  the minimal threshold

- relative:

  When `FALSE` the threshold is the number of non-zero observations.
  When `TRUE` the threshold is the proportion of non-zero observations.
  Defaults to `FALSE`.

- dimension:

  Indicates which element of `variable` is used for the final
  aggregation.

## Examples

``` r
observation <- data.frame(
  Count = c(4, 4, 4, 4, 3, 3, 3, 0, 2, 2, 0, 0),
  LocationID = rep(1:3, each = 4),
  Year = rep(c(1, 1, 1, 1, 2, 2), 2)
)
# Select the locations with at least 3 prescenses
select_factor_count_non_zero(
  observation,
  variable = "LocationID",
  threshold = 3
)
#>   Count LocationID Year
#> 1     4          1    1
#> 2     4          1    1
#> 3     4          1    1
#> 4     4          1    1
#> 5     3          2    2
#> 6     3          2    2
#> 7     3          2    1
#> 8     0          2    1
# Select those locations in which the species is present in at least 2 years
select_factor_count_non_zero(
  observation, variable = c("LocationID", "Year"), threshold = 2
)
#>   Count LocationID Year
#> 5     3          2    2
#> 6     3          2    2
#> 7     3          2    1
#> 8     0          2    1
# Select those years in which the species is present in at least 2 locations
select_factor_count_non_zero(
  observation, variable = c("LocationID", "Year"),
  threshold = 2,
  dimension = 2
)
#>    Count LocationID Year
#> 1      4          1    1
#> 2      4          1    1
#> 3      4          1    1
#> 4      4          1    1
#> 7      3          2    1
#> 8      0          2    1
#> 9      2          3    1
#> 10     2          3    1
```
