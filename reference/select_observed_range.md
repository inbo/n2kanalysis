# Limit the observation to the range in which the species is present

Limit the observation to the range in which the species is present

## Usage

``` r
select_observed_range(observation, variable)
```

## Arguments

- observation:

  the `data.frame` with observations

- variable:

  the name of the `factor`

## Examples

``` r
observation <- data.frame(
  Count = c(0, 0, 100, 101, 0, 51, 1, 0, 0, 0),
  Year = 1:10
)
select_observed_range(observation, "Year")
#>   Count Year
#> 3   100    3
#> 4   101    4
#> 5     0    5
#> 6    51    6
#> 7     1    7
```
