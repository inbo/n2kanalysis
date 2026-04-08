# Calculate moving trend coefficients

Calculate moving trend coefficients

## Usage

``` r
moving_average(n_year, duration, first_year = 0)
```

## Arguments

- n_year:

  Number of available years in the data.

- duration:

  Number of years in the moving window. If the number of available years
  is less than this value, the trend will be calculated for the
  available years.

- first_year:

  First year of the data. Only used to name the rows.

## Value

A matrix with moving average coefficients One row for each window and
and one column for each year in the data. The format of the row names is
`average_{window mid point}_{window length}`. `average_2001.5_4` is the
average for the years 2000 to 2003.

## Examples

``` r
moving_average(5, 3)
#>                    [,1]      [,2]      [,3]      [,4]      [,5]
#> average_1.0_3 0.3333333 0.3333333 0.3333333 0.0000000 0.0000000
#> average_2.0_3 0.0000000 0.3333333 0.3333333 0.3333333 0.0000000
#> average_3.0_3 0.0000000 0.0000000 0.3333333 0.3333333 0.3333333
moving_average(5, 3, 2000)
#>                       [,1]      [,2]      [,3]      [,4]      [,5]
#> average_2001.0_3 0.3333333 0.3333333 0.3333333 0.0000000 0.0000000
#> average_2002.0_3 0.0000000 0.3333333 0.3333333 0.3333333 0.0000000
#> average_2003.0_3 0.0000000 0.0000000 0.3333333 0.3333333 0.3333333
```
