# Calculate moving trend coefficients

Calculate moving trend coefficients

## Usage

``` r
moving_trend(n_year, duration, first_year = 0)
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

A matrix with the moving trend coefficients. One row for each window and
and one column for each year in the data. The format of the row names is
`trend_{window mid point}_{window length}`. `trend_2001.5_4` is the
trend for the years 2000 to 2003.

## Examples

``` r
moving_trend(5, 3)
#>             [,1] [,2] [,3] [,4] [,5]
#> trend_1.0_3 -0.5  0.0  0.5  0.0  0.0
#> trend_2.0_3  0.0 -0.5  0.0  0.5  0.0
#> trend_3.0_3  0.0  0.0 -0.5  0.0  0.5
moving_trend(5, 4, 2000)
#>                [,1] [,2] [,3] [,4] [,5]
#> trend_2001.5_4 -0.3 -0.1  0.1  0.3  0.0
#> trend_2002.5_4  0.0 -0.3 -0.1  0.1  0.3
```
