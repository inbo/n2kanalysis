# Calculate coefficients for a moving difference

Calculate coefficients for a moving difference

## Usage

``` r
moving_difference(n_year, duration, first_year = 1)
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
`difference_{window mid point start}_{window mid point end}_{window length}`.
`difference_2001.5_2010.5_4` is the difference of the average for period
for the years 2009 to 2012 compared to the period from 2000 to 2003.

## Examples

``` r
moving_difference(6, 2)
#>                      [,1] [,2] [,3] [,4] [,5] [,6]
#> difference_1.5_5.5_2 -0.5 -0.5  0.0  0.0  0.5  0.5
#> difference_2.5_5.5_2  0.0 -0.5 -0.5  0.0  0.5  0.5
#> difference_3.5_5.5_2  0.0  0.0 -0.5 -0.5  0.5  0.5
#> difference_1.5_4.5_2 -0.5 -0.5  0.0  0.5  0.5  0.0
#> difference_2.5_4.5_2  0.0 -0.5 -0.5  0.5  0.5  0.0
#> difference_1.5_3.5_2 -0.5 -0.5  0.5  0.5  0.0  0.0
moving_difference(6, 2, 2000)
#>                            [,1] [,2] [,3] [,4] [,5] [,6]
#> difference_2000.5_2004.5_2 -0.5 -0.5  0.0  0.0  0.5  0.5
#> difference_2001.5_2004.5_2  0.0 -0.5 -0.5  0.0  0.5  0.5
#> difference_2002.5_2004.5_2  0.0  0.0 -0.5 -0.5  0.5  0.5
#> difference_2000.5_2003.5_2 -0.5 -0.5  0.0  0.5  0.5  0.0
#> difference_2001.5_2003.5_2  0.0 -0.5 -0.5  0.5  0.5  0.0
#> difference_2000.5_2002.5_2 -0.5 -0.5  0.5  0.5  0.0  0.0
```
