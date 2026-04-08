# Convert a `sessionInfo()` to a data.frame of packages

Convert a [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) to
a data.frame of packages

## Usage

``` r
session_package(session)

# S4 method for class 'sessionInfo'
session_package(session)
```

## Arguments

- session:

  The output of
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)

## Value

a data.frame with the packages of a
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)
