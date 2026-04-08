# Aggregate all results meta data in a single dataframe

Aggregate all results meta data in a single dataframe

## Usage

``` r
result_metadata(x, ...)

# S4 method for class 'character'
result_metadata(x, ..., base, project)

# S4 method for class 'n2kResult'
result_metadata(x, ...)
```

## Arguments

- x:

  object with the current results

- ...:

  further arguments (see Details)

- base:

  the base location to read the results

- project:

  will be a relative path within the base location
