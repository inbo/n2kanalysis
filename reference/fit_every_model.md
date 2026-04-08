# Fit the model to the analysis files

Fit the model to the analysis files

## Usage

``` r
fit_every_model(path, status, verbose = TRUE, n_cluster = 1, ...)
```

## Arguments

- path:

  The path containing the analysis files.

- status:

  A vector with status levels to (re-)fit the model. Defaults to
  `c("new", "waiting")` when missing.

- verbose:

  Show the name of the current analysis file on screen. Defaults to
  `TRUE`

- n_cluster:

  The number of clusters to use.

- ...:

  Arguments passed to
  [`fit_model()`](https://inbo.github.io/n2kanalysis/reference/fit_model.md)
