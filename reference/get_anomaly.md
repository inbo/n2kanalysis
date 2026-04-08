# Get the anomalies from a model

Get the anomalies from a model

## Usage

``` r
get_anomaly(analysis, ...)

# S4 method for class 'n2kInla'
get_anomaly(
  analysis,
  n = 20,
  expected_ratio = 5,
  expected_absent = 5,
  random_threshold = 1.05,
  verbose = TRUE,
  ...
)

# S4 method for class 'n2kModel'
get_anomaly(analysis, verbose = TRUE, ...)
```

## Arguments

- analysis:

  The model to add

- ...:

  Extra options. See details.

- n:

  the number of anomalies per category.

- expected_ratio:

  Observations that have `observed / fitted > expected_ratio` or
  `fitted / observed > expected_ratio` are potential anomalies. Defaults
  to `5`, which implies that observed values that are 5 times higher or
  lower than the fitted values are potential anomalies.

- expected_absent:

  Zero observations where `fitted > expected_absent` are potential
  anomalies.

- random_threshold:

  The minimal relative effect size of a random effect. Random effect
  with a smaller effect size will never be an anomaly. Defaults to 1.05
  (5%).

- verbose:

  Print extra information on the screen
