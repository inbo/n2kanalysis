# Create an `n2kInlaComparison` object

A new `n2kInlaComparison` model is created when `parent` is a
`character`.

## Usage

``` r
n2k_inla_comparison(parent_status, ...)

# S4 method for class 'data.frame'
n2k_inla_comparison(
  parent_status,
  status = "waiting",
  result_datasource_id,
  scheme_id,
  formula,
  species_group_id,
  location_group_id,
  model_type,
  first_imported_year,
  last_imported_year,
  duration,
  last_analysed_year,
  analysis_date,
  ...,
  seed
)
```

## Arguments

- parent_status:

  A `data.frame` with columns `parent_analysis` (the file fingerprint of
  the parent), `parentstatus_fingerprint` (the status fingerprint of the
  parent), and `parent_status` (the status of the parent).

- ...:

  other arguments

- status:

  A single character indicating the status of the model. Defaults to
  `"waiting"`.

- result_datasource_id:

  A string identifying the data source.

- scheme_id:

  A single integer holding the id of the scheme.

- formula:

  A single character identifying the comparison.

- species_group_id:

  A string identifying the species group.

- location_group_id:

  A string identifying the location group.

- model_type:

  The type of the models. Must start with `"inla comparison:"`.

- first_imported_year:

  Oldest year considered in the data.

- last_imported_year:

  Most recent year considered in the data.

- duration:

  The width of the moving window. Defaults to the
  `last_imported_year - first_imported_year + 1`.

- last_analysed_year:

  Most recent year in the window. Defaults to `last_imported_year`.

- analysis_date:

  A `POSIXct` date indicating the date that the dataset was imported.

- seed:

  A single integer used as a seed for all calculations. A random seed
  will be inserted when missing.
