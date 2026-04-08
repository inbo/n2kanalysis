# Create an `n2kSpde` object

A new `n2kSpde` model is created when `data` is a `data.frame`.

In case `data` is an `n2kSpde` object, then only the model and status
are updated. All other slots are unaffected.

## Usage

``` r
n2k_spde(data, ..., model_fit)

# S4 method for class 'data.frame,ANY'
n2k_spde(
  data,
  status = "new",
  result_datasource_id,
  scheme_id,
  family = "poisson",
  formula,
  species_group_id,
  location_group_id,
  model_type,
  spde,
  first_imported_year,
  last_imported_year,
  duration,
  last_analysed_year,
  analysis_date,
  lin_comb = NULL,
  minimum = "",
  imputation_size,
  parent = character(0),
  seed,
  replicate_name = list(),
  control = list(),
  parent_status = "converged",
  parent_statusfingerprint,
  extra,
  ...,
  model_fit
)

# S4 method for class 'n2kSpde,inla'
n2k_spde(data, status, raw_imputed = NULL, ..., model_fit)
```

## Arguments

- data:

  a `data.frame` with the data to analyse

- ...:

  other arguments. See below

- model_fit:

  The fitted model

- status:

  A single character indicating the status of the model. Defaults to
  `"waiting"`.

- result_datasource_id:

  A string identifying the data source.

- scheme_id:

  A single integer holding the id of the scheme.

- family:

  the family to use in the INLA model.

- formula:

  A single character identifying the comparison.

- species_group_id:

  A string identifying the species group.

- location_group_id:

  A string identifying the location group.

- model_type:

  The type of the models. Must start with `"inla comparison:"`.

- spde:

  the `Spde` object. See `[spde]`.

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

- lin_comb:

  A model matrix to calculate linear combinations.

- minimum:

  The name of the variable which holds the minimum counts. Only relevant
  in case of multiple imputation.

- imputation_size:

  The required number of imputations defaults to 0.

- parent:

  The file fingerprint of the optional parent analysis.

- seed:

  A single integer used as a seed for all calculations. A random seed
  will be inserted when missing.

- replicate_name:

  A list with the names of replicates. Defaults to an empty list. Used
  in case of `f(X, ..., replicate = Z)`. Should be a named list like
  e.g. `list(X = c("a", "b", "c"))`.

- control:

  A named list passed to
  [`INLA::inla()`](https://rdrr.io/pkg/INLA/man/inla.html) when fitting
  the model.

- parent_status:

  A `data.frame` with columns `parent_analysis` (the file fingerprint of
  the parent), `parentstatus_fingerprint` (the status fingerprint of the
  parent), and `parent_status` (the status of the parent).

- parent_statusfingerprint:

  The status fingerprint of the parent analysis.

- extra:

  a `data.frame` with extra observations not used in the model. They
  will be added in subsequent analyses.

- raw_imputed:

  the optional `rawImputed` object
