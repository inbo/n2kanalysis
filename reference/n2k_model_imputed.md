# Create an `n2kModelImputed` object

A new `n2kModelImputed` model.

## Usage

``` r
n2k_model_imputed(...)

# S4 method for class 'ANY'
n2k_model_imputed(...)
```

## Arguments

- ...:

  other arguments. See below

## Details

- `scheme_id`: a string holding the id of the scheme.

- `species_group_id`: a string identifying the species group.

- `location_group_id`: a string identifying the location group.

- `model_type`: a string identifying the type of model to fit to the
  data.

- `first_imported_year`: Oldest year considered in the data.

- `last_imported_year`: Most recent year considered in the data.

- `duration`: The width of the moving window. Defaults to the
  `last_imported_year - first_imported_year + 1`.

- `last_analysed_year`: Most recent year in the window. Defaults to
  `last_imported_year`.

- `analysis_date`: A `POSIXct` date indicating the date that the dataset
  was imported.

- `seed`: A single integer used as a seed for all calculations. A random
  seed will be inserted when missing.

&nbsp;

- `model_fun`: The `model_fun` argument of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).

- `package`: A character vector of package names which must be loaded
  for `model_fun`.

- `model_args`: An optional list for the `model_args` argument of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).

- `extractor`: An optional list for the `extractor` argument of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).

- `extractor_args`: An optional list for the `extractor_args` argument
  of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).

- `filter`: An optional list for the `filter` argument of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).

- `mutate`: An optional list for the \`mutate“ argument of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).
