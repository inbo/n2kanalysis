# Create an `n2kImport` object

A new `n2kImport` model.

## Usage

``` r
n2k_import(...)

# S4 method for class 'ANY'
n2k_import(...)
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

- `formula`: a string holding the model formula.

- `dataset`: A `data.frame` with `filename`, `fingerprint` and
  `import_date`.
