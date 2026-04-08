# Changelog

## `n2kanalysis` 0.4.1

- Improve
  [`get_result()`](https://inbo.github.io/n2kanalysis/reference/get_result.md).
- Add
  [`connect_inbo_s3()`](https://inbo.github.io/n2kanalysis/reference/connect_inbo_s3.md)
  function to make the temporary credentials available to the R
  functions.
- Improve automated tests.
- Update code style with `air`.

## `n2kanalysis` 0.4.0

- Handle INLA models with an SPDE element.

## `n2kanalysis` 0.3.2

- Make
  [`fit_model()`](https://inbo.github.io/n2kanalysis/reference/fit_model.md)
  more efficient.

## `n2kanalysis` 0.3.1

- Add the `n2kHurdleImputed` class and
  [`n2k_hurdle_imputed()`](https://inbo.github.io/n2kanalysis/reference/n2k_hurdle_imputed.md)
  to fit hurdle models with imputation.
- Add `Manifest_yaml_to_bash()` to convert a manifest file into a bash
  script.
- Update [`checklist`](https://inbo.github.io/checklist/)
  infrastructure.

## `n2kanalysis` 0.3.0

### Breaking changes

- Add [`checklist`](https://inbo.github.io/checklist/) infrastructure.
  This required renaming some function names and arguments in order to
  comply with the style guide.

### Other changes

- Suggest `INLA` instead of importing it to make `n2kanalysis` build on
  [R-universe](https://inbo.r-universe.dev).
- Added a `NEWS.md` file to track changes to the package.
