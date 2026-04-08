# The `n2kModelImputed` class

It holds the model of aggregated imputed data

## Slots

- `Function`:

  The object to pass to the `model.fun` argument of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).

- `Package`:

  A vector of package names which must be loaded to run the function.

- `ModelArgs`:

  The object to pass to the `model.args` argument of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).

- `PrepareModelArgs`:

  An optional list containing a single function that will be applied to
  the object. The result of the function will be appended to the
  `ModelsArgs`.

- `Extractor`:

  The object to pass to the `extractor` argument of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).

- `ExtractorArgs`:

  The object to pass to the `extractor.args` argument of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).

- `Filter`:

  The object to pass to the `filter` argument of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).

- `Mutate`:

  The object to pass to the `mutate` argument of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).

- `AggregatedImputed`:

  An `aggregatedImputed` object with multiple imputations.

- `Results`:

  The `data.frame` with the results of
  [`multimput::model_impute()`](https://inbo.github.io/multimput/reference/model_impute.html).
