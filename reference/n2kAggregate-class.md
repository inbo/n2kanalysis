# The `n2kAggregate` class

It holds analysis data based on an aggregated imputation

## Slots

- `RawImputed`:

  A `rawImputed` object with multiple imputations.

- `Function`:

  The function to apply on each group.

- `Filter`:

  The object to pass to the `filter` argument of
  [`multimput::aggregate_impute()`](https://inbo.github.io/multimput/reference/aggregate_impute.html).

- `Join`:

  The object to pass to the `join` argument of
  [`multimput::aggregate_impute()`](https://inbo.github.io/multimput/reference/aggregate_impute.html).

- `AggregatedImputed`:

  An `aggregatedImputed` object with multiple imputations.
