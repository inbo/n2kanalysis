# The `n2kInla` class

It hold analysis data based on an INLA Poisson model

## Slots

- `Data`:

  A `data.frame` with the data.

- `LinearCombination`:

  An optional matrix with the linear combinations.

- `ReplicateName`:

  An optional list with names of replicates.

- `Model`:

  Either NULL or the resulting INLA model.

- `Family`:

  The family of the INLA model.

- `Control`:

  A named list with options passed to the arguments of
  [`INLA::inla()`](https://rdrr.io/pkg/INLA/man/inla.html).

- `ImputationSize`:

  The number of multiple imputations. Defaults to `0`, indication no
  multiple imputation.

- `Minimum`:

  An optional string containing the name of the variable in `Data`
  holding the minimal values for imputation.

- `RawImputed`:

  A `rawImputed` object with multiple imputations.

- `Extra`:

  A data.frame with extra data to add to the imputations. This data is
  not used in the imputation model. It must contain the same variables
  as the original data.
