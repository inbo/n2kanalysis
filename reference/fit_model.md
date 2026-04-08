# Fit an `n2kModel` object

Fit an `n2kModel` object

## Usage

``` r
fit_model(x, ...)

# S4 method for class 'character'
fit_model(
  x,
  base,
  project,
  status = c("new", "waiting"),
  verbose = TRUE,
  ...,
  bucket
)

# S4 method for class 'n2kAggregate'
fit_model(x, ...)

# S4 method for class 'n2kComposite'
fit_model(x, base, project, status = "new", ...)

# S4 method for class 'n2kHurdleImputed'
fit_model(x, base, project, status = c("new", "waiting"), ...)

# S4 method for class 'n2kInla'
fit_model(
  x,
  status = "new",
  ...,
  timeout = NULL,
  seed = get_seed(x),
  num_threads = NULL,
  parallel_configs = TRUE
)

# S4 method for class 'n2kInlaComparison'
fit_model(x, base, project, status = "new", verbose = TRUE, ...)

# S4 method for class 'n2kManifest'
fit_model(
  x,
  base,
  project,
  status = c("new", "waiting"),
  verbose = TRUE,
  ...,
  local = NULL
)

# S4 method for class 'n2kModelImputed'
fit_model(x, ...)

# S4 method for class 'n2kSpde'
fit_model(
  x,
  status = "new",
  ...,
  timeout = NULL,
  seed = get_seed(x),
  num_threads = NULL,
  parallel_configs = TRUE
)

# S4 method for class 's3_object'
fit_model(x, status = c("new", "waiting"), ...)
```

## Arguments

- x:

  the `n2kModel`

- ...:

  other arguments. See details

- base:

  The root of a project. Can be either a directory on a file system or
  an AWS S3 bucket object. Extracted from `bucket` or `x` when missing.

- project:

  The subdirectory of the project. Is relative the `base`. Extracted
  from `x` when missing.

- status:

  A vector with status levels naming the levels which should be
  calculated. Defaults to `"new"`.

- verbose:

  A logical indicating if the function should display the name of the
  file and the status. Defaults to `TRUE`.

- bucket:

  The name of the AWS S3 bucket. Only used when `base` is missing.

- timeout:

  the optional number of second until the model will time out

- seed:

  See the same argument in
  [`INLA::inla.qsample()`](https://rdrr.io/pkg/INLA/man/qsample.html)
  for further information. In order to produce reproducible results, you
  ALSO need to make sure the RNG in R is in the same state, see the
  example in
  [`INLA::inla.posterior.sample()`](https://rdrr.io/pkg/INLA/man/posterior.sample.html).
  When seed is non-zero, `num_threads` is forced to `"1:1"` and
  `parallel_configs` is set to `FALSE`, since parallel sampling would
  not produce a reproducible sequence of pseudo-random numbers.

- num_threads:

  The number of threads to use in the format `"A:B"` defining the number
  threads in the outer (`A`) and inner (`B`) layer for nested
  parallelism. `A "0"` will be replaced intelligently. `seed != 0`
  requires serial computations.

- parallel_configs:

  Logical. If TRUE and not on Windows, then try to run each
  configuration in parallel (not Windows) using `A` threads (see
  `num_threads`), where each of them is using `B:0` threads.

- local:

  A local folder into which objects from an AWS S3 bucket are
  downloaded.

## Details

- `status`: A vector with status levels naming the levels which should
  be recalculated. Defaults to `c("new", "waiting")`.

- `verbose`: A logical indicating if the function should display the
  name of the file and the status. Defaults to `TRUE`.
