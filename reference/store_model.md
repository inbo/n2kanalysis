# Store an `n2kModel` object

Store an `n2kModel` object

## Usage

``` r
store_model(x, base, project, overwrite = TRUE, validate = TRUE)

# S4 method for class 'ANY,character'
store_model(x, base, project, overwrite = TRUE, validate = TRUE)

# S4 method for class 'ANY,s3_bucket'
store_model(x, base, project, overwrite = TRUE, validate = TRUE)
```

## Arguments

- x:

  The `n2kModel`.

- base:

  The base location to store the model.

- project:

  Will be a relative path within the base location.

- overwrite:

  Should an existing object be overwritten? Defaults to `TRUE`.

- validate:

  Check that the object is valid before storing it. Defaults to `TRUE`.
