# Add the results from an analysis

Add the results from an analysis

## Usage

``` r
get_result(x, base, ...)

# S4 method for class 'character,character'
get_result(x, base, ..., project, verbose = TRUE)

# S4 method for class 'character,s3_bucket'
get_result(x, base, ..., project, verbose = TRUE)

# S4 method for class 'n2kInla,ANY'
get_result(x, base, ..., verbose = TRUE)

# S4 method for class 'n2kModel,ANY'
get_result(x, verbose = TRUE, ...)

# S4 method for class 'n2kManifest,character'
get_result(x, base, ..., verbose = TRUE)

# S4 method for class 'n2kManifest,s3_bucket'
get_result(x, base, project, ..., verbose = TRUE)

# S4 method for class 's3_object,ANY'
get_result(x, base, ..., project, verbose = TRUE)
```

## Arguments

- x:

  object with the current results

- base:

  the base location to read the model

- ...:

  further arguments (see Details)

- project:

  will be a relative path within the base location

- verbose:

  Print extra information on the screen
