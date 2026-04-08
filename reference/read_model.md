# Read an `n2kModel` object

Read an `n2kModel` object

## Usage

``` r
read_model(x, base, project)

# S4 method for class 'ANY,character'
read_model(x, base, project)

# S4 method for class 'ANY,s3_bucket'
read_model(x, base, project)

# S4 method for class 'ANY,ANY'
read_model(x, base, project)
```

## Arguments

- x:

  the file fingerprint of the `n2kModel`

- base:

  the base location to read the model

- project:

  will be a relative path within the base location
