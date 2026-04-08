# Read an `n2kResult` object

Read an `n2kResult` object

## Usage

``` r
read_result(x, base, project)

# S4 method for class 'ANY,character'
read_result(x, base, project)

# S4 method for class 'ANY,s3_bucket'
read_result(x, base, project)

# S4 method for class 'ANY,ANY'
read_result(x, base, project)
```

## Arguments

- x:

  the file fingerprint of the `n2kResult`

- base:

  the base location to read the results

- project:

  will be a relative path within the base location
