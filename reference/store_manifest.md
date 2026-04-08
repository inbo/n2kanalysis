# Store an `n2kManifest` object

Store an `n2kManifest` object

## Usage

``` r
store_manifest(x, base, project, overwrite = FALSE)

# S4 method for class 'ANY,character'
store_manifest(x, base, project, overwrite = FALSE)

# S4 method for class 'ANY,s3_bucket'
store_manifest(x, base, project, overwrite = FALSE)
```

## Arguments

- x:

  the `n2kManifest`

- base:

  the base location to store the manifest

- project:

  will be a relative path within the base location

- overwrite:

  Should an existing object be overwritten? Defaults to `TRUE`.
