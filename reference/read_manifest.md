# Read a `n2kManifest` object

Read a `n2kManifest` object

## Usage

``` r
read_manifest(base, project, hash)

# S4 method for class 'character'
read_manifest(base, project, hash)

# S4 method for class 's3_bucket'
read_manifest(base, project, hash)
```

## Arguments

- base:

  The base location to read the manifest.

- project:

  Will be a relative path within the base location.

- hash:

  Optional the `sha1` of the manifest. This can be abbreviated to to
  first unique characters. The function will return an error in case of
  multiple matches. If missing, then most recent manifest will be
  returned.
