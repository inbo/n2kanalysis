# Store a Docker configuration

Store a Docker configuration

## Usage

``` r
store_manifest_yaml(x, base, project, docker, dependencies, overwrite = FALSE)

# S4 method for class 'ANY,s3_bucket'
store_manifest_yaml(x, base, project, docker, dependencies, overwrite = FALSE)

# S4 method for class 'ANY,character'
store_manifest_yaml(x, base, project, docker, dependencies, overwrite = FALSE)
```

## Arguments

- x:

  the `n2kManifest`

- base:

  the base location to store the manifest

- project:

  will be a relative path within the base location

- docker:

  the docker image to use

- dependencies:

  extra GitHub packages to install

- overwrite:

  Should an existing object be overwritten? Defaults to `TRUE`.
