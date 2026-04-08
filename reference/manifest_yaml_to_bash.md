# Convert a manifest yaml file into a bash script

Convert a manifest yaml file into a bash script

## Usage

``` r
manifest_yaml_to_bash(
  base,
  project,
  hash,
  shutdown = FALSE,
  split = 1,
  status = c("new", "waiting"),
  limit = FALSE,
  timeout = 4
)

# S4 method for class 's3_bucket'
manifest_yaml_to_bash(
  base,
  project,
  hash,
  shutdown = FALSE,
  split = 1,
  status = c("new", "waiting"),
  limit = FALSE,
  timeout = 4
)

# S4 method for class 'character'
manifest_yaml_to_bash(
  base,
  project,
  hash,
  shutdown = FALSE,
  split = 1,
  status = c("new", "waiting"),
  limit = FALSE,
  timeout = 4
)
```

## Arguments

- base:

  the base location to store the manifest

- project:

  will be a relative path within the base location

- hash:

  Fingerprint of the manifest `yaml`file.

- shutdown:

  Append a shutdown command at the end of the script. Defaults to
  `FALSE`.

- split:

  Number of scripts over which to splits the analyses. Default to 1.

- status:

  A vector with status levels naming the levels which should be
  calculated. Defaults to `c("new", "waiting")`.

- limit:

  Limit bandwidth and CPU usage. Defaults to `FALSE`.

- timeout:

  number of hours to time out the docker container. Defaults to `4`.
