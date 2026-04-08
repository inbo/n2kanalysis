# Convert a manifest in vector of analysis fingerprints

The order of the analysis is determined by the parent-child
relationship. It starts with the analyses without parents. Then it adds
the analyses with parents that have already been added. This process is
repeated until all analyses have been added.

## Usage

``` r
order_manifest(manifest)
```

## Arguments

- manifest:

  the `n2kManifest`
