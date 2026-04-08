# Create an `Spde` object

Create an `Spde` object

## Usage

``` r
spde(coordinates, range, sigma)
```

## Arguments

- coordinates:

  a `data.frame` of coordinates use to define the mesh.

- range:

  a numeric vector of length 2. Will be used as the `prior.range`
  argument of `[INLA::inla.spde2.pcmatern]`.

- sigma:

  a numeric vector of length 2. Will be used as the `prior.sigma`
  argument of `[INLA::inla.spde2.pcmatern]`.
