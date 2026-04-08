# The `spde` class

It holds the coordinates, range and sigma parameters for the SPDE model.

## Slots

- `Coordinates`:

  a `data.frame` with the coordinates used for the mesh.

- `Range`:

  a numeric vector of length 2. Will be used as the `prior.range`
  argument of `[INLA::inla.spde2.pcmatern]`.

- `Sigma`:

  a numeric vector of length 2. Will be used as the `prior.sigma`
  argument of `[INLA::inla.spde2.pcmatern]`.
