#' Convert an `Spde` object to a mesh object
#' @param object The `Spde` object
#' @name spde2mesh
#' @rdname spde2mesh
#' @exportMethod spde2mesh
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "spde2mesh",
  def = function(object) {
    standardGeneric("spde2mesh") # nocov
  }
)

#' @rdname spde2mesh
#' @importFrom methods setMethod new
#' @include spde_class.R
setMethod(
  f = "spde2mesh",
  signature = signature(object = "Spde"),
  definition = function(object) {
    stopifnot(
      "fmesher package required but not installed." =
        requireNamespace("fmesher", quietly = TRUE),
      "sf package required but not installed." =
        requireNamespace("sf", quietly = TRUE)
    )
    max_dist <- object@Range[1]
    object@Coordinates |>
      sf::st_as_sf(coords = colnames(object@Coordinates)) |>
      sf::st_buffer(dist = max_dist) |>
      sf::st_union() |>
      sf::st_simplify(dTolerance = max_dist / 10) -> region
    fmesher::fm_mesh_2d_inla(
      boundary = region, max.edge = c(max_dist / 3, max_dist * 2),
      cutoff = max_dist / 10
    )
  }
)
