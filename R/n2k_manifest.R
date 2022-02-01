#' Create a n2kManifest object
#' @param manifest a data.frame with the manifest content
#' @name n2k_manifest
#' @rdname n2k_manifest
#' @exportMethod n2k_manifest
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "n2k_manifest",
  def = function(
    manifest
  ) {
    standardGeneric("n2k_manifest") # nocov
  }
)

#' @description A new n2kManifest object is created when `manifest` is a
#' `data.frame`.
#' @rdname n2k_manifest
#' @aliases n2k_manifest,n2kManifest-methods
#' @importFrom methods setMethod new
#' @importFrom dplyr %>% distinct arrange
#' @importFrom rlang .data
#' @importFrom digest sha1
#' @include n2k_manifest_class.R
setMethod(
  f = "n2k_manifest",
  signature = signature(manifest = "data.frame"),
  definition = function(
    manifest
  ) {
    assert_that(has_name(manifest, "fingerprint"))
    assert_that(has_name(manifest, "parent"))

    if (inherits(manifest, "tbl")) {
      manifest <- as.data.frame(manifest)
    }
    manifest %>%
      distinct(.data$fingerprint, .data$parent) %>%
      arrange(.data$fingerprint, .data$parent) -> manifest
    new(
      "n2kManifest",
      Manifest = manifest,
      Fingerprint = sha1(manifest)
    )
  }
)
