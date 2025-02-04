#' @rdname get_result
#' @inheritParams read_model
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom methods setMethod new
#' @importFrom purrr map_chr
#' @include import_s3_classes.R
setMethod(
  f = "get_result",
  signature = signature(x = "n2kManifest"),
  definition = function(x, base, ..., verbose = TRUE) {
    assert_that(validObject(x))
    display(verbose = verbose, paste("Handle manifest", x@Fingerprint))
    order_manifest(manifest = x) |>
      vapply(
        FUN = function(hash, base, verbose, ...) {
          list(get_result(x = hash, base = base, ..., verbose = verbose))
        },
        FUN.VALUE = vector(mode = "list", length = 1), base = base,
        verbose = verbose, ...
      ) |>
      do.call(what = combine)
  }
)
