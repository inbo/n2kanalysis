#' @rdname get_result
#' @inheritParams read_model
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom methods setMethod new
#' @importFrom purrr map_chr
#' @include import_s3_classes.R
setMethod(
  f = "get_result",
  signature = signature(x = "n2kManifest", base = "character"),
  definition = function(x, base, ..., verbose = TRUE) {
    assert_that(validObject(x))
    display(verbose = verbose, paste("Handle manifest", x@Fingerprint))
    order_manifest(manifest = x) |>
      vapply(
        FUN = function(hash, base, verbose, ...) {
          list(get_result(x = hash, base = base, ..., verbose = verbose))
        },
        FUN.VALUE = vector(mode = "list", length = 1),
        base = base,
        verbose = verbose,
        ...
      ) |>
      do.call(what = combine)
  }
)

#' @rdname get_result
#' @inheritParams read_model
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom methods setMethod new
#' @importFrom purrr map_chr
#' @include import_s3_classes.R
setMethod(
  f = "get_result",
  signature = signature(x = "n2kManifest", base = "s3_bucket"),
  definition = function(x, base, project, ..., verbose = TRUE) {
    assert_that(validObject(x))
    display(verbose = verbose, paste("Handle manifest", x@Fingerprint))
    get_bucket(
      bucket = base,
      prefix = file.path(project, "results"),
      max = Inf
    ) |>
      map_chr("Key") |>
      basename() -> done
    to_do <- order_manifest(manifest = x)
    vapply(
      to_do[!paste0(to_do, ".rds") %in% done],
      FUN.VALUE = logical(1),
      FUN = function(x, base, verbose, project, ...) {
        display(verbose = verbose, paste("  extracting", x))
        substring(x, 1, 4) |>
          sprintf(fmt = "%2$s/%1$s", project) |>
          get_bucket(bucket = base, max = Inf) -> available
        available <- available[
          map_chr(available, "Key") |>
            grepl(pattern = x)
        ]
        stopifnot(
          "object not found or multiple objects found" = length(available) == 1
        )
        get_result(
          available[[1]],
          base = base,
          verbose = verbose,
          project = project,
          ...
        )
        gc(verbose = FALSE)
        return(TRUE)
      },
      base = base,
      verbose = verbose,
      project = project,
      ...
    )
    order_manifest(manifest = x) |>
      vapply(
        FUN = function(hash, base, project, verbose, ...) {
          get_result(
            x = hash,
            base = base,
            project = project,
            ...,
            verbose = verbose
          ) |>
            list()
        },
        FUN.VALUE = vector(mode = "list", length = 1),
        base = base,
        verbose = verbose,
        project = project,
        ...
      ) |>
      do.call(what = combine)
  }
)
