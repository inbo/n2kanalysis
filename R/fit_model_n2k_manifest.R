#' @rdname fit_model
#' @importFrom assertthat assert_that is.string is.flag is.dir has_name noNA
#' @importFrom fs dir_create dir_ls path
#' @importFrom methods setMethod new
#' @importFrom purrr walk
#' @importFrom stats na.omit
#' @importFrom tools R_user_dir
#' @include n2k_manifest_class.R
#' @param local A local folder into which objects from an AWS S3 bucket are
#' downloaded.
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kManifest"),
  definition = function(
    x, base, project, status = c("new", "waiting"), verbose = TRUE, ...,
    local = tempfile("fit_model")
  ) {
    assert_that(
      is.string(project), noNA(project), is.character(status), noNA(status),
      length(status) >= 1
    )
    to_do <- order_manifest(x)
    R_user_dir("n2kanalysis", which = "cache") |>
      file.path(x@Fingerprint) -> cache_file
    dirname(cache_file) |>
      dir.create(recursive = TRUE, showWarnings = FALSE)
    if (file_test("-f", cache_file)) {
      processed <- read.table(cache_file, header = TRUE, sep = "\t")
      done <- processed$status %in% status
      to_do <- to_do[!to_do %in% processed$fingerprint[done]]
    } else {
      data.frame(fingerprint = character(0), status = character(0)) |>
        write.table(
          file = cache_file, sep = "\t", row.names = FALSE, quote = FALSE
        )
    }
    for (i in seq_along(to_do)) {
      display(
        verbose = verbose,
        message = sprintf(
          "Processing %i from %i (%.2f%%)", i, length(to_do),
          100 * (i - 1) / length(to_do)
        )
      )
      result <- try(fit_model(
        x = to_do[i], base = base, project = project, status = status,
        verbose = verbose, ..., local = local
      ))
      if (!inherits(result, "try-error")) {
        write.table(
          result, file = cache_file, append = TRUE, sep = "\t",
          row.names = FALSE, quote = FALSE, col.names = FALSE
        )
      }
    }
    return(invisible(NULL))
  }
)

download_model <- function(hash, base, local, project, verbose = FALSE) {
  display(verbose, paste("Moving", hash))
  read_model(x = hash, base = base, project = project) |>
    store_model(base = local, project = project)
}
