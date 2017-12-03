#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.flag noNA is.dir
#' @importFrom dplyr %>% transmute_ mutate_ left_join arrange_
#' @importFrom aws.s3 get_bucket
#' @importFrom tibble rowid_to_column
#' @include n2kManifest_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kManifest"),
  definition = function(x, ...){
    dots <- list(...)
    if (is.null(dots$base)) {
      stop("base is missing")
    }
    if (is.null(dots$project)) {
      stop("project is missing")
    }
    assert_that(is.string(dots$project))
    manifest <- x@Manifest %>%
      mutate_(Level = ~ifelse(is.na(Parent), 0, 1))
    while (!all(is.na(manifest$Parent))) {
      manifest <- manifest %>%
        left_join(x@Manifest, by = c("Parent" = "Fingerprint")) %>%
        transmute_(
          ~Fingerprint,
          Parent = ~Parent.y,
          Level = ~ifelse(is.na(Parent), Level, Level + 1)
        )
    }
    if (inherits(dots$base, "character")) {
      assert_that(is.dir(dots$base), msg = "base is not a directory")
      assert_that(
        is.string(dots$project),
        msg = "project is not a string (single character vector)"
      )
      manifest <- data.frame(
        Filename = sprintf("%s/%s", dots$base, dots$project) %>%
          list.files(recursive = TRUE, full.names = TRUE),
        stringsAsFactors = FALSE
      ) %>%
        mutate_(
          Fingerprint = ~gsub(".*([[:xdigit:]]{40})\\.rds$", "\\1", Filename)
        ) %>%
        left_join(x = manifest, by = "Fingerprint") %>%
        arrange_(~Level, ~Fingerprint)
      sapply(manifest$Filename, fit_model, ...)
      return(invisible(NULL))
    }
    if (inherits(dots$base, "s3_bucket")) {
      available <- get_bucket(
        bucket = dots$base,
        prefix = dots$project,
        max = Inf
      )
      manifest <- data.frame(
        Filename = sapply(available, "[[", "Key"),
        stringsAsFactors = FALSE
      ) %>%
        rowid_to_column() %>%
        mutate_(
          Fingerprint = ~gsub(".*([[:xdigit:]]{40})\\.rds$", "\\1", Filename)
        ) %>%
        inner_join(x = manifest, by = "Fingerprint") %>%
        arrange_(~Level, ~Fingerprint)
      sapply(available[manifest$rowid], fit_model, ...)
      return(invisible(NULL))
    }
    stop("base should be either a local directory or an S3 bucket")
  }
)
