#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.flag noNA is.dir
#' @importFrom dplyr %>% transmute mutate left_join arrange
#' @importFrom aws.s3 get_bucket
#' @importFrom tibble rowid_to_column
#' @importFrom rlang .data
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
      mutate(Level = ifelse(is.na(.data$Parent), 0, 1))
    while (!all(is.na(manifest$Parent))) {
      manifest <- manifest %>%
        left_join(x@Manifest, by = c("Parent" = "Fingerprint")) %>%
        transmute(
          .data$Fingerprint,
          Parent = .data$Parent.y,
          Level = ifelse(is.na(.data$Parent), .data$Level, .data$Level + 1)
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
        mutate(
          Fingerprint = gsub(
            ".*([[:xdigit:]]{40})\\.rds$",
            "\\1",
            .data$Filename
          )
        ) %>%
        left_join(x = manifest, by = "Fingerprint") %>%
        arrange(.data$Level, .data$Fingerprint)
      sapply(manifest$Filename, fit_model, ...)
      return(invisible(NULL))
    }
    if (!inherits(dots$base, "s3_bucket")) {
      stop("base should be either a local directory or an S3 bucket")
    }
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
      mutate(
        Fingerprint = gsub(".*([[:xdigit:]]{40})\\.rds$", "\\1", .data$Filename)
      ) %>%
      inner_join(x = manifest, by = "Fingerprint") %>%
      arrange(.data$Level, .data$Fingerprint)
    if (is.null(dots$local)) {
      dots$local <- tempdir()
    }
    targets <- paste(dots$local, manifest$Filename, sep = "/") %>%
      normalizePath(mustWork = FALSE)
    downloads <- which(!file.exists(targets))
    for (i in downloads) {
      if (!dir.exists(dirname(targets[i]))) {
        dir.create(dirname(targets[i]), recursive = TRUE)
      }
      s3readRDS(manifest$Filename[i], bucket = dots$base) %>%
        saveRDS(targets[i])
    }
    sapply(targets, fit_model, base = dots$local, project = dots$project, ...)
    targets <- sapply(
      basename(targets),
      function(x) {
        read_model(x, base = dots$local, project = dots$project) %>%
          store_model(base = dots$base, project = dots$project)
      }
    )
    paste(dots$local, targets, sep = "/") %>%
      unlink()
    return(invisible(NULL))
  }
)
