#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.flag noNA is.dir has_name
#' @importFrom dplyr %>% transmute mutate left_join arrange distinct
#' @importFrom aws.s3 get_bucket
#' @importFrom tibble rowid_to_column
#' @importFrom rlang .data
#' @importFrom purrr map_lgl pmap_chr map
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
    manifest %>%
      distinct(.data$Fingerprint, .data$Level) -> manifest
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
      if (has_name(dots, "bash")) {
        strings <- sapply(dots, is.string)
        dots[strings] <- paste0("\"", dots[strings], "\"")
        sprintf(
          "r --package n2kanalysis -e 'fit_model(\"%s\", %s)'",
          manifest$Filename,
          paste(names(dots), dots, sep = " = ", collapse = ", ")
        ) %>%
          lapply(system)
      } else {
        sapply(manifest$Filename, fit_model, ...)
      }
      return(invisible(NULL))
    }
    if (!inherits(dots$base, "s3_bucket")) {
      stop("base should be either a local directory or an S3 bucket")
    }
    if (has_name(dots, "status")) {
      assert_that(is.character(dots$status))
      assert_that(length(dots$status) >= 1)
    } else {
      dots$status <- c("new", "waiting")
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
        Fingerprint = gsub(
          ".*([[:xdigit:]]{40})\\.rds$", "\\1",
          .data$Filename
        ),
        Status = dirname(.data$Filename) %>%
          basename(),
        ToDo = .data$Status %in% dots$status
      ) %>%
      inner_join(x = manifest, by = "Fingerprint") %>%
      arrange(.data$Level, .data$Fingerprint)
    if (!any(manifest$Status %in% dots$status)) {
      return(invisible(NULL))
    }
    if (!has_name(dots, "verbose")) {
      dots$verbose <- TRUE
    }
    if (dots$verbose) {
      message("Downloading objects")
    }
    if (is.null(dots$local)) {
      dots$local <- tempdir()
    }
    file.path(dots$local, dots$project) %>%
      list.files(recursive = TRUE) -> local
    manifest %>%
      mutate(
        Local = map_lgl(
          .data$Fingerprint,
          function(h) {
            any(grepl(h, local))
          }
        ),
        LocalFilename = pmap_chr(
          list(
            .data$Local,
            .data$Fingerprint,
            .data$Filename
          ),
          function(l, h, f) {
            ifelse(l, local[grep(h, local)], f)
          }
        )
      ) -> manifest
    for (i in which(!manifest$Local)) {
      if (dots$verbose) {
        message(manifest$LocalFilename[i])
      }
      if (!dir.exists(dirname(manifest$LocalFilename[i]))) {
        dir.create(dirname(manifest$LocalFilename[i]), recursive = TRUE)
      }
      m <- read_model(
        manifest$Fingerprint[i], base = dots$base, project = dots$project
      )
      if (inherits(m, "try-error")) {
        stop(
          sprintf(
            "Object '%s' is missing for manifest '%s'",
            manifest$Fingerprint[i],
            x@Fingerprint
          )
        )
      }
      store_model(m, base = dots$local, project = dots$project)
    }
    if (has_name(dots, "bash")) {
      pass_dots <- dots
      pass_dots$base <- pass_dots$local
      pass_dots$local <- NULL
      strings <- sapply(pass_dots, is.string)
      pass_dots[strings] <- paste0("\"", pass_dots[strings], "\"")
      sprintf(
        "r --package n2kanalysis -e 'fit_model(\"%s\", %s)'",
        manifest$LocalFilename[manifest$ToDo],
        paste(names(pass_dots), pass_dots, sep = " = ", collapse = ", ")
      ) %>%
        lapply(system)
    } else {
      sapply(
        manifest$LocalFilename[manifest$ToDo],
        fit_model,
        base = dots$local,
        project = dots$project,
        ...
      )
    }
    if (dots$verbose) {
      message("Uploading objects")
    }
    sapply(
      basename(manifest$LocalFilename[manifest$ToDo]),
      function(x) {
        if (dots$verbose) {
          message(x)
        }
        object <- try(read_model(x, base = dots$local, project = dots$project))
        if (!inherits(object, "try-error")) {
          store_model(x = object, base = dots$base, project = dots$project)
        }
      }
    )
    map(
      manifest$Fingerprint,
      delete_model,
      base = dots$local,
      project = dots$project
    )
    return(invisible(NULL))
  }
)
