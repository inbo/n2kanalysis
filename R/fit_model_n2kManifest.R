#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.flag is.dir has_name
#' @importFrom dplyr %>% transmute mutate left_join arrange distinct
#' @importFrom aws.s3 get_bucket
#' @importFrom tibble rowid_to_column
#' @importFrom rlang .data
#' @importFrom purrr map_lgl pmap_chr map map2_chr
#' @include n2kManifest_class.R
#' @param local a local folder into which objects from an AWS S3 bucket are downloaded
#' @param bash use the `littler` package do run the models in separate sessions. This will release the memory.
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kManifest"),
  definition = function(
    x, base, project, status = "new", verbose = TRUE, ..., local, bash = FALSE
  ){
    assert_that(is.string(project))
    assert_that(is.flag(bash))
    assert_that(is.character(status))
    assert_that(length(status) >= 1)

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
    if (inherits(base, "character")) {
      assert_that(is.dir(base))
      manifest <- data.frame(
        Filename = sprintf("%s/%s", base, project) %>%
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
      if (isTRUE(bash)) {
        assert_that(requireNamespace("littler", quietly = TRUE))
        dots <- c(list(...), status = status)
        if (length(dots)) {
          strings <- sapply(dots, is.string)
          dots[strings] <- paste0("\"", dots[strings], "\"")
          dots <- paste(names(dots), dots, sep = " = ", collapse = ", ")
          dots <- paste(",", dots)
        } else {
          dots <- ""
        }
        sprintf(
          "r --package n2kanalysis -e 'fit_model(
\"%s\", base = \"%s\", project = \"%s\", verbose = %s)'",
          manifest$Filename, base, project, dots, verbose
        ) %>%
          lapply(system)
      } else {
        for (i in manifest$Filename) {
          fit_model(
            i, base = base, project = project, status = status,
            verbose = verbose, ...
          )
        }
      }
      return(invisible(NULL))
    }
    if (!inherits(base, "s3_bucket")) {
      stop("base should be either a local directory or an S3 bucket")
    }
    manifest %>%
      mutate(
        Prefix = sprintf("%s/%s", project, substring(.data$Fingerprint, 1, 4)),
        Filename = map(.data$Prefix, get_bucket, bucket = base) %>%
          map("Contents") %>%
          map("Key"),
        Filename = map2_chr(
          .data$Fingerprint, .data$Filename,
          ~.y[grep(.x, .y)]
        ),
        Status = dirname(.data$Filename) %>%
          basename(),
        ToDo = .data$Status %in% status
      ) %>%
      arrange(.data$Level, .data$Fingerprint) -> manifest
    if (!any(manifest$ToDo)) {
      return(invisible(NULL))
    }
    if (isTRUE(verbose)) {
      message("Downloading objects")
    }
    if (missing(local)) {
      local <- tempdir()
    }
    file.path(local, project) %>%
      list.files(recursive = TRUE) -> local_files
    manifest %>%
      mutate(
        Local = map_lgl(
          .data$Fingerprint,
          function(h) {
            any(grepl(h, local_files))
          }
        ),
        LocalFilename = pmap_chr(
          list(
            .data$Local,
            .data$Fingerprint,
            .data$Filename
          ),
          function(l, h, f) {
            ifelse(l, local_files[grep(h, local_files)], f)
          }
        )
      ) -> manifest
    for (i in which(!manifest$Local)) {
      if (isTRUE(verbose)) {
        message(manifest$LocalFilename[i])
      }
      if (!dir.exists(dirname(manifest$LocalFilename[i]))) {
        dir.create(dirname(manifest$LocalFilename[i]), recursive = TRUE)
      }
      m <- read_model(
        manifest$Fingerprint[i], base = base, project = project
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
      store_model(m, base = local, project = project)
    }
    if (isTRUE(bash)) {
      assert_that(requireNamespace("littler", quietly = TRUE))
      dots <- c(list(...), status = status)
      if (length(dots)) {
        strings <- sapply(dots, is.string)
        dots[strings] <- paste0("\"", dots[strings], "\"")
        dots <- paste(names(dots), dots, sep = " = ", collapse = ", ")
        dots <- paste(",", dots)
      } else {
        dots <- ""
      }
      sprintf(
        "r --package n2kanalysis -e 'fit_model(
\"%s\", base = \"%s\", project = \"%s\", verbose = %s)'",
        manifest$Fingerprint[manifest$ToDo], local, project, dots, verbose
      ) %>%
        lapply(system)
    } else {
      for (i in manifest$Fingerprint[manifest$ToDo]) {
        fit_model(
          i, base = local, project = project, status = status,
          verbose = verbose, ...
        )
      }
    }
    if (isTRUE(verbose)) {
      message("Uploading objects")
    }
    sapply(
      basename(manifest$LocalFilename[manifest$ToDo]),
      function(x) {
        if (isTRUE(verbose)) {
          message(x)
        }
        object <- try(read_model(x, base = local, project = project))
        if (!inherits(object, "try-error")) {
          store_model(x = object, base = base, project = project)
        }
      }
    )
    map(manifest$Fingerprint, delete_model, base = local, project = project)
    return(invisible(NULL))
  }
)
