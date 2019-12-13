#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.flag is.dir has_name
#' @importFrom dplyr %>% transmute mutate left_join arrange distinct
#' @importFrom aws.s3 get_bucket
#' @importFrom tibble rowid_to_column
#' @importFrom rlang .data
#' @importFrom purrr map map_lgl map2_chr pmap_chr walk
#' @include n2kManifest_class.R
#' @param local A local folder into which objects from an AWS S3 bucket are
#' downloaded.
#' @param bash Use the `littler` package do run the models in separate sessions.
#' This will release the memory.
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kManifest"),
  definition = function(
    x, base, project, status = c("new", "waiting"), verbose = TRUE, ...,
    local, bash = FALSE
  ) {
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
      if (isTRUE(bash) && requireNamespace("littler", quietly = TRUE)) {
        dots <- c(list(...), status = status)
        strings <- sapply(dots, is.string)
        dots[strings] <- paste0("\"", dots[strings], "\"")
        dots <- paste(names(dots), dots, sep = " = ", collapse = ", ")
        dots <- paste(",", dots)
        sprintf(
          "r --package n2kanalysis -e 'fit_model(
\"%s\", base = \"%s\", project = \"%s\", verbose = %s)'",
          manifest$Filename, base, project, dots, verbose
        ) %>%
          lapply(system)
      } else {
        walk(
          manifest$Filename,
          fit_model,
          base = base, project = project, status = status,
          verbose = verbose, ...
        )
      }
      return(invisible(NULL))
    }
    assert_that(
      inherits(base, "s3_bucket"),
      msg = "base should be either a local directory or an S3 bucket"
    )

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
    display(verbose, "Downloading objects")
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
    walk(
      which(!manifest$Local),
      function(i) {
        display(verbose, manifest$LocalFilename[i])
        dir.create(
          dirname(manifest$LocalFilename[i]),
          recursive = TRUE,
          showWarnings = FALSE
        )
        m <- read_model(
          manifest$Fingerprint[i], base = base, project = project
        )
        assert_that(
          !inherits(m, "try-error"),
          msg = sprintf(
            "Object '%s' is missing for manifest '%s'",
            manifest$Fingerprint[i],
            x@Fingerprint
          )
        )
        store_model(m, base = local, project = project)
      }
    )
    if (isTRUE(bash) && requireNamespace("littler", quietly = TRUE)) {
      dots <- c(list(...), status = status)
      strings <- sapply(dots, is.string)
      dots[strings] <- paste0("\"", dots[strings], "\"")
      dots <- paste(names(dots), dots, sep = " = ", collapse = ", ")
      dots <- paste(",", dots)
      sprintf(
        "r --package n2kanalysis -e 'fit_model(
\"%s\", base = \"%s\", project = \"%s\", verbose = %s)'",
        manifest$Fingerprint[manifest$ToDo], local, project, dots, verbose
      ) %>%
        lapply(system)
    } else {
      walk(
        manifest$Fingerprint[manifest$ToDo],
        fit_model,
        base = local, project = project, status = status,
        verbose = verbose, ...
      )
    }
    display(verbose, "Uploading objects")
    sapply(
      basename(manifest$LocalFilename[manifest$ToDo]),
      function(x) {
        display(verbose, x)
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
