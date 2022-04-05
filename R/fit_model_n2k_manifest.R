#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.string is.flag is.dir has_name
#' @importFrom dplyr %>% transmute mutate left_join arrange distinct
#' @importFrom aws.s3 get_bucket
#' @importFrom tibble rowid_to_column
#' @importFrom rlang .data
#' @importFrom purrr map map_lgl map2_chr pmap_chr walk
#' @include n2k_manifest_class.R
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
      mutate(level = ifelse(is.na(.data$parent), 0, 1))
    while (!all(is.na(manifest$parent))) {
      manifest <- manifest %>%
        left_join(x@Manifest, by = c("parent" = "fingerprint")) %>%
        transmute(
          .data$fingerprint,
          parent = .data$parent.y,
          level = ifelse(is.na(.data$parent), .data$level, .data$level + 1)
        )
    }
    manifest %>%
      distinct(.data$fingerprint, .data$level) -> manifest
    if (inherits(base, "character")) {
      assert_that(is.dir(base))
      manifest <- data.frame(
        filename = paste(base, project, sep = "/") %>%
          list.files(recursive = TRUE, full.names = TRUE),
        stringsAsFactors = FALSE
      ) %>%
        mutate(
          fingerprint = gsub(
            ".*([[:xdigit:]]{40})\\.rds$", "\\1", .data$filename
          )
        ) %>%
        left_join(x = manifest, by = "fingerprint") %>%
        arrange(.data$level, .data$fingerprint)
      if (isTRUE(bash) && requireNamespace("littler", quietly = TRUE)) {
        dots <- c(list(...), status = status)
        strings <- sapply(dots, is.string)
        dots[strings] <- paste0("\"", dots[strings], "\"")
        dots <- paste(names(dots), dots, sep = " = ", collapse = ", ")
        dots <- paste(",", dots)
        sprintf(
          "r --package n2kanalysis -e 'fit_model(
\"%s\", base = \"%s\", project = \"%s\", verbose = %s)'",
          manifest$filename, base, project, dots, verbose
        ) %>%
          lapply(system)
      } else {
        walk(
          manifest$filename, fit_model, base = base, project = project,
          status = status, verbose = verbose, ...
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
        prefix = file.path(
          project, substring(.data$fingerprint, 1, 4), fsep = "/"
        ),
        filename = map(.data$prefix, get_bucket, bucket = base) %>%
          map("Contents") %>%
          map("Key"),
        filename = map2_chr(
          .data$fingerprint, .data$filename, ~.y[grep(.x, .y)]
        ),
        status = dirname(.data$filename) %>%
          basename(),
        to_do = .data$status %in% status
      ) %>%
      arrange(.data$level, .data$fingerprint) -> manifest
    if (!any(manifest$to_do)) {
      return(invisible(NULL))
    }
    display(verbose, "Downloading objects")
    if (missing(local)) {
      local <- tempfile("fit_model")
      dir.create(local, showWarnings = FALSE)
    }
    file.path(local, project) %>%
      list.files(recursive = TRUE, full.names = TRUE) -> local_files
    manifest %>%
      mutate(
        is_local = map_lgl(
          .data$fingerprint,
          function(h) {
            any(grepl(h, local_files))
          }
        ),
        local_filename = pmap_chr(
          list(.data$is_local, .data$fingerprint, .data$filename),
          function(l, h, f) {
            ifelse(l, local_files[grep(h, local_files)], file.path(local, f))
          }
        )
      ) -> manifest
    walk(
      which(!manifest$is_local),
      function(i) {
        display(verbose, manifest$local_filename[i])
        dir.create(
          dirname(manifest$local_filename[i]), recursive = TRUE,
          showWarnings = FALSE
        )
        m <- read_model(manifest$fingerprint[i], base = base, project = project)
        assert_that(
          !inherits(m, "try-error"),
          msg = sprintf(
            "Object '%s' is missing for manifest '%s'", manifest$fingerprint[i],
            x@fingerprint
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
        manifest$fingerprint[manifest$to_do], local, project, dots, verbose
      ) %>%
        lapply(system)
    } else {
      walk(
        manifest$fingerprint[manifest$to_do], fit_model, base = local,
        project = project, status = status, verbose = verbose, ...
      )
    }
    display(verbose, "Uploading objects")
    sapply(
      basename(manifest$local_filename[manifest$to_do]),
      function(x) {
        display(verbose, x)
        object <- try(read_model(x, base = local, project = project))
        if (!inherits(object, "try-error")) {
          store_model(x = object, base = base, project = project)
        }
      }
    )
    map(manifest$fingerprint, delete_model, base = local, project = project)
    return(invisible(NULL))
  }
)
