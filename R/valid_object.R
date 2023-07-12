#' Get the validity of objects in files
#' @rdname validObject
#' @aliases validObject,character-methods
#' @importFrom methods setMethod validObject new
#' @importFrom n2khelper read_object_environment
#' @importFrom utils file_test
#' @param object a single filename, a single path or a vector with filenames.
#' @param test See [methods::validObject()]
#' @param complete See [methods::validObject()]
#' @export
setMethod(
  f = "validObject",
  signature = signature(object = "character"),
  definition = function(object, test = FALSE, complete = FALSE) {
    if (length(object) > 1) {
      # assume x are files when length(x) > 1
      files <- object[file_test("-f", object)]
      # ignore elements of x which are not existing files
      return(
        do.call(
          rbind,
          lapply(files, validObject, test = test, complete = complete)
        )
      )
    }

    # assume x is a file or directory when length(x) == 1
    if (file_test("-d", object)) {
      # handle a directory
      files <- list.files(
        path = object,
        pattern = "\\.rd(s|a)$",
        ignore.case = TRUE,
        full.names = TRUE,
        recursive = TRUE
      )
      if (length(files) == 0) {
        stop("no matching files found")
      }
      return(validObject(files, test = test, complete = complete))
    }

    if (!file_test("-f", object)) {
      # object is not a file
      return(
        methods::validObject(
          object = object,
          test = test,
          complete = complete
        )
      )
    }

    # handle a file
    if (grepl("\\.rds$", object)) {
      objects <- list(readRDS(file = object))
      names(objects) <- object
    } else {
      local_environment <- new.env()
      load(object, envir = local_environment)
      objects <- ls(envir = local_environment) %>%
        lapply(read_object_environment, env = local_environment)
      names(objects) <- ls(envir = local_environment)
    }

    # handles an empty file
    if (length(objects) == 0) {
      return(
        data.frame(
          Filename = character(0),
          Object = character(0),
          Valid = logical(0)
        )
      )
    }
    invalid <- sapply(
      objects,
      function(x) {
        try(
          validObject(x, test = test, complete = complete),
          silent = TRUE
        ) %>%
          inherits("try-error")
      }
    )
    data.frame(
      Filename = object,
      Object = names(objects),
      Valid = !invalid,
      stringsAsFactors = FALSE
    )
  }
)
