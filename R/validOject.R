#' Get the validity of objects in files
#' @rdname validObject
#' @aliases validObject,character-methods
#' @importFrom methods setMethod validObject
#' @importFrom n2khelper check_path read_object_environment
#' @param object a single filename, a single path or a vector with filenames.
#' @param test See \code{\link[methods]{validObject}}
#' @param complete See \code{\link[methods]{validObject}}
#' @export
setMethod(
  f = "validObject",
  signature = signature(object = "character"),
  definition = function(object, test = FALSE, complete = FALSE){
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
    } else {
      # assume x is a file or directory when length(x) == 1
      if (file_test("-d", object)) {
        # handle a directory
        path <- check_path(object, type = "directory")
        files <- list.files(
          path = path,
          pattern = "\\.rda$",
          full.names = TRUE,
          recursive = TRUE
        )
        if (length(files) == 0) {
          stop("no matching files found")
        }
        return(validObject(files, test = test, complete = complete))
      } else {
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
        object <- check_path(object, type = "file")
        local.environment <- new.env()
        load(object, envir = local.environment)
        objects <- ls(envir = local.environment)
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
        valid <- sapply(
          objects,
          function(x){
            y <- read_object_environment(
              object = x,
              env = local.environment
            )
            check <- try(validObject(y, test = test, complete = complete))
            class(check) != "try-error"
          }
        )
        data.frame(
          Filename = object,
          Object = objects,
          Valid = valid,
          stringsAsFactors = FALSE
        )
      }
    }
  }
)
