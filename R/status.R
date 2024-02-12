#' Get the status of an `n2kModel`
#' @param x the `n2kModel` object
#' @return the status of the object
#' @name status
#' @rdname status
#' @exportMethod status
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "status",
  def = function(x) {
    # nocov start
    standardGeneric("status") # nocov
    # nocov end
  }
)

#' @rdname status
#' @aliases status,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod new
#' @include n2k_analysis_metadata_class.R
setMethod(
  f = "status",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x) {
    return(x@AnalysisMetadata$status)
  }
)

#' @rdname status
#' @aliases status,character-methods
#' @importFrom methods setMethod new
#' @importFrom n2khelper read_object_environment
#' @importFrom utils file_test
setMethod(
  f = "status",
  signature = signature(x = "character"),
  definition = function(x) {
    if (length(x) == 0) {
      stop("no filename provided")
    }
    if (length(x) > 1) {
      # assume x are files when length(x) > 1
      files <- x[file_test("-f", x)]
      # ignore elements of x which are not existing files
      return(do.call(rbind, lapply(files, status)))
    }
    # assume x is a file or directory when length(x) == 1
    if (file_test("-d", x)) {
      # handle a directory
      files <- list.files(
        path = x,
        pattern = "\\.rds$",
        ignore.case = TRUE,
        full.names = TRUE,
        recursive = TRUE
      )
      return(status(files))
    }
    # handle a file
    analysis <- readRDS(x)
    return(
      data.frame(
        Filename = x,
        file_fingerprint = analysis@AnalysisMetadata$file_fingerprint,
        status_fingerprint = analysis@AnalysisMetadata$status_fingerprint,
        status = analysis@AnalysisMetadata$status,
        stringsAsFactors = FALSE
      )
    )
  }
)

#' Overwrite the status of an `n2kAnalysisMetadata`
#' @param x the `n2kAnalysisMetadata` object
#' @param value the new values for the status
#' @name status<-
#' @rdname status_change
#' @exportMethod status<-
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2k_analysis_metadata_class.R
setGeneric(
  name = "status<-",
  def = function(x, value) {
    standardGeneric("status<-") # nocov
  }
)

#' @rdname status_change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2k_inla_class.R
setReplaceMethod(
  "status",
  "n2kInla",
  function(x, value) {
    x@AnalysisMetadata$status <- value
    x@AnalysisMetadata$status_fingerprint <- sha1(
      list(
        x@AnalysisMetadata$file_fingerprint, x@AnalysisMetadata$status, x@Model,
        x@AnalysisMetadata$analysis_version, x@AnalysisVersion, x@RPackage,
        x@AnalysisVersionRPackage, x@AnalysisRelation, x@RawImputed
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)

#' @rdname status_change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2k_composite_class.R
setReplaceMethod(
  "status",
  "n2kComposite",
  function(x, value) {
    x@AnalysisMetadata$status <- value
    x@AnalysisMetadata$status_fingerprint <- sha1(
      list(
        x@AnalysisMetadata$file_fingerprint, x@AnalysisMetadata$status,
        x@Parameter, x@Index, x@AnalysisMetadata$analysis_version,
        x@AnalysisVersion, x@RPackage, x@AnalysisVersionRPackage,
        x@AnalysisRelation
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)

#' @rdname status_change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2k_inla_comparison_class.R
setReplaceMethod(
  "status",
  "n2kInlaComparison",
  function(x, value) {
    x@AnalysisMetadata$status <- value
    x@AnalysisMetadata$status_fingerprint <- sha1(
      list(
        x@AnalysisMetadata$file_fingerprint, x@AnalysisMetadata$status,
        x@WAIC, x@AnalysisMetadata$analysis_version,
        x@AnalysisVersion, x@RPackage, x@AnalysisVersionRPackage,
        x@AnalysisRelation
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)

#' @rdname status_change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2k_aggregate_class.R
setReplaceMethod(
  "status",
  "n2kAggregate",
  function(x, value) {
    x@AnalysisMetadata$status <- value
    x@AnalysisMetadata$status_fingerprint <- sha1(
      list(
        x@AnalysisMetadata$file_fingerprint, x@AnalysisMetadata$status,
        x@AnalysisMetadata$analysis_version, x@AnalysisVersion, x@RPackage,
        x@AnalysisVersionRPackage, x@AnalysisRelation, x@RawImputed,
        x@AggregatedImputed
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)

#' @rdname status_change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2k_model_imputed_class.R
setReplaceMethod(
  "status",
  "n2kModelImputed",
  function(x, value) {
    x@AnalysisMetadata$status <- value
    x@AnalysisMetadata$status_fingerprint <- sha1(
      list(
        x@AnalysisMetadata$file_fingerprint, x@AnalysisMetadata$status,
        x@AnalysisMetadata$analysis_version, x@AnalysisVersion, x@RPackage,
        x@AnalysisVersionRPackage, x@AnalysisRelation, x@AggregatedImputed,
        x@Results
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)


#' @rdname status_change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2k_hurdle_imputed_class.R
setReplaceMethod(
  "status",
  "n2kHurdleImputed",
  function(x, value) {
    x@AnalysisMetadata$status <- value
    x@AnalysisMetadata$status_fingerprint <- sha1(
      list(
        get_file_fingerprint(x), x@AnalysisMetadata$status,
        x@AnalysisVersion$fingerprint, x@AnalysisVersion, x@RPackage,
        x@AnalysisVersionRPackage, x@AnalysisRelation, x@Presence, x@Count,
        x@Hurdle
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)
