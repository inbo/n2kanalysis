#' Get the status of a n2kModel
#' @param x the n2kModel object
#' @return the status of the object
#' @name status
#' @rdname status
#' @exportMethod status
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "status",
  def = function(x){
    # nocov start
    standardGeneric("status") # nocov
    # nocov end
  }
)

#' @rdname status
#' @aliases status,n2kAnalysisMetadata-methods
#' @importFrom methods setMethod new
#' @include n2kAnalysisMetadata_class.R
setMethod(
  f = "status",
  signature = signature(x = "n2kAnalysisMetadata"),
  definition = function(x){
    return(x@AnalysisMetadata$Status)
  }
)

#' @rdname status
#' @aliases status,character-methods
#' @importFrom methods setMethod new
#' @importFrom n2khelper check_path read_object_environment
#' @importFrom utils file_test
setMethod(
  f = "status",
  signature = signature(x = "character"),
  definition = function(x){
    if (length(x) > 1) {
      # assume x are files when length(x) > 1
      files <- x[file_test("-f", x)]
      # ignore elements of x which are not existing files
      return(do.call(rbind, lapply(files, status)))
    } else {
      # assume x is a file or directory when length(x) == 1
      if (file_test("-d", x)) {
        # handle a directory
        path <- check_path(x, type = "directory")
        files <- list.files(path = path, pattern = "\\.rds$", full.names = TRUE)
        return(status(files))
      } else {
        # handle a file
        x <- check_path(x, type = "file")
        analysis <- readRDS(x)
        return(
          data.frame(
            Filename = x,
            FileFingerprint = analysis@AnalysisMetadata$FileFingerprint,
            StatusFingerprint = analysis@AnalysisMetadata$StatusFingerprint,
            Status = analysis@AnalysisMetadata$Status,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
)

#' Overwrite the status of a n2kAnalysisMetadata
#' @param x the n2kAnalysisMetadata object
#' @param value the new values for the status
#' @name status<-
#' @rdname status.change
#' @exportMethod status<-
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2kAnalysisMetadata_class.R
setGeneric(
  name = "status<-",
  def = function(x, value){
    standardGeneric("status<-") # nocov
  }
)

#' @rdname status.change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2kGlmerPoisson_class.R
setReplaceMethod(
  "status",
  "n2kGlmerPoisson",
  function(x, value){
    x@AnalysisMetadata$Status <- value
    x@AnalysisMetadata$StatusFingerprint <- sha1(
      list(
        x@AnalysisMetadata$FileFingerprint, x@AnalysisMetadata$Status,
        x@Model, x@AnalysisMetadata$AnalysisVersion,
        x@AnalysisVersion, x@RPackage, x@AnalysisVersionRPackage,
        x@AnalysisRelation
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)

#' @rdname status.change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2kInlaNbinomial_class.R
setReplaceMethod(
  "status",
  "n2kInlaNbinomial",
  function(x, value){
    x@AnalysisMetadata$Status <- value
    x@AnalysisMetadata$StatusFingerprint <- sha1(
      list(
        x@AnalysisMetadata$FileFingerprint, x@AnalysisMetadata$Status, x@Model,
        x@AnalysisMetadata$AnalysisVersion, x@AnalysisVersion, x@RPackage,
        x@AnalysisVersionRPackage, x@AnalysisRelation, x@RawImputed
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)

#' @rdname status.change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2kLrtGlmer_class.R
setReplaceMethod(
  "status",
  "n2kLrtGlmer",
  function(x, value){
    x@AnalysisMetadata$Status <- value
    if (is.null(x@Model)) {
      model <- NULL
    } else {
      model <- x@Model@frame
    }
    if (is.null(x@Model0)) {
      model0 <- NULL
    } else {
      model0 <- x@Model0@frame
    }
    x@AnalysisMetadata$StatusFingerprint <- sha1(
      list(
        x@AnalysisMetadata$FileFingerprint, x@AnalysisMetadata$Status,
        x@Model, x@Model0, x@Anova,
        x@AnalysisMetadata$AnalysisVersion,
        x@AnalysisVersion, x@RPackage, x@AnalysisVersionRPackage,
        x@AnalysisRelation
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)

#' @rdname status.change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2kComposite_class.R
setReplaceMethod(
  "status",
  "n2kComposite",
  function(x, value){
    x@AnalysisMetadata$Status <- value
    x@AnalysisMetadata$StatusFingerprint <- sha1(
      list(
        x@AnalysisMetadata$FileFingerprint, x@AnalysisMetadata$Status,
        x@Parameter, x@Index, x@AnalysisMetadata$AnalysisVersion,
        x@AnalysisVersion, x@RPackage, x@AnalysisVersionRPackage,
        x@AnalysisRelation
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)

#' @rdname status.change
#' @importFrom methods setReplaceMethod
#' @importFrom digest sha1
#' @include n2kInlaComparison_class.R
setReplaceMethod(
  "status",
  "n2kInlaComparison",
  function(x, value){
    x@AnalysisMetadata$Status <- value
    x@AnalysisMetadata$StatusFingerprint <- sha1(
      list(
        x@AnalysisMetadata$FileFingerprint, x@AnalysisMetadata$Status,
        x@Models, x@WAIC, x@AnalysisMetadata$AnalysisVersion,
        x@AnalysisVersion, x@RPackage, x@AnalysisVersionRPackage,
        x@AnalysisRelation
      ),
      digits = 6L
    )
    validObject(x)
    return(x)
  }
)
