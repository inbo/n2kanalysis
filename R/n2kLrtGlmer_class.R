#' @importFrom methods setClassUnion
#' @include import_S3_classes.R
setClassUnion("maybeAnova", c("anova", "NULL"))

#' The n2kLrtGlmer class
#'
#' Calculate composite indices from multiple analysis
#' @section Slots:
#'   \describe{
#'    \item{\code{Parent0}}{the file fingerprint of the NULL model}
#'    \item{\code{Model}}{the alternative model}
#'    \item{\code{Model0}}{the NULL model}
#'    \item{\code{Anova}}{the anova table}
#'   }
#' @name n2kLrtGlmer-class
#' @rdname n2kLrtGlmer-class
#' @exportClass n2kLrtGlmer
#' @aliases n2kLrtGlmer-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R n2kGlmerPoisson_class.R
setClass(
  "n2kLrtGlmer",
  representation = representation(
    Parent0 = "character",
    Model = "maybeGlmerMod",
    Model0 = "maybeGlmerMod",
    Anova = "maybeAnova"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom assertthat assert_that is.string
setValidity(
  "n2kLrtGlmer",
  function(object){
    assert_that(is.string(object@Parent0))

    if (nrow(object@AnalysisRelation) != 2) {
      stop("'AnalysisRelation' slot must have exactly 2 rows")
    }
    if (anyNA(object@AnalysisRelation$ParentAnalysis)) {
      stop("'ParentAnalysis' in 'AnalysisRelation' slot cannot be missing")
    }
    if (!object@Parent0 %in% object@AnalysisRelation$ParentAnalysis) {
      stop("'Parent0' is not available in 'AnalysisRelation' slot")
    }

    file.fingerprint <- get_sha1(
      list(
        object@AnalysisMetadata$SchemeID,
        object@AnalysisMetadata$SpeciesGroupID,
        object@AnalysisMetadata$LocationGroupID,
        object@AnalysisMetadata$ModelType, object@AnalysisMetadata$Formula,
        object@AnalysisMetadata$FirstImportedYear,
        object@AnalysisMetadata$LastImportedYear,
        object@AnalysisMetadata$Duration,
        object@AnalysisMetadata$LastAnalysedYear,
        object@AnalysisMetadata$AnalysisDate, object@AnalysisMetadata$Seed,
        object@Parent0, object@AnalysisRelation$ParentAnalysis
      )
    )
    if (object@AnalysisMetadata$FileFingerprint != file.fingerprint) {
      stop("Corrupt FileFingerprint")
    }

    status.fingerprint <- get_sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        coef(object@Model), coef(object@Model0), object@Anova,
        object@AnalysisMetadata$AnalysisVersion,
        object@AnalysisVersion, object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation
      )
    )
    if (object@AnalysisMetadata$StatusFingerprint != status.fingerprint) {
      stop("Corrupt StatusFingerprint")
    }

    return(TRUE)
  }
)
