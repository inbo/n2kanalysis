#' @importFrom methods setClassUnion
#' @include import_S3_classes.R
setClassUnion("maybeInla", c("inla", "NULL"))
setClassUnion("maybeMatrix", c("matrix", "NULL"))

#' The n2kInlaNBinomial class
#'
#' It hold analysis data based on a INLA negative binomial model
#' @section Slots:
#'   \describe{
#'    \item{\code{Data}}{a data.frame with the data}
#'    \item{\code{LinearCombination}}{an optional matrix with the linear
#'        combinations}
#'    \item{\code{Model}}{Either NULL or the resulting INLA model.}
#'   }
#' @name n2kInlaNbinomial-class
#' @rdname n2kInlaNbinomial-class
#' @exportClass n2kInlaNbinomial
#' @aliases n2kInlaNbinomial-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R
setClass(
  "n2kInlaNbinomial",
  representation = representation(
    Data = "data.frame",
    LinearCombination = "maybeMatrix",
    Model = "maybeInla"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_dataframe_variable
setValidity(
  "n2kInlaNbinomial",
  function(object){
    check_dataframe_variable(
      df = object@Data[1, ],
      variable = c(
        all.vars(object@AnalysisFormula[[1]]),
        "DatasourceID", "ObservationID"
      ),
      error = TRUE
    )
    if (!grepl("^inla nbinomial", object@AnalysisMetadata$ModelType)) {
      stop("ModelType should be 'inla nbinomial'")
    }
    if (class(object@Model) == "inla") {
      if (object@Model$all.hyper$family[[1]]$label != "nbinomial") {
        stop("The model must be from the nbinomial family")
      }
    }

    file.fingerprint <- get_sha1(
      list(
        object@Data, object@AnalysisMetadata$SchemeID,
        object@AnalysisMetadata$SpeciesGroupID,
        object@AnalysisMetadata$LocationGroupID,
        object@AnalysisMetadata$ModelType, object@AnalysisMetadata$Covariate,
        object@AnalysisMetadata$FirstImportedYear,
        object@AnalysisMetadata$LastImportedYear,
        object@AnalysisMetadata$Duration,
        object@AnalysisMetadata$LastAnalysedYear,
        object@AnalysisMetadata$AnalysisDate, object@AnalysisMetadata$Seed,
        object@AnalysisRelation$ParentAnalysis,
        object@LinearCombination
      )
    )
    if (object@AnalysisMetadata$FileFingerprint != file.fingerprint) {
      stop("Corrupt FileFingerprint")
    }

    status.fingerprint <- get_sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        object@Model, object@AnalysisMetadata$AnalysisVersion,
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
