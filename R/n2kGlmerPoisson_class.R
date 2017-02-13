#' @importFrom methods setClassUnion
#' @importClassesFrom lme4 glmerMod
setClassUnion("maybeGlmerMod", c("glmerMod", "NULL"))

#' The n2kGlmerPoisson class
#'
#' It hold analysis data based on a glmer poisson model
#' @section Slots:
#'   \describe{
#'    \item{\code{Data}}{a data.frame with the data}
#'    \item{\code{Model}}{Either NULL or the resulting glmer model.}
#'   }
#' @name n2kGlmerPoisson-class
#' @rdname n2kGlmerPoisson-class
#' @exportClass n2kGlmerPoisson
#' @aliases n2kGlmerPoisson-class
#' @importFrom methods setClass
#' @importFrom digest sha1
#' @docType class
#' @include n2kModel_class.R
setClass(
  "n2kGlmerPoisson",
  representation = representation(
    Data = "data.frame",
    Model = "maybeGlmerMod"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_dataframe_variable
setValidity(
  "n2kGlmerPoisson",
  function(object){
    check_dataframe_variable(
      df = object@Data[1, ],
      variable = c(
        all.vars(object@AnalysisFormula[[1]]), "ObservationID", "DatasourceID"
      ),
      error = TRUE
    )
    if (anyDuplicated(object@Data$ObservationID)) {
      stop("Duplicated ObservationID")
    }

    if (!grepl("glmer poisson", object@AnalysisMetadata$ModelType)) {
      stop("ModelType should be 'glmer poisson'")
    }
    if (grepl("^weighted glmer poisson", object@AnalysisMetadata$ModelType)) {
      check_dataframe_variable(
        df = object@Data,
        variable = "Weight",
        name = "data"
      )
    }
    if (class(object@Model) == "glmerMod") {
      if (object@Model@resp$family$family != "poisson") {
        stop("The model must be from the poisson family")
      }
    }

    file.fingerprint <- sha1(
      list(
        object@Data,
        object@AnalysisMetadata$ResultDatasourceID,
        object@AnalysisMetadata$SchemeID,
        object@AnalysisMetadata$SpeciesGroupID,
        object@AnalysisMetadata$LocationGroupID,
        object@AnalysisMetadata$ModelType, object@AnalysisMetadata$Formula,
        object@AnalysisMetadata$FirstImportedYear,
        object@AnalysisMetadata$LastImportedYear,
        object@AnalysisMetadata$Duration,
        object@AnalysisMetadata$LastAnalysedYear,
        object@AnalysisMetadata$AnalysisDate, object@AnalysisMetadata$Seed,
        object@AnalysisRelation$ParentAnalysis
      )
    )
    if (object@AnalysisMetadata$FileFingerprint != file.fingerprint) {
      stop("Corrupt FileFingerprint")
    }

    status.fingerprint <- sha1(
      list(
        object@AnalysisMetadata$FileFingerprint, object@AnalysisMetadata$Status,
        object@Model, object@AnalysisMetadata$AnalysisVersion,
        object@AnalysisVersion, object@RPackage, object@AnalysisVersionRPackage,
        object@AnalysisRelation
      ),
      digits = 6L
    )
    if (object@AnalysisMetadata$StatusFingerprint != status.fingerprint) {
      stop("Corrupt StatusFingerprint")
    }

    return(TRUE)
  }
)
