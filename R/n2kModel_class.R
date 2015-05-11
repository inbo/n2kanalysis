#' The n2kModel class
#' 
#' A virtual superclass to contain the analysis models
#' @section Slots:
#'   \describe{
#'    \item{\code{Data}}{a data.frame with the data}
#'    \item{\code{Status}}{a single character indicating the status of the model}
#'    \item{\code{SchemeID}}{a single integer holding the id of the scheme}
#'    \item{\code{SpeciesGroupID}}{a single integer identifing the species group}
#'    \item{\code{LocationGroupID}}{a single integer identifing the location group}
#'    \item{\code{ModelType}}{a single character identifying the type of model to fit to the data}
#'    \item{\code{Covariate}}{a single character holding the right hand side of the model formula}
#'    \item{\code{FirstImportedYear}}{Oldest year considered in the data}
#'    \item{\code{AnalysisDate}}{A POSIXct date indicating the date that the dataset was imported}
#'    \item{\code{Seed}}{a single integer uses as a seed for all calculations}
#'    \item{\code{DataFingerprint}}{the SHA1 fingerprint of the data}
#'    \item{\code{FileFingerprint}}{the SHA1 fingerprint of the data}
#'   }
#' @name n2kModel-class
#' @rdname n2kModel-class
#' @exportClass n2kModel
#' @aliases n2kModel-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "n2kModel",
  representation = representation(
    Data = "data.frame",
    Status = "character",
    SchemeID = "integer",
    SpeciesGroupID = "integer",
    LocationGroupID = "integer",
    ModelType = "character",
    Covariate = "character",
    FirstImportedYear = "integer",
    AnalysisDate = "POSIXct",
    Seed = "integer",
    DataFingerprint = "character",
    FileFingerprint = "character",
    "VIRTUAL"
  )
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_single_strictly_positive_integer check_single_character check_single_posix
#' @importFrom digest digest
setValidity(
  "n2kModel",
  function(object){
    check_single_strictly_positive_integer(object@SchemeID, name = "SchemeID")
    check_single_strictly_positive_integer(object@SpeciesGroupID, name = "SpeciesGroupID")
    check_single_strictly_positive_integer(object@LocationGroupID, name = "LocationGroupID")
    check_single_strictly_positive_integer(
      object@FirstImportedYear, 
      name = "FirstImportedYear"
    )
    check_single_strictly_positive_integer(object@Seed, name = "Seed")
    check_single_character(object@DataFingerprint, name = "DataFingerprint")
    check_single_character(object@Status, name = "Status")
    check_single_character(object@ModelType, name = "ModelType")
    check_single_character(object@Covariate, name = "Covariate")
    check_single_posix(object@AnalysisDate, name = "AnalysisDate", past = TRUE)
    
    ok.status <- c("new", "error", "converged", "false convergence", "insufficient data")
    if(!object@Status %in% ok.status){
      stop(
        "Status must be one of the following: ", 
        paste0("'", ok.status, "'", collapse = ", ")
      )
    }
    if(object@DataFingerprint != digest(object@Data, algo = "sha1")){
      stop("Mismatch between DataFingerprint and Data")
    }
    check_dataframe_covariate(df = object@Data, covariate = object@Covariate)
    if(object@FirstImportedYear > as.integer(format(Sys.time(), "%Y"))){
      stop("Importing data from the future?")
    }
    
    return(TRUE)
  }
)
