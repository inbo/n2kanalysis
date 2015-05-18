#' @importFrom methods setOldClass
setOldClass("sessionInfo")

#' The n2kModel class
#' 
#' A virtual superclass to contain the analysis models
#' @section Slots:
#'   \describe{
#'    \item{\code{Status}}{a single character indicating the status of the model}
#'    \item{\code{SchemeID}}{a single integer holding the id of the scheme}
#'    \item{\code{SpeciesGroupID}}{a single integer identifing the species group}
#'    \item{\code{LocationGroupID}}{a single integer identifing the location group}
#'    \item{\code{ModelType}}{a single character identifying the type of model to fit to the data}
#'    \item{\code{Covariate}}{a single character holding the right hand side of the model formula}
#'    \item{\code{FirstImportedYear}}{Oldest year considered in the data}
#'    \item{\code{LastImportedYear}}{Most recent year considered in the data}
#'    \item{\code{Duration}}{The width of the moving window}
#'    \item{\code{LastAnalysedYear}}{Most recent year in the window}
#'    \item{\code{AnalysisDate}}{A POSIXct date indicating the date that the dataset was imported}
#'    \item{\code{Seed}}{a single integer uses as a seed for all calculations}
#'    \item{\code{FileFingerprint}}{the SHA1 fingerprint of the analysis}
#'    \item{\code{StatusFingerprint}}{the SHA1 fingerprint of the status of the analysis}
#'    \item{\code{SessionInfo}}{The sessionInfo() output}
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
    Status = "character",
    SchemeID = "integer",
    SpeciesGroupID = "integer",
    LocationGroupID = "integer",
    ModelType = "character",
    Covariate = "character",
    FirstImportedYear = "integer",
    LastImportedYear = "integer",
    Duration = "integer",
    LastAnalysedYear = "integer",
    AnalysisDate = "POSIXct",
    Seed = "integer",
    FileFingerprint = "character",
    StatusFingerprint = "character",
    SessionInfo = "sessionInfo",
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
    check_single_strictly_positive_integer(
      object@LastImportedYear, 
      name = "LastImportedYear"
    )
    check_single_strictly_positive_integer(object@Seed, name = "Seed")
    check_single_character(object@Status, name = "Status")
    check_single_character(object@ModelType, name = "ModelType")
    check_single_character(object@Covariate, name = "Covariate")
    check_single_posix(object@AnalysisDate, name = "AnalysisDate", past = TRUE)
    check_single_character(object@FileFingerprint, name = "FileFingerprint")
    check_single_character(object@StatusFingerprint, name = "StatusFingerprint")
    
    required.names <- c("R.version", "platform", "locale", "running", "basePkgs", "otherPkgs", "loadedOnly")
    if(!all(required.names %in% names(object@SessionInfo))){
      missing.names <- required.names[!required.names %in% names(object@SessionInfo)]
      stop(
        "SessionInfo is missing following elements: ", 
        paste(missing.names, collapse = ", ")
      )
    }
    
    ok.status <- c("new", "working", "waiting", "error", "converged", "false convergence", "insufficient data")
    if(!object@Status %in% ok.status){
      stop(
        "Status must be one of the following: ", 
        paste0("'", ok.status, "'", collapse = ", ")
      )
    }
    if(object@FirstImportedYear > as.integer(format(Sys.time(), "%Y"))){
      stop("Importing data from the future?")
    }
    if(object@LastImportedYear > as.integer(format(Sys.time(), "%Y"))){
      stop("Importing data from the future?")
    }
    if(object@LastImportedYear < object@FirstImportedYear){
      stop("LastImportedYear cannot proceed FirstImportedYear")
    }
    check_single_strictly_positive_integer(object@Duration, name = "Duration")
    if(object@Duration > object@LastImportedYear - object@FirstImportedYear + 1){
      stop("Duration longer than the interval from FirstImportedYear to LastImportedYear")
    }
    
    check_single_strictly_positive_integer(
      object@LastAnalysedYear, 
      name = "LastAnalysedYear"
    )
    if(object@LastAnalysedYear > object@LastImportedYear){
      stop("LastAnalysedYear larger than LastImportedYear. Window outside imported range.")
    }
    if(object@LastAnalysedYear < object@FirstImportedYear + object@Duration - 1){
      stop("LastAnalysedYear smaller than FirstImportedYear + Duration - 1. Window outside imported range.")
    }
    
    return(TRUE)
  }
)
