#' The n2kAnalysisVersion class
#' @name n2kAnalysisVersion-class
#' @rdname n2kAnalysisVersion-class
#' @exportClass n2kAnalysisVersion
#' @aliases n2kAnalysisVersion-class
#' @importFrom methods setClass
#' @docType class
setClass(
  "n2kAnalysisVersion",
  representation = representation(
    AnalysisVersion = "data.frame",
    RPackage = "data.frame",
    AnalysisVersionRPackage = "data.frame"
  ),
  prototype = prototype(
    AnalysisVersion = data.frame(
      Fingerprint = character(0),
      stringsAsFactors = FALSE
    ),
    RPackage = data.frame(
      Fingerprint = character(0),
      Description = character(0),
      Version = character(0),
      stringsAsFactors = FALSE
    ),
    AnalysisVersionRPackage = data.frame(
      AnalysisVersion = character(0),
      RPackage = character(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
setValidity(
  "n2kAnalysisVersion",
  function(object){
    check_dataframe_variable(
      df = object@AnalysisVersion, 
      variable = "Fingerprint",
      name = "AnalysisVersion"
    )
    check_dataframe_variable(
      df = object@RPackage, 
      variable = c("Fingerprint", "Description", "Version"),
      name = "AnalysisVersionRPackage"
    )
    check_dataframe_variable(
      df = object@AnalysisVersionRPackage, 
      variable = c("AnalysisVersion", "RPackage"),
      name = "AnalysisVersionRPackage"
    )
    
    if(!all(object@AnalysisVersionRPackage$AnalysisVersion %in% object@AnalysisVersion$Fingerprint)){
      stop("Some AnalysisVersion in 'AnalysisVersionRPackage' slot are not present in 'AnalysisVersion' slot")
    }
    if(!all(object@AnalysisVersionRPackage$RPackage %in% object@RPackage$Fingerprint)){
      stop("Some AnalysisVersion in 'AnalysisVersionRPackage' slot are not present in 'AnalysisVersion' slot")
    }
    if(anyDuplicated(object@AnalysisVersionRPackage)){
      stop("Duplicated rows in 'AnalysisVersionRPackage' slot")
    }
    if(anyDuplicated(object@AnalysisVersion)){
      stop("Duplicated rows in 'AnalysisVersion' slot")
    }
    if(anyDuplicated(object@RPackage)){
      stop("Duplicated rows in 'RPackage' slot")
    }
    return(TRUE)
  }
)
