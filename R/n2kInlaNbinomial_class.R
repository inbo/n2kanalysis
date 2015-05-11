#' @importFrom methods setOldClass
setOldClass("inla")

#' @importFrom methods setClassUnion
setClassUnion("maybeInla", c("inla", "NULL"))

#' The n2kInlaNBinomial class
#' 
#' It hold analysis data based on a INLA negative binomial model
#' @section Slots:
#'   \describe{
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
    Model = "maybeInla"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_single_character check_dataframe_variable
#' @importFrom digest digest
setValidity(
  "n2kInlaNbinomial",
  function(object){
    if(!grepl("^inla nbinomial", object@ModelType)){
      stop("ModelType should be 'inla nbinomial'")
    }
    if(class(object@Model) == "inla"){
      if(object@Model$all.hyper$family[[1]]$label != "nbinomial"){
        stop("The model must be from the nbinomial family")
      }
    }
    
    file.fingerprint <- digest(
      list(
        object@Data, object@SchemeID, object@SpeciesGroupID, object@LocationGroupID, 
        object@ModelType, object@Covariate, object@AnalysisDate, object@Seed
      ),
      algo = "sha1"
    )
    if(object@FileFingerprint != file.fingerprint){
      stop("Corrupt FileFingerprint")
    }

    return(TRUE)
  }
)
