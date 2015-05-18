#' @importFrom methods setClassUnion
#' @importClassesFrom lme4 glmerMod
setClassUnion("maybeGlmerMod", c("glmerMod", "NULL"))

#' The n2kGlmerPoisson class
#' 
#' It hold analysis data based on a glmer poisson model
#' @section Slots:
#'   \describe{
#'    \item{\code{Data}}{a data.frame with the data}
#'    \item{\code{Weight}}{The name of the variable to use as weights. '' indicates no weighting.}
#'    \item{\code{Model}}{Either NULL or the resulting glmer model.}
#'   }
#' @name n2kGlmerPoisson-class
#' @rdname n2kGlmerPoisson-class
#' @exportClass n2kGlmerPoisson
#' @aliases n2kGlmerPoisson-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kModel_class.R
setClass(
  "n2kGlmerPoisson",
  representation = representation(
    Data = "data.frame",
    Weight = "character",
    Model = "maybeGlmerMod"
  ),
  contains = "n2kModel"
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_single_character check_dataframe_variable
#' @importFrom digest digest
setValidity(
  "n2kGlmerPoisson",
  function(object){
    check_dataframe_covariate(df = object@Data, covariate = object@Covariate)
    check_single_character(object@Weight, name = "Weight")
    if(object@Weight == ''){
      if(!grepl("^glmer poisson", object@ModelType)){
        stop("ModelType should be 'glmer poisson'")
      }
    } else {
      check_dataframe_variable(
        df = object@Data,
        variable = object@Weight,
        name = "data"
      )
      if(!grepl("^weighted glmer poisson", object@ModelType)){
        stop("ModelType should be 'weighted glmer poisson'")
      }
    }
    if(class(object@Model) == "glmerMod"){
      if(object@Model@resp$family$family != "poisson"){
        stop("The model must be from the poisson family")
      }
    }
    
    file.fingerprint <- digest(
      list(
        object@Data, object@SchemeID, object@SpeciesGroupID, object@LocationGroupID, 
        object@ModelType, object@Covariate, object@FirstImportedYear, object@LastImportedYear,
        object@Duration, object@LastAnalysedYear, object@AnalysisDate, object@Seed, 
        object@Weight
      ),
      algo = "sha1"
    )
    if(object@FileFingerprint != file.fingerprint){
      stop("Corrupt FileFingerprint")
    }

    return(TRUE)
  }
)
