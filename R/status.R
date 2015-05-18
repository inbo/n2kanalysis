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
    standard.generic("status")
  }
)

#' @rdname status
#' @aliases status,n2kModel-methods
#' @importFrom methods setMethod
#' @include n2kModel_class.R
setMethod(
  f = "status",
  signature = signature(x = "n2kModel"),
  definition = function(x){
    return(x@Status)
  }
)

#' @rdname status
#' @aliases status,n2kModel-methods
#' @importFrom methods setMethod
#' @importFrom n2khelper check_path
setMethod(
  f = "status",
  signature = signature(x = "character"),
  definition = function(x){
    path <- check_path(x, type = "directory")
    files <- list.files(path = path, pattern = "\\.rda$", full.names = TRUE)
    return(
      do.call(
        rbind, 
        lapply(files, function(file){
          local.environment <- new.env()
          load(file, envir = local.environment)
          analysis <- read_object_environment(object = "analysis", env = local.environment)
          cbind(
            Filename = file,
            SchemeID = get_scheme_id(analysis),
            SpeciesGroupID = get_species_group_id(analysis),
            LocationGroupID = get_location_group_id(analysis),
            get_model_set(analysis),
            AnalysisDate = get_analysis_date(analysis),
            Status = status(analysis),
            FileFingerprint = get_file_fingerprint(analysis),
            StatusFingerprint = get_status_fingerprint(analysis)
          )
        })
      )
    )
  }
)

#' Overwrite the status of a n2kModel
#' @param x the n2kModel object
#' @param value the new values for the status
#' @name status<-
#' @rdname status.change
#' @exportMethod status<-
#' @docType methods
#' @importFrom methods setGeneric
#' @include n2kModel_class.R
setGeneric(
  name = "status<-", 
  def = function(x, value){
    standard.generic("status<-")
  }
)

#' @rdname status.change
#' @importFrom methods setReplaceMethod
#' @importFrom digest digest
#' @include n2kGlmerPoisson_class.R
setReplaceMethod(
  "status",
  "n2kGlmerPoisson",
  function(x, value){
    x@Status <- value
    x@StatusFingerprint <- digest(
      list(
        x@FileFingerprint, x@Status, x@Model, x@SessionInfo
      ),
      algo = "sha1"
    )
    validObject(x)
    return(x)
  }
)

#' @rdname status.change
#' @importFrom methods setReplaceMethod
#' @importFrom digest digest
#' @include n2kInlaNbinomial_class.R
setReplaceMethod(
  "status",
  "n2kInlaNbinomial",
  function(x, value){
    x@Status <- value
    x@StatusFingerprint <- digest(
      list(
        x@FileFingerprint, x@Status, x@Model, x@SessionInfo
      ),
      algo = "sha1"
    )
    validObject(x)
    return(x)
  }
)
setReplaceMethod(
  "status",
  "n2kModel",
  function(x, value){
    x@Status <- value
    x@StatusFingerprint <- digest(
      list(
        x@FileFingerprint, value, x@Model, x@SessionInfo
      ),
      algo = "sha1"
    )
    validObject(x)
    return(x)
  }
)
