#' Convert a sessionInfo() to a data.frame of packages
#' @param session The output of sessionInfo()
#' @return a data.frame with the packages of a sessionInfo()
#' @name session_package
#' @rdname session_package
#' @exportMethod session_package
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "session_package", 
  def = function(session){
    standard.generic("session_package")
  }
)

#' @rdname session_package
#' @aliases session_package,sessionInfo-methods
#' @importFrom methods setMethod
#' @importFrom digest digest
setMethod(
  f = "session_package",
  signature = signature(session = "sessionInfo"),
  definition = function(session){
    package <- data.frame(
      Description = c(session$running, "R"),
      Version = c(
        session$R.version$platform, 
        paste(
          session$R.version[c("major", "minor")], 
          collapse = "."
        )
      ),
      stringsAsFactors = FALSE
    )
    if("otherPkgs" %in% names(session)){
      package <- rbind(
        package,
        do.call(rbind, lapply(session$otherPkgs, function(i){
          data.frame(
            Description = i$Package,
            Version = i$Version,
            stringsAsFactors = FALSE
          )
        }))
      )
    }
    if("loadedOnly" %in% names(session)){
      package <- rbind(
        package,
        do.call(rbind, lapply(session$loadedOnly, function(i){
          data.frame(
            Description = i$Package,
            Version = i$Version,
            stringsAsFactors = FALSE
          )
        }))
      )
    }
    rownames(package) <- NULL
    package <- package[order(package$Description), ]
    package$Fingerprint <- apply(package, 1, digest, algo = "sha1")
    attr(package, "AnalysisVersion") <- digest(package, algo = "sha1")
    return(package)
  }
)

#' @rdname session_package
#' @aliases session_package,n2kModel-methods
#' @importFrom methods setMethod
#' @include n2kModel_class.R
setMethod(
  f = "session_package",
  signature = signature(session = "n2kModel"),
  definition = function(session){
    session_package(get_session_info(session))
  }
)
