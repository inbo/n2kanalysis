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
    standardGeneric("session_package") # nocov
  }
)

#' @importFrom assertthat has_name
package_version <- function(x){
  if (has_name(x, "Repository")) {
    return(
      data.frame(
        Description = x$Package,
        Version = x$Version,
        Origin = "CRAN",
        stringsAsFactors = FALSE
      )
    )
  }
  if (has_name(x, "RemoteType")) {
    if (x$RemoteType != "github") {
      stop("Only github remotes are currently handled")
    }
    return(
      data.frame(
        Description = x$Package,
        Version = x$Version,
        Origin = sprintf(
          "Github: %s/%s@%s", x$GithubUsername, x$GithubRepo, x$GithubSHA1
        ),
        stringsAsFactors = FALSE
      )
    )
  }
  data.frame(
    Description = x$Package,
    Version = x$Version,
    Origin = "local",
    stringsAsFactors = FALSE
  )
}

#' @rdname session_package
#' @aliases session_package,sessionInfo-methods
#' @importFrom methods setMethod new
#' @importFrom digest sha1
#' @importFrom utils sessionInfo
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
      Origin = "CRAN",
      stringsAsFactors = FALSE
    )
    if ("otherPkgs" %in% names(session)) {
      package <- rbind(
        package,
        do.call(rbind, lapply(session$otherPkgs, package_version))
      )
    }
    if ("loadedOnly" %in% names(session)) {
      package <- rbind(
        package,
        do.call(rbind, lapply(session$loadedOnly, package_version))
      )
    }
    rownames(package) <- NULL
    package <- package[order(package$Description), ]
    package$Fingerprint <- apply(package, 1, sha1)
    attr(package, "AnalysisVersion") <- sha1(package)
    return(package)
  }
)
