#' Make a union of `AnalysisVersions`
#' @param ... the `n2kAnalysisVersions` to union
#' @importFrom digest sha1
#' @export
union <- function(...) {
  dots <- list(...)
  r_package <- do.call(
    rbind,
    lapply(
      dots,
      function(x) {
        x@RPackage
      }
    )
  )
  r_package <- unique(r_package)
  r_package <- r_package[order(r_package$description, r_package$version), ]
  rownames(r_package) <- NULL
  analysis_version <- sha1(r_package)
  analysis_version_r_package <- data.frame(
    analysis_version = analysis_version, r_package = r_package$fingerprint
  )
  output <- combine(
    ...,
    new(
      "n2kAnalysisVersion",
      AnalysisVersion = data.frame(fingerprint = analysis_version),
      RPackage = r_package,
      AnalysisVersionRPackage = analysis_version_r_package
    )
  )
  return(list(Union = output, Unionfingerprint = analysis_version))
}
