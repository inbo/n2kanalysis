#' Make a union of AnalysisVersions
#' @param ... the n2kAnalysisVersions to union
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
  r_package <- r_package[order(r_package$Description, r_package$Version), ]
  rownames(r_package) <- NULL
  analysis_version <- sha1(r_package)
  analysis_version_r_package <- data.frame(
    AnalysisVersion = analysis_version,
    RPackage = r_package$Fingerprint
  )
  output <- combine(
    ...,
    new(
      "n2kAnalysisVersion",
      AnalysisVersion = data.frame(Fingerprint = analysis_version),
      RPackage = r_package,
      AnalysisVersionRPackage = analysis_version_r_package
    )
  )
  return(list(Union = output, UnionFingerprint = analysis_version))
}
