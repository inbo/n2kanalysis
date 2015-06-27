#' Make a union of AnalysisVersions
#' @param ... the n2kAnalysisVersions to union
#' @export
union <- function(...){
  dots <- list(...)
  r.package <- do.call(rbind, lapply(dots, function(x){x@RPackage}))
  r.package <- unique(r.package)
  r.package <- r.package[order(r.package$Description, r.package$Version), ]
  rownames(r.package) <- NULL
  analysis.version <- get_sha1(r.package)
  analysis.version.r.package <- data.frame(
    AnalysisVersion = analysis.version,
    RPackage = r.package$Fingerprint
  )
  output <- combine(
    ...,
    new(
      "n2kAnalysisVersion",
      AnalysisVersion = data.frame(Fingerprint = analysis.version),
      RPackage = r.package,
      AnalysisVersionRPackage = analysis.version.r.package
    )
  )
  return(list(Union = output, UnionFingerprint = analysis.version))
}
