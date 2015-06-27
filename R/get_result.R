#' Add the results from an analysis
#' @param x object with the current results
#' @param ... further arguments (see Details)
#' @name get_result
#' @rdname get_result
#' @exportMethod get_result
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_result", 
  def = function(x, ...){
    standard.generic("get_result")
  }
)

#' @rdname get_result
#' @aliases get_result,n2kModel-methods
#' @importFrom methods setMethod
#' @include n2kAnalysisMetadata_class.R
#' @include n2kResult_class.R
#' @include n2kModel_class.R
setMethod(
  f = "get_result",
  signature = signature(x = "n2kModel"),
  definition = function(x, ...){
    validObject(x)
    anomaly <- get_anomaly(analysis = x, ...)
    new(
      "n2kResult",
      AnalysisMetadata = x@AnalysisMetadata,
      AnalysisFormula = lapply(x@AnalysisMetadata$Formula, as.formula),
      AnalysisRelation = x@AnalysisRelation,
      AnalysisVersion = x@AnalysisVersion,
      RPackage = x@RPackage,
      AnalysisVersionRPackage = x@AnalysisVersionRPackage,
      Parameter = anomaly@Parameter,
      ParameterEstimate = anomaly@ParameterEstimate,
      AnomalyType = anomaly@AnomalyType,
      Anomaly = anomaly@Anomaly
    )
  }
)

#' @rdname get_result
#' @importFrom methods setMethod validObject
#' @importFrom n2khelper check_single_character check_single_logical
#' @param keep.fingerprint Keep the character fingerprints? Otherwise change them into integers
#' @param n.cluster the number of clusters to run this function in parallel. Defaults to 1 (= no parallel computing).
setMethod(
  f = "get_result",
  signature = signature(x = "character"),
  definition = function(x, keep.fingerprint = TRUE, n.cluster = 1, ...){
    # check arguments
    x <- check_single_character(x)
    keep.fingerprint <- check_single_logical(keep.fingerprint, name = "keep.fingerprint")
    n.cluster <- check_single_strictly_positive_integer(n.cluster, name = "n.cluster")

    # x is an existing file
    if (file_test("-f", x)) {
      message(x)
      local.environment <- new.env()
      load(x, envir = local.environment)
      analysis <- read_object_environment(object = "analysis", env = local.environment)
      return(get_result(x = analysis, ...))
    }
  
    if (!file_test("-d", x)) {
      stop("'x' is neither an existing file, neither an existing directory")
    }
    
    # x is an existing directory
    x <- normalizePath(x, winslash = "/", mustWork = TRUE)
    files <- list.files(path = x, pattern = "\\.rda$", full.names = TRUE)
    if (n.cluster == 1) {
      result <- lapply(files, get_result, ...)
    } else {
      if (requireNamespace("parallel", quietly = TRUE)) {
        available.cluster <- parallel::detectCores()
        if (n.cluster > available.cluster) {
          message("Requesting ", n.cluster, " clusters but only ", available.cluster, " available.")
          n.cluster <- available.cluster
        }
        message("Reading results in parallel on ", n.cluster, " clusters")
        utils::flush.console()
        cl <- parallel::makeCluster(n.cluster)
        result <- parallel::clusterApplyLB(cl = cl, x = files, fun = get_result, ...)
        parallel::stopCluster(cl)
      } else {
        message("Cannot load the parallel package. Falling back to non-parallel computing.")
        utils::flush.console()
        result <- lapply(files, get_result, ...)
      }
    }
    
    message("Combining results")
    utils::flush.console()
    result <- do.call(combine, result)
    
    if (keep.fingerprint) {
      return(result)
    }
    
    message("Converting sha1 to integer")
    utils::flush.console()
    # convert analysis fingerprint from sha1 to factor
    analysis.level <- c(
      result@AnalysisMetadata$FileFingerprint,
      result@AnalysisRelation$ParentAnalysis
    )
    analysis.level <- sort(unique(analysis.level))
    
    result@AnalysisRelation$Analysis <- factor(
      result@AnalysisRelation$Analysis,
      levels = analysis.level
    )
    result@AnalysisRelation$ParentAnalysis <- factor(
      result@AnalysisRelation$ParentAnalysis,
      levels = analysis.level
    )
    result@ParameterEstimate$Analysis <- factor(
      result@ParameterEstimate$Analysis,
      levels = analysis.level
    )
    result@Anomaly$Analysis <- factor(
      result@Anomaly$Analysis,
      levels = analysis.level
    )
    result@AnalysisMetadata$FileFingerprint <- factor(
      result@AnalysisMetadata$FileFingerprint,
      levels = analysis.level
    )
    
    # convert parameter fingerprint from sha1 to integer
    result@Anomaly$Parameter <- as.integer(factor(
      result@Anomaly$Parameter,
      levels = result@Parameter$Fingerprint
    ))
    result@ParameterEstimate$Parameter <- as.integer(factor(
      result@ParameterEstimate$Parameter,
      levels = result@Parameter$Fingerprint
    ))
    result@Parameter$Parent <- as.integer(factor(
      result@Parameter$Parent,
      levels = result@Parameter$Fingerprint
    ))
    result@Parameter$Fingerprint <- as.integer(factor(
      result@Parameter$Fingerprint,
      levels = result@Parameter$Fingerprint
    ))

    # convert anomaly type fingerprint from sha1 to integer
    result@Anomaly$AnomalyType <- as.integer(factor(
      result@Anomaly$AnomalyType,
      levels = result@AnomalyType$Fingerprint
    ))
    result@AnomalyType$Fingerprint <- as.integer(factor(
      result@AnomalyType$Fingerprint,
      levels = result@AnomalyType$Fingerprint
    ))
    
    # convert R package fingerprint from sha1 to integer
    result@AnalysisVersionRPackage$RPackage <- as.integer(factor(
      result@AnalysisVersionRPackage$RPackage,
      levels = result@RPackage$Fingerprint
    ))
    result@RPackage$Fingerprint <- as.integer(factor(
      result@RPackage$Fingerprint,
      levels = result@RPackage$Fingerprint
    ))

    # convert analysis version fingerprint to factor
    result@AnalysisMetadata$AnalysisVersion <- factor(
      result@AnalysisMetadata$AnalysisVersion,
      levels = result@AnalysisVersion$Fingerprint
    )
    result@AnalysisVersionRPackage$AnalysisVersion <- factor(
      result@AnalysisVersionRPackage$AnalysisVersion,
      levels = result@AnalysisVersion$Fingerprint
    )
    result@AnalysisVersion$Fingerprint <- factor(
      result@AnalysisVersion$Fingerprint,
      levels = result@AnalysisVersion$Fingerprint
    )
    
    # convert observationID to factor
    result@AnalysisRelation$ParentStatusFingerprint <- factor(
      result@AnalysisRelation$ParentStatusFingerprint
    )
    result@AnalysisRelation$ParentStatus <- factor(
      result@AnalysisRelation$ParentStatus
    )
    result@AnalysisMetadata$Status <- factor(
      result@AnalysisMetadata$Status
    )
    result@AnalysisMetadata$Formula <- factor(
      result@AnalysisMetadata$Formula
    )
    result@AnalysisMetadata$ModelType <- factor(
      result@AnalysisMetadata$ModelType
    )
    result@Anomaly$Datafield <- factor(result@Anomaly$Datafield)
    
    validObject(result)
    return(result)
  }
)
