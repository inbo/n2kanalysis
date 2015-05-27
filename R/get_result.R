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
    message("reading metadata", appendLF = FALSE)
    utils::flush.console()
    version <- get_analysis_version(version = x)
    analysis.metadata <- cbind(
      LocationGroupID = get_location_group_id(x),
      SpeciesGroupID = get_species_group_id(x),
      get_model_set(x),
      Fingerprint = get_file_fingerprint(x),
      AnalysisVersion = version@AnalysisVersion$Fingerprint,
      Status = x@Status
    )
    message(", model parameters", appendLF = FALSE)
    utils::flush.console()
    parameter <- get_model_parameter(analysis = x)
    message(", anomaly")
    utils::flush.console()
    anomaly <- get_anomaly(analysis = x)
    new(
      "n2kResult",
      AnalysisMetadata = new("n2kAnalysisMetadata", analysis.metadata),
      AnalysisVersion = version@AnalysisVersion,
      RPackage = version@RPackage,
      AnalysisVersionRPackage = version@AnalysisVersionRPackage,
      Parameter = parameter@Parameter,
      ParameterEstimate = parameter@ParameterEstimate,
      AnomalyType = anomaly@AnomalyType,
      Anomaly = anomaly@Anomaly
    )
  }
)

#' @rdname get_result
#' @importFrom methods setMethod validObject
#' @importFrom n2khelper check_single_character check_single_logical
setMethod(
  f = "get_result",
  signature = signature(x = "character"),
  definition = function(x, ...){
    # check arguments
    x <- check_single_character(x)
    dots <- list(...)
    if(is.null(dots$keep.fingerprint)){
      dots$keep.fingerprint <- TRUE
    } else {
      dots$keep.fingerprint <- check_single_logical(
        dots$keep.fingerprint, 
        name = "keep.fingerprint"
      )
    }
    if(is.null(dots$n.cluster)){
      dots$n.cluster <- 1
    } else {
      dots$n.cluster <- check_single_strictly_positive_integer(
        dots$n.cluster, 
        name = "n.cluster"
      )
    }
    
    # x is an existing file
    if(file_test("-f", x)){
      message(x)
      local.environment <- new.env()
      load(x, envir = local.environment)
      analysis <- read_object_environment(object = "analysis", env = local.environment)
      return(get_result(analysis, ...))
    }
  
    if(!file_test("-d", x)){
      stop("'x' is neither an existing file, neither an existing directory")
    }
    
    # x is an existing directory
    x <- normalizePath(x, winslash = "/", mustWork = TRUE)
    files <- list.files(path = x, pattern = "\\.rda$", full.names = TRUE)
    if(dots$n.cluster == 1){
      result <- lapply(files, get_result, ...)
    } else {
      if(requireNamespace("parallel", quietly = TRUE)){
        available.cluster <- parallel::detectCores()
        if(dots$n.cluster > available.cluster){
          message("Requesting ", dots$n.cluster, " clusters but only ", available.cluster, " available.")
          dots$n.cluster <- available.cluster
        }
        message("Reading results in parallel on ", dots$n.cluster, " clusters")
        utils::flush.console()
        cl <- parallel::makeCluster(dots$n.cluster)
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
    
    if(dots$keep.fingerprint){
      return(result)
    }
    
    message("Converting sha1 to integer")
    utils::flush.console()
    # convert analysis fingerprint from sha1 to factor
    result@ParameterEstimate$Analysis <- factor(
      result@ParameterEstimate$Analysis,
      levels = result@AnalysisMetadata$Fingerprint
    )
    result@Anomaly$Analysis <- factor(
      result@Anomaly$Analysis,
      levels = result@AnalysisMetadata$Fingerprint
    )
    result@AnalysisMetadata$Fingerprint <- factor(
      result@AnalysisMetadata$Fingerprint,
      levels = result@AnalysisMetadata$Fingerprint
    )
    
    # convert parameter fingerprint from sha1 to integer
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
    result@AnalysisVersionRPackage$AnalysisVersion <- factor(
      result@AnalysisVersionRPackage$AnalysisVersion,
      levels = result@AnalysisVersion$Fingerprint
    )
    result@AnalysisVersion$Fingerprint <- factor(
      result@AnalysisVersion$Fingerprint,
      levels = result@AnalysisVersion$Fingerprint
    )
    
    validObject(result)
    return(result)
  }
)
