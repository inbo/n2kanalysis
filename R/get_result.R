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
    standard.generic("get_result") # nocov
  }
)


#' @rdname get_result
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that is.flag noNA
#' @include n2kModel_class.R
#' @include n2kResult_class.R
#' @param verbose Print extra information on the screen
setMethod(
  f = "get_result",
  signature = signature(x = "n2kModel"),
  definition = function(x, verbose = TRUE, ...){
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

    validObject(x)
    anomaly <- get_anomaly(analysis = x, verbose = verbose, ...)
    return(
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
    )
  }
)

#' @rdname get_result
#' @importFrom methods setMethod
#' @importFrom dplyr %>% rowwise mutate_ add_rownames inner_join select_ transmute_ arrange_ filter_
#' @importFrom n2khelper get_sha1
#' @importFrom tidyr gather_
#' @importFrom assertthat assert_that is.flag noNA
#' @include n2kResult_class.R
#' @include n2kInlaNbinomial_class.R
setMethod(
  f = "get_result",
  signature = signature(x = "n2kInlaNbinomial"),
  definition = function(x, verbose = TRUE, ...){
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

    validObject(x)
    anomaly <- get_anomaly(analysis = x, verbose = verbose, ...)
    if (is.null(x@LinearCombination)) {
      return(
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
      )
    }
    contrast <- data_frame(
      Description = rownames(x@LinearCombination),
      Analysis = get_file_fingerprint(x)
    ) %>%
      rowwise() %>%
      mutate_(
        Fingerprint = ~get_sha1(
          c(Description = Description, Analysis = Analysis)
        )
      ) %>%
      select_(~Fingerprint, ~Description, ~Analysis) %>%
      arrange_(~Analysis, ~Description) %>%
      as.data.frame()
    if (is.null(x@Model)) {
      return(
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
          Anomaly = anomaly@Anomaly,
          Contrast = contrast
        )
      )
    }
    contrast.coefficient <- x@LinearCombination %>%
      as.data.frame() %>%
      add_rownames("Description")
    contrast.coefficient <- contrast.coefficient %>%
      gather_(
        "ParameterID",
        "Coefficient",
        tail(colnames(contrast.coefficient), -1)
      ) %>%
      inner_join(
        contrast %>%
          select_(~-Analysis),
        by = "Description"
      ) %>%
      select_(~-Description, Contrast = ~Fingerprint) %>%
      mutate_(ParameterID = ~levels(ParameterID)[ParameterID]) %>%
      filter_(~ abs(Coefficient) > 1e-8)
    concat <- function(parent, child){
      parent.split <- strsplit(parent, ":")
      child.split <- strsplit(child, ":")
      too.short <- sapply(child.split, length) < sapply(parent.split, length)
      child.split[too.short] <- lapply(child.split[too.short], c, "")
      sapply(
        seq_along(parent.split),
        function(i){
          rbind(parent.split[[i]], child.split[[i]])
        }
      )
      apply(
        cbind(parent.split, child.split),
        1,
        function(z){
          do.call(
            function(...){
              paste0(..., collapse = ":")
            },
            z
          )
        }
      )
    }
    fixed.fingerprint <- anomaly@Parameter %>%
      filter_(~Description == "Fixed effect") %>%
      select_(~Fingerprint)
    contrast.coefficient <- anomaly@Parameter %>%
      filter_(~Parent == fixed.fingerprint$Fingerprint) %>%
      select_(ParentDescription = ~Description, Parent = ~Fingerprint) %>%
      left_join(anomaly@Parameter, by = "Parent") %>%
      transmute_(
        Parameter = ~ifelse(is.na(Fingerprint), Parent, Fingerprint),
        ParameterID = ~concat(child = Description, parent = ParentDescription)
      ) %>%
      inner_join(contrast.coefficient, by = "ParameterID") %>%
      select_(~Contrast, ~Parameter, ~Coefficient) %>%
      arrange_(~Contrast, ~Parameter)
    if (nrow(x@Model$summary.lincomb) == 0) {
      lc <- x@Model$summary.lincomb.derived
    } else {
      lc <- x@Model$summary.lincomb
    }
    contrast.estimate <- data_frame(
      Description = rownames(lc),
      Estimate = lc$mean,
      LowerConfidenceLimit = lc[, "0.025quant"],
      UpperConfidenceLimit = lc[, "0.975quant"]
    ) %>%
      inner_join(
        contrast %>%
          select_(~-Analysis),
        by = "Description"
      ) %>%
      select_(
        Contrast = ~Fingerprint,
        ~Estimate,
        ~LowerConfidenceLimit,
        ~UpperConfidenceLimit
      ) %>%
      arrange_(~Contrast) %>%
      as.data.frame()
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
      Anomaly = anomaly@Anomaly,
      Contrast = contrast,
      ContrastCoefficient = contrast.coefficient,
      ContrastEstimate = contrast.estimate
    )
  }
)

#' @rdname get_result
#' @importFrom methods setMethod validObject
#' @importFrom assertthat assert_that is.string is.flag is.count noNA
#' @param keep.fingerprint Keep the character fingerprints? Otherwise change them into integers
#' @param n.cluster the number of clusters to run this function in parallel. Defaults to 1 (= no parallel computing).
setMethod(
  f = "get_result",
  signature = signature(x = "character"),
  definition = function(
    x,
    keep.fingerprint = TRUE,
    n.cluster = 1,
    verbose = TRUE,
    ...
  ){
    # check arguments
    assert_that(is.string(x))
    assert_that(is.flag(keep.fingerprint))
    assert_that(is.count(n.cluster))
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

    # x is an existing file
    if (file_test("-f", x)) {
      if (verbose) {
        message(x)
      }
      local.environment <- new.env()
      load(x, envir = local.environment)
      analysis <- read_object_environment(
        object = "analysis",
        env = local.environment
      )
      return(get_result(x = analysis, verbose = verbose, ...))
    }

    if (!file_test("-d", x)) {
      stop("'x' is neither an existing file, neither an existing directory")
    }

    # x is an existing directory
    x <- normalizePath(x, winslash = "/", mustWork = TRUE)
    files <- list.files(path = x, pattern = "\\.rda$", full.names = TRUE)
    if (n.cluster == 1) {
      result <- lapply(files, get_result, verbose = verbose, ...)
    } else {
      # nocov start
      if (requireNamespace("parallel", quietly = TRUE)) {
        available.cluster <- parallel::detectCores()
        if (n.cluster > available.cluster) {
          message(
            "Requesting ", n.cluster, " clusters but only ", available.cluster,
            " available."
          )
          n.cluster <- available.cluster
        }
        if (verbose) {
          message("Reading results in parallel on ", n.cluster, " clusters")
        }
        utils::flush.console()
        cl <- parallel::makeCluster(n.cluster)
        result <- parallel::clusterApplyLB(
          cl = cl,
          x = files,
          fun = get_result,
          verbose = verbose,
          ...
        )
        parallel::stopCluster(cl)
      } else {
        message(
"Cannot load the parallel package. Falling back to non-parallel computing."
        )
        utils::flush.console()
        result <- lapply(files, get_result, verbose = verbose, ...)
      }
      # nocov end
    }

    if (verbose) {
      message("Combining results")
    }
    utils::flush.console()
    result <- do.call(combine, result)

    if (keep.fingerprint) {
      return(result)
    }

    if (verbose) {
      message("Converting sha1 to integer")
    }
    utils::flush.console()
    return(simplify_result(result = result))
  }
)
