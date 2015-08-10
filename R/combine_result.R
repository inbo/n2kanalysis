#' @rdname combine
#' @aliases combine,n2kResult-methods
#' @importFrom methods setMethod
#' @include n2kResult_class.R
setMethod(
  f = "combine",
  signature = "n2kResult",
  definition = function(...){
    dots <- list(...)
    
    analysis.metadata <- do.call(
      rbind, 
      lapply(dots, function(x){x@AnalysisMetadata}
    ))
    analysis.metadata <- analysis.metadata[order(analysis.metadata$FileFingerprint),]
    
    analysis.formula <- lapply(analysis.metadata$Formula, as.formula)
    
    analysis.relation <- do.call(
      rbind, 
      lapply(dots, function(x){x@AnalysisRelation}
    ))
    analysis.relation <- analysis.relation[
      order(analysis.relation$Analysis, analysis.relation$ParentAnalysis),
    ]
    
    analysis.version <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@AnalysisVersion})
    ))
    analysis.version <- analysis.version[order(analysis.version$Fingerprint), , drop = FALSE]
    
    r.package <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@RPackage})
    ))
    r.package <- r.package[order(r.package$Fingerprint), ]
    
    analysis.version.r.package <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@AnalysisVersionRPackage})
    ))
    analysis.version.r.package <- analysis.version.r.package[
      order(analysis.version.r.package$AnalysisVersion, analysis.version.r.package$RPackage),
    ]
    
    parameter <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@Parameter})
    ))
    parameter <- parameter[order(parameter$Fingerprint), ]
    
    parameter.estimate <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@ParameterEstimate})
    ))
    parameter.estimate <- parameter.estimate[
      order(parameter.estimate$Analysis, parameter.estimate$Parameter), 
    ]
    
    anomaly.type <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@AnomalyType})
    ))
    anomaly.type <- anomaly.type[order(anomaly.type$Fingerprint), ]
    
    anomaly <- unique(do.call(
      rbind,
      lapply(dots, function(x){x@Anomaly})
    ))
    anomaly <- anomaly[order(anomaly$Analysis, anomaly$AnomalyType, anomaly$Parameter), ]

    new(
      "n2kResult",
      AnalysisMetadata = analysis.metadata,
      AnalysisFormula = analysis.formula,
      AnalysisRelation = analysis.relation,
      AnalysisVersion = analysis.version,
      RPackage = r.package,
      AnalysisVersionRPackage = analysis.version.r.package,
      Parameter = parameter,
      ParameterEstimate = parameter.estimate,
      AnomalyType = anomaly.type,
      Anomaly = anomaly
    )
  }
)
