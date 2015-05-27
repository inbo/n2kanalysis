#' Get the anomalies from a model
#' @param analysis The model to add
#' @param ... Extra options. See details
#' @name get_anomaly
#' @rdname get_anomaly
#' @exportMethod get_anomaly
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_anomaly", 
  def = function(analysis, ...){
    standard.generic("get_anomaly")
  }
)

#' @rdname get_anomaly
#' @aliases get_anomaly,n2kGlmerPoisson-methods
#' @importFrom methods setMethod
#' @importFrom digest digest
#' @importFrom n2khelper check_single_strictly_positive_integer check_single_numeric
#' @importFrom lme4 ranef
#' @include n2kGlmerPoisson_class.R
#' @include n2kAnomaly_class.R
setMethod(
  f = "get_anomaly",
  signature = signature(analysis = "n2kGlmerPoisson"),
  definition = function(analysis, ...){
    if(analysis@Status != "converged"){
      return(new("n2kAnomaly"))
    }
    dots <- list(...)
    if(is.null(dots$n)){
      dots$n <- 20
    } else {
      dots$n <- check_single_strictly_positive_integer(dots$n, name = "n")
    }
    if(is.null(dots$log.expected.ratio)){
      dots$log.expected.ratio <- log(5)
    } else {
      dots$log.expected.ratio <- check_single_numeric(
        dots$log.expected.ratio, 
        name = "log.expected.ratio"
      )
    }
    if(is.null(dots$expected.absent)){
      dots$log.expected.absent <- log(5)
    } else {
      dots$log.expected.absent <- check_single_numeric(
        dots$log.expected.absent, 
        name = "log.expected.absent"
      )
    }
    if(is.null(dots$random.treshold)){
      dots$random.treshold <- log(5)
    } else {
      dots$random.treshold <- check_single_numeric(
        dots$random.treshold, 
        name = "random.treshold"
      )
    }
    
    anomaly.type <- data.frame(
      Description = character(0),
      Fingerprint = character(0),
      stringsAsFactors = FALSE
    )
    anomaly <- data.frame(
      Analysis = character(0),
      AnomalyType = character(0),
      ObservationID = character(0),
      Estimate = numeric(0),
      stringsAsFactors = FALSE
    )
    
    # check observed counts versus expected counts
    data <- analysis@Data[, c("ObservationID", "Count")]
    data$Expected <- exp(fitted(analysis@Model))
    data$Ratio <- data$Count / data$Expected
    data.subset <- data[data$Count > 0 & data$Ratio > exp(dots$log.expected.ratio), ]
    if(nrow(data.subset) > 0){
      data.subset <- tail(
        data.subset[order(data.subset$Ratio), ],
        dots$n
      )
      extra <- data.frame(
        Description = "Large ratio of observed Count vs expected Count",
        stringsAsFactors = FALSE
      )
      extra$Fingerprint <- apply(extra, 1, digest, algo = "sha1")
      anomaly.type <- rbind(anomaly.type, extra)
      extra.observation <- data.frame(
        Analysis = analysis@FileFingerprint,
        AnomalyType = extra$Fingerprint,
        ObservationID = data.subset$ObservationID,
        Estimate = data.subset$Ratio,
        stringsAsFactors = FALSE
      )
      anomaly <- rbind(anomaly, extra.observation)
    }
    
    data.subset <- data[data$Count > 0 & data$Ratio < exp(-dots$log.expected.ratio), ]
    if(nrow(data.subset) > 0){
      data.subset <- head(
        data.subset[order(data.subset$Ratio), ],
        dots$n
      )
      extra <- data.frame(
        Description = "Small ratio of observed Count vs expected Count",
        stringsAsFactors = FALSE
      )
      extra$Fingerprint <- apply(extra, 1, digest, algo = "sha1")
      anomaly.type <- rbind(anomaly.type, extra)
      extra.observation <- data.frame(
        Analysis = analysis@FileFingerprint,
        AnomalyType = extra$Fingerprint,
        ObservationID = data.subset$ObservationID,
        Estimate = data.subset$Ratio,
        stringsAsFactors = FALSE
      )
      anomaly <- rbind(anomaly, extra.observation)
    }
    
    data.subset <- data[data$Count == 0 & data$Expected > log(dots$log.expected.absent), ]
    if(nrow(data.subset) > 0){
      data.subset <- tail(
        data.subset[order(data.subset$Expected), ],
        dots$n
      )
      extra <- data.frame(
        Description = "Zero observed Count and high expected Count",
        stringsAsFactors = FALSE
      )
      extra$Fingerprint <- apply(extra, 1, digest, algo = "sha1")
      anomaly.type <- rbind(anomaly.type, extra)
      extra.observation <- data.frame(
        Analysis = analysis@FileFingerprint,
        AnomalyType = extra$Fingerprint,
        ObservationID = data.subset$ObservationID,
        Estimate = data.subset$Expected,
        stringsAsFactors = FALSE
      )
      anomaly <- rbind(anomaly, extra.observation)
    }
    
    re <- ranef(analysis@Model)
    if(any(sapply(re, ncol) > 1)){
      stop("get_anomaly cannot handle random slopes yet")
    }
    for(i in seq_along(re)){
      this.re <- re[[i]]
      this.re <- this.re[order(this.re[, 1]), , drop = FALSE]
      high.re <- tail(this.re[this.re[, 1] > dots$random.treshold, , drop = FALSE], dots$n)
      low.re <- head(this.re[this.re[, 1] < -dots$random.treshold, , drop = FALSE], dots$n)
      if(nrow(high.re) > 0){
        extra <- data.frame(
          Description = paste("High random effect of", names(re)[i]),
          stringsAsFactors = FALSE
        )
        extra$Fingerprint <- apply(extra, 1, digest, algo = "sha1")
        anomaly.type <- unique(rbind(anomaly.type, extra))
        
        extra.observation <- data.frame(
          rownames(high.re),
          Estimate = high.re[, 1],
          stringsAsFactors = FALSE
        )
        colnames(extra.observation)[1] <- names(re)[i]
        extra.observation <- merge(
          extra.observation, 
          analysis@Data[, c(names(re)[i], "ObservationID")]
        )
        extra.observation <- unique(extra.observation[, c("ObservationID", "Estimate")])
        extra.observation$Analysis <- analysis@FileFingerprint
        extra.observation$AnomalyType <- extra$Fingerprint
        anomaly <- rbind(anomaly, extra.observation)
      }
      if(nrow(low.re) > 0){
        extra <- data.frame(
          Description = paste("Low random effect of", names(re)[i]),
          stringsAsFactors = FALSE
        )
        extra$Fingerprint <- apply(extra, 1, digest, algo = "sha1")
        anomaly.type <- unique(rbind(anomaly.type, extra))
        
        extra.observation <- data.frame(
          rownames(low.re),
          Estimate = low.re[, 1],
          stringsAsFactors = FALSE
        )
        colnames(extra.observation)[1] <- names(re)[i]
        extra.observation <- merge(
          extra.observation, 
          analysis@Data[, c(names(re)[i], "ObservationID")]
        )
        extra.observation <- unique(extra.observation[, c("ObservationID", "Estimate")])
        extra.observation$Analysis <- analysis@FileFingerprint
        extra.observation$AnomalyType <- extra$Fingerprint
        anomaly <- rbind(anomaly, extra.observation)
      }
    }
    
    new(
      "n2kAnomaly",
      AnomalyType = anomaly.type,
      Anomaly = anomaly
    )
  }
)

#' @rdname get_anomaly
#' @aliases get_anomaly,n2kLtrGlmer-methods
#' @importFrom methods setMethod
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "get_anomaly",
  signature = signature(analysis = "n2kLrtGlmer"),
  definition = function(analysis, ...){
    new("n2kAnomaly")
  }
)

#' @rdname get_anomaly
#' @aliases get_anomaly,n2kComposite-methods
#' @importFrom methods setMethod
#' @include n2kComposite_class.R
setMethod(
  f = "get_anomaly",
  signature = signature(analysis = "n2kComposite"),
  definition = function(analysis, ...){
    new("n2kAnomaly")
  }
)
