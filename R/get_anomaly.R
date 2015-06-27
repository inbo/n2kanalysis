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
#' @importFrom n2khelper check_single_strictly_positive_integer check_single_numeric
#' @importFrom lme4 ranef
#' @include n2kGlmerPoisson_class.R
#' @include n2kAnomaly_class.R
#' @param datasource.id ID of the datasource for the random effects
#' @param n the maximum number of anomalies per type of anomalies
#' @param log.expected.ratio observations that have a abs(log(observed/fitted)) above this ratio are potential anomalies. Defaults to log(5), which implies that observed values that are 5 times higher of lower than the fitted values are potential anomalies.
#' @param log.expected.absent Zero observations with log(fitted) larger than this treshold are potential anomalies.
#' @param random.treshold random effects with a absolute value above this treshold are potential anomalies
setMethod(
  f = "get_anomaly",
  signature = signature(analysis = "n2kGlmerPoisson"),
  definition = function(
    analysis, 
    datasource.id,
    n = 20, 
    log.expected.ratio = log(5), 
    log.expected.absent = log(5), 
    random.treshold = log(5), 
    ...
  ){
    datasource.id <- check_single_strictly_positive_integer(
      datasource.id, 
      name = "datasource.id"
    )
    n <- check_single_strictly_positive_integer(n, name = "n")
    log.expected.ratio <- check_single_numeric(
      log.expected.ratio, 
      name = "log.expected.ratio"
    )
    log.expected.absent <- check_single_numeric(
      log.expected.absent, 
      name = "log.expected.absent"
    )
    random.treshold <- check_single_numeric(
      random.treshold, 
      name = "random.treshold"
    )
    
    parameter <- get_model_parameter(analysis = analysis)
    if (status(analysis) != "converged") {
      return(
        new(
          "n2kAnomaly",
          Parameter = parameter@Parameter,
          ParameterEstimate = parameter@ParameterEstimate
        )
      )
    }
    
    message("    reading anomaly", appendLF = FALSE)
    utils::flush.console()

    anomaly.type <- data.frame(
      Description = character(0),
      Fingerprint = character(0),
      stringsAsFactors = FALSE
    )
    anomaly <- data.frame(
      AnomalyType = character(0),
      Analysis = character(0),
      Parameter = character(0),
      DatasourceID = integer(0),
      Datafield = character(0),
      stringsAsFactors = FALSE
    )
    
    response <- as.character(analysis@AnalysisFormula[[1]][2])
    data <- get_data(analysis)
    if (!class(data$ObservationID) %in% c("character", "factor")) {
      data$ObservationID <- as.character(data$ObservationID)
    }
    data$Expected <- fitted(get_model(analysis))
    fitted.sha <- parameter@Parameter$Fingerprint[
      parameter@Parameter$Description == "Fitted"
    ]
    parameter.sha <- parameter@Parameter[
      !is.na(parameter@Parameter$Parent) & parameter@Parameter$Parent == fitted.sha, 
      c("Fingerprint", "Description")
    ]
    data <- merge(
      data, 
      parameter.sha, 
      by.x = "ObservationID", 
      by.y = "Description", 
      all.x = TRUE
    )

    # check observed counts versus expected counts
    message(": observed > 0 vs fit", appendLF = FALSE)
    data.subset <- data[data[, response] > 0, ]
    data.subset$LogRatio <- log(data.subset[, response]) - data.subset$Expected
    data.subset <- data.subset[abs(data.subset$LogRatio) > log.expected.ratio, ]
    data.subset <- data.subset[order(abs(data.subset$LogRatio)), ]
    
    combination <- c("Large" = 1, "Small" = -1)
    for (i in seq_along(combination)) {
      selection <- tail(
        data.subset[
          sign(data.subset$LogRatio) == combination[i],
        ],
        n = n
      )
      if (nrow(selection) == 0) {
        next
      }
      extra <- data.frame(
        Description = paste(names(combination)[i], "ratio of observed vs expected"),
        stringsAsFactors = FALSE
      )
      extra$Fingerprint <- apply(extra, 1, get_sha1)
      anomaly.type <- rbind(anomaly.type, extra)
      
      extra.observation <- data.frame(
        AnomalyType = extra$Fingerprint,
        Analysis = get_file_fingerprint(analysis),
        Parameter = selection$Fingerprint,
        DatasourceID = selection$DatasourceID,
        Datafield = "Observation",
        stringsAsFactors = FALSE
      )
      anomaly <- rbind(anomaly, extra.observation)
    }

    message(", observed == 0 vs fit", appendLF = FALSE)
    data.subset <- data[data[, response] == 0 & data$Expected > log.expected.absent, ]
    if (nrow(data.subset) > 0) {
      data.subset <- tail(
        data.subset[order(data.subset$Expected), ],
        n
      )
      extra <- data.frame(
        Description = "Zero observed and high expected",
        stringsAsFactors = FALSE
      )
      extra$Fingerprint <- apply(extra, 1, get_sha1)
      anomaly.type <- rbind(anomaly.type, extra)
      extra.observation <- data.frame(
        AnomalyType = extra$Fingerprint,
        Analysis = get_file_fingerprint(analysis),
        Parameter = data.subset$Fingerprint,
        DatasourceID = data.subset$DatasourceID,
        Datafield = "Observation",
        stringsAsFactors = FALSE
      )
      anomaly <- rbind(anomaly, extra.observation)
    }
    
    # select anomalies on random effects
    message(", random effect")
    re <- ranef(get_model(analysis))
    if (any(sapply(re, ncol) > 1)) {
      stop("get_anomaly cannot handle random slopes yet")
    }
    data.field <- gsub("^f", "", names(re))
    data.field <- gsub("ID$", "", data.field)
    main.sha <- parameter@Parameter$Fingerprint[
      parameter@Parameter$Description == "Random effect BLUP"
    ]
    for (i in seq_along(re)) {
      this.re <- re[[i]]
      this.re <- this.re[this.re[, 1] > random.treshold, , drop = FALSE]
      if (nrow(this.re) == 0) {
        next
      }
      this.re <- this.re[order(abs(this.re[, 1])), , drop = FALSE]
      parent.sha <- parameter@Parameter$Fingerprint[
        parameter@Parameter$Parent == main.sha &
        parameter@Parameter$Description == names(re)[i]
      ]
      
      for (j in combination) {
        selection <- tail(
          this.re[sign(this.re[, 1]) == combination[j], , drop = FALSE],
          n = n
        )
        if (nrow(selection) == 0) {
          next
        }
        
        extra <- data.frame(
          Description = paste(names(combination)[j], "random intercept"),
          stringsAsFactors = FALSE
        )
        extra$Fingerprint <- apply(extra, 1, get_sha1)
        anomaly.type <- unique(rbind(anomaly.type, extra))
        
        selection[names(re)[i]] <- row.names(selection)
        parameter.sha <- parameter@Parameter[
          parameter@Parameter$Parent == parent.sha, 
          c("Description", "Fingerprint")
        ]
        selection <- merge(
          selection, 
          parameter.sha, 
          by.x = names(re)[i], 
          by.y = "Description"
        )
        
        extra.observation <- data.frame(
          AnomalyType = extra$Fingerprint,
          Analysis = get_file_fingerprint(analysis),
          Parameter = selection$Fingerprint,
          DatasourceID = datasource.id,
          Datafield = data.field[i],
          stringsAsFactors = FALSE
        )
        anomaly <- rbind(anomaly, extra.observation)
      }
    }

    return(
      new(
        "n2kAnomaly",
        Parameter = parameter@Parameter,
        ParameterEstimate = parameter@ParameterEstimate,
        AnomalyType = anomaly.type,
        Anomaly = anomaly
      )
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
    parameter <- get_model_parameter(analysis = analysis)
    return(
      new(
        "n2kAnomaly",
        Parameter = parameter@Parameter,
        ParameterEstimate = parameter@ParameterEstimate
      )
    )
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
    parameter <- get_model_parameter(analysis = analysis)
    return(
      new(
        "n2kAnomaly",
        Parameter = parameter@Parameter,
        ParameterEstimate = parameter@ParameterEstimate
      )
    )
  }
)
