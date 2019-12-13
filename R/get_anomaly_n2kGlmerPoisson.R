#' @rdname get_anomaly
#' @aliases get_anomaly,n2kGlmerPoisson-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.number is.string
#' @importFrom lme4 ranef
#' @importFrom digest sha1
#' @importFrom stats fitted
#' @importFrom utils tail head
#' @include n2kGlmerPoisson_class.R
#' @include n2kAnomaly_class.R
#' @param n The maximum number of anomalies per type of anomalies.
#' @param log.expected.ratio Observations that have `abs(log(observed/fitted))`
#' above this ratio are potential anomalies.
#' Defaults to `log(5)`, which implies that observed values that are 5 times
#' higher of lower than the fitted values are potential anomalies.
#' @param log.expected.absent Zero observations with `log(fitted)` larger than
#' this threshold are potential anomalies.
#' @param random.threshold Random effects with a absolute value above this
#' threshold are potential anomalies.
#' @param verbose Print extra information on the screen
setMethod(
  f = "get_anomaly",
  signature = signature(analysis = "n2kGlmerPoisson"),
  definition = function(
    analysis,
    n = 20,
    log.expected.ratio = log(5),
    log.expected.absent = log(5),
    random.threshold = log(5),
    verbose = TRUE,
    ...
  ) {
    assert_that(is.count(n))
    assert_that(is.number(log.expected.ratio))
    assert_that(is.number(log.expected.absent))
    assert_that(is.number(random.threshold))

    parameter <- get_model_parameter(analysis = analysis, verbose = verbose)
    if (status(analysis) != "converged") {
      return(
        new(
          "n2kAnomaly",
          Parameter = parameter@Parameter,
          ParameterEstimate = parameter@ParameterEstimate
        )
      )
    }

    display(verbose, "    reading anomaly", FALSE)

    anomaly.type <- data.frame(
      Description = character(0),
      Fingerprint = character(0),
      stringsAsFactors = FALSE
    )
    anomaly <- data.frame(
      AnomalyType = character(0),
      Analysis = character(0),
      Parameter = character(0),
      Observation = character(0),
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
      !is.na(parameter@Parameter$Parent) &
        parameter@Parameter$Parent == fitted.sha,
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
    display(verbose, ": observed > 0 vs fit", FALSE)
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
        Description = paste(
          names(combination)[i],
          "ratio of observed vs expected"
        ),
        stringsAsFactors = FALSE
      )
      extra$Fingerprint <- apply(extra, 1, sha1)
      anomaly.type <- rbind(anomaly.type, extra)

      extra.observation <- data.frame(
        AnomalyType = extra$Fingerprint,
        Analysis = get_file_fingerprint(analysis),
        Parameter = selection$Fingerprint,
        DataFieldID = selection$DataFieldID,
        Observation = selection$ObservationID,
        stringsAsFactors = FALSE
      )
      anomaly <- rbind(anomaly, extra.observation)
    }

    display(verbose, ", observed == 0 vs fit", FALSE)
    data.subset <- data[
      data[, response] == 0 & data$Expected > log.expected.absent,
    ]
    if (nrow(data.subset) > 0) {
      data.subset <- tail(
        data.subset[order(data.subset$Expected), ],
        n
      )
      extra <- data.frame(
        Description = "Zero observed and high expected",
        stringsAsFactors = FALSE
      )
      extra$Fingerprint <- apply(extra, 1, sha1)
      anomaly.type <- rbind(anomaly.type, extra)
      extra.observation <- data.frame(
        AnomalyType = extra$Fingerprint,
        Analysis = get_file_fingerprint(analysis),
        Parameter = data.subset$Fingerprint,
        DataFieldID = data.subset$DataFieldID,
        Observation = data.subset$ObservationID,
        stringsAsFactors = FALSE
      )
      anomaly <- rbind(anomaly, extra.observation)
    }

    # select anomalies on random effects
    display(verbose, ", random effect")
    re <- ranef(get_model(analysis))
    if (any(sapply(re, ncol) > 1)) {
      stop("get_anomaly cannot handle random slopes yet")
    }
    main.sha <- parameter@Parameter$Fingerprint[
      parameter@Parameter$Description == "Random effect BLUP"
    ]
    for (i in seq_along(re)) {
      this.re <- re[[i]]
      this.re <- this.re[this.re[, 1] > random.threshold, , drop = FALSE] #nolint
      if (nrow(this.re) == 0) {
        next
      }
      this.re <- this.re[order(abs(this.re[, 1])), , drop = FALSE] #nolint
      parent.sha <- parameter@Parameter$Fingerprint[
        parameter@Parameter$Parent == main.sha &
        parameter@Parameter$Description == names(re)[i]
      ]

      for (j in combination) {
        selection <- tail(
          this.re[sign(this.re[, 1]) == combination[j], , drop = FALSE], #nolint
          n = n
        )
        if (nrow(selection) == 0) {
          next
        }

        extra <- data.frame(
          Description = paste(names(combination)[j], "random intercept"),
          stringsAsFactors = FALSE
        )
        extra$Fingerprint <- apply(extra, 1, sha1)
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
          DatasourceID = analysis@AnalysisMetadata$ResultDatasourceID,
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
