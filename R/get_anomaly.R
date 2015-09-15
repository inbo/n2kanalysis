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
#' @importFrom assertthat assert_that is.count is.number
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
    assert_that(is.count(datasource.id))
    assert_that(is.count(n))
    assert_that(is.number(log.expected.ratio))
    assert_that(is.number(log.expected.absent))
    assert_that(is.number(random.treshold))

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
        Description = paste(
          names(combination)[i],
          "ratio of observed vs expected"
        ),
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

#' @rdname get_anomaly
#' @aliases get_anomaly,n2kInlaNbinomial-methods
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that is.count is.number
#' @importFrom dplyr data_frame add_rownames select_ filter_ mutate_ bind_cols arrange_ ungroup slice transmute_
#' @include n2kInlaNbinomial_class.R
setMethod(
  f = "get_anomaly",
  signature = signature(analysis = "n2kInlaNbinomial"),
  definition = function(
    analysis,
    datasource.id,
    n = 20,
    log.expected.ratio = log(5),
    log.expected.absent = log(5),
    random.treshold = log(1.05),
    ...
  ){
    assert_that(is.count(datasource.id))
    datasource.id <- as.integer(datasource.id)
    assert_that(is.count(n))
    assert_that(is.number(log.expected.ratio))
    assert_that(is.number(log.expected.absent))
    assert_that(is.number(random.treshold))

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

    anomaly.type <- data_frame(
      Description = c(
        "Large ratio of observed vs expected",
        "Small ratio of observed vs expected",
        "Zero observed and high expected"
      )
    ) %>%
      rowwise() %>%
      mutate_(
        Fingerprint = ~get_sha1(c(Description = Description))
      )
    anomaly <- data_frame(
      AnomalyType = character(0),
      Analysis = character(0),
      Parameter = character(0),
      DatasourceID = integer(0),
      Datafield = character(0)
    )

    response <- as.character(analysis@AnalysisFormula[[1]][2])
    data <- get_data(analysis)
    if (!class(data$ObservationID) %in% c("character", "factor")) {
      data$ObservationID <- as.character(data$ObservationID)
    }
    data <- data %>%
      mutate_(
        Response = response,
        Expected = ~analysis@Model$summary.fitted.values[, "mean"],
        LogRatio = ~Expected - log(Response),
        Analysis = ~get_file_fingerprint(analysis)
      )

    parameter.id <- parameter@Parameter %>%
      filter_(~Description == "Fitted") %>%
      select_(~Fingerprint) %>%
      inner_join(parameter@Parameter, by = c("Fingerprint" = "Parent")) %>%
      select_(Parameter = ~Fingerprint.y, ~Description)
    length.antijoin <- data %>%
      anti_join(parameter.id, by = c("ObservationID" = "Description")) %>%
      nrow()
    assert_that(length.antijoin == 0)

    data <- data %>%
      inner_join(parameter.id, by = c("ObservationID" = "Description")) %>%
      arrange_(~desc(abs(LogRatio)), ~desc(Expected))

    # check observed counts versus expected counts
    message(": observed > 0 vs fit", appendLF = FALSE)
    high.ratio <- data %>%
      select_(~Analysis, ~Parameter, ~DatasourceID, Estimate = ~LogRatio) %>%
      filter_(~is.finite(Estimate), ~Estimate > log.expected.ratio) %>%
      head(n)
    if (nrow(high.ratio) > 0) {
      anomaly <- anomaly.type %>%
        filter_(~ Description == "Large ratio of observed vs expected") %>%
        select_(AnomalyType = ~Fingerprint) %>%
        merge(high.ratio) %>%
        mutate_(Datafield = ~"Observation") %>%
        bind_rows(anomaly)
    }
    low.ratio <- data %>%
      select_(~Analysis, ~Parameter, ~DatasourceID, Estimate = ~LogRatio) %>%
      filter_(~is.finite(Estimate), ~-Estimate > log.expected.ratio) %>%
      head(n)
    if (nrow(low.ratio) > 0) {
      anomaly <- anomaly.type %>%
        filter_(~ Description == "Small ratio of observed vs expected") %>%
        select_(AnomalyType = ~Fingerprint) %>%
        merge(low.ratio) %>%
        mutate_(Datafield = ~"Observation") %>%
        bind_rows(anomaly)
    }

    message(", observed == 0 vs fit", appendLF = FALSE)
    high.absent <- data %>%
      select_(
        ~Analysis,
        ~Parameter,
        ~DatasourceID,
        Estimate = ~Expected,
        ~Response
      ) %>%
      filter_(~Response == 0, ~Estimate > log.expected.absent) %>%
      select_(~-Response) %>%
      head(n)
    if (nrow(high.absent) > 0) {
      anomaly <- anomaly.type %>%
        filter_(~ Description == "Zero observed and high expected") %>%
        select_(AnomalyType = ~Fingerprint) %>%
        merge(high.absent) %>%
        mutate_(Datafield = ~"Observation") %>%
        bind_rows(anomaly)
    }
    # select anomalies on random effects
    message(", random effect")
    re.anomaly <- parameter@Parameter %>%
      filter_(~Description == "Random effect BLUP") %>%
      select_(Parent = ~Fingerprint) %>%
      inner_join(parameter@Parameter, by = "Parent") %>%
      transmute_(
        AnomalyType = ~paste(Description, "random intercept"),
        Datafield = ~Description,
        Parent = ~Fingerprint
      ) %>%
      inner_join(parameter@Parameter, by = "Parent") %>%
      select_(~AnomalyType, ~Datafield, Parameter = ~Fingerprint) %>%
      inner_join(
        parameter@ParameterEstimate %>%
          filter_(~abs(Estimate) > random.treshold) %>%
          select_(~Analysis, ~Parameter, ~Estimate),
        by = "Parameter"
      ) %>%
      mutate_(Sign = ~sign(Estimate)) %>%
      arrange_(~desc(abs(Estimate))) %>%
      group_by_(~AnomalyType, ~Sign) %>%
      slice(seq_len(n)) %>%
      ungroup() %>%
      select_(~-Sign)
    anomaly.type <- re.anomaly %>%
      group_by_(~AnomalyType) %>%
      summarise_() %>%
      select_(Description = ~ AnomalyType) %>%
      rowwise() %>%
      mutate_(
        Fingerprint = ~get_sha1(c(Description = Description))
      ) %>%
      bind_rows(anomaly.type)
    anomaly <- re.anomaly %>%
      inner_join(anomaly.type, by = c("AnomalyType" = "Description")) %>%
      select_(~-AnomalyType, AnomalyType = ~ Fingerprint) %>%
      mutate_(DatasourceID = datasource.id) %>%
      bind_rows(anomaly)

    return(
      new(
        "n2kAnomaly",
        Parameter = parameter@Parameter,
        ParameterEstimate = parameter@ParameterEstimate,
        AnomalyType = as.data.frame(anomaly.type),
        Anomaly = as.data.frame(anomaly)
      )
    )
  }
)
