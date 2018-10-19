#' @rdname get_anomaly
#' @aliases get_anomaly,n2kInlaNbinomial-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.number is.flag noNA is.string
#' @importFrom dplyr data_frame select_ filter_ mutate_ bind_cols arrange_ ungroup slice_ transmute_ group_by
#' @importFrom digest sha1
#' @importFrom n2khelper is.chartor
#' @include n2kInlaNbinomial_class.R
setMethod(
  f = "get_anomaly",
  signature = signature(analysis = "n2kInlaNbinomial"),
  definition = function(
    analysis,
    n = 20,
    log.expected.ratio = log(5),
    log.expected.absent = log(5),
    random.threshold = log(1.05),
    verbose = TRUE,
    ...
  ){
    assert_that(is.count(n))
    assert_that(is.number(log.expected.ratio))
    assert_that(is.number(log.expected.absent))
    assert_that(is.number(random.threshold))
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

    parameter <- get_model_parameter(
      analysis = analysis,
      verbose = verbose,
      ...
    )
    if (status(analysis) != "converged") {
      return(
        new(
          "n2kAnomaly",
          Parameter = parameter@Parameter,
          ParameterEstimate = parameter@ParameterEstimate
        )
      )
    }

    if (verbose) {
      message("    reading anomaly", appendLF = FALSE)
    }
    utils::flush.console()

    anomaly.type <- data_frame(
      Description = c(
        "Large ratio of observed vs expected",
        "Small ratio of observed vs expected",
        "Zero observed and high expected",
        "Unstable imputations"
      )
    ) %>%
      rowwise() %>%
      mutate_(
        Fingerprint = ~sha1(c(Description = Description))
      )
    anomaly <- data_frame(
      AnomalyType = character(0),
      Analysis = character(0),
      Parameter = character(0),
      Observation = character(0)
    )

    response <- as.character(analysis@AnalysisFormula[[1]][2])
    data <- get_data(analysis)
    if (!is.chartor(data$ObservationID)) {
      data$ObservationID <- as.character(data$ObservationID)
    }
    data <- data %>%
      mutate_(
        Response = response,
        Expected = ~analysis@Model$summary.fitted.values[, "mean"],
        LogRatio = ~Expected - log(Response),
        Analysis = ~get_file_fingerprint(analysis),
        Observation = ~ObservationID
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
    if (verbose) {
      message(": observed > 0 vs fit", appendLF = FALSE)
    }
    high.ratio <- data %>%
      select_(~Analysis, ~Parameter, ~Observation, ~LogRatio) %>%
      filter_(~is.finite(LogRatio), ~LogRatio > log.expected.ratio) %>%
      select_(~-LogRatio) %>%
      head(n)
    if (nrow(high.ratio) > 0) {
      anomaly <- anomaly.type %>%
        filter_(~ Description == "Large ratio of observed vs expected") %>%
        select_(AnomalyType = ~Fingerprint) %>%
        merge(high.ratio) %>%
        bind_rows(anomaly)
    }
    low.ratio <- data %>%
      select_(~Analysis, ~Parameter, ~Observation, ~LogRatio) %>%
      filter_(~is.finite(LogRatio), ~-LogRatio > log.expected.ratio) %>%
      select_(~-LogRatio) %>%
      head(n)
    if (nrow(low.ratio) > 0) {
      anomaly <- anomaly.type %>%
        filter_(~ Description == "Small ratio of observed vs expected") %>%
        select_(AnomalyType = ~Fingerprint) %>%
        merge(low.ratio) %>%
        bind_rows(anomaly)
    }

    if (verbose) {
      message(", observed == 0 vs fit", appendLF = FALSE)
    }
    high.absent <- data %>%
      select_(
        ~Analysis,
        ~Parameter,
        ~Observation,
        ~Expected,
        ~Response
      ) %>%
      filter_(~Response == 0, ~Expected > log.expected.absent) %>%
      select_(~-Response, ~-Expected) %>%
      head(n)
    if (nrow(high.absent) > 0) {
      anomaly <- anomaly.type %>%
        filter_(~ Description == "Zero observed and high expected") %>%
        select_(AnomalyType = ~Fingerprint) %>%
        merge(high.absent) %>%
        bind_rows(anomaly)
    }
    # select anomalies on random effects
    if (verbose) {
      message(", random effect")
    }
    re.anomaly <- parameter@Parameter %>%
      filter_(~Description == "Random effect BLUP") %>%
      select_(Parent = ~Fingerprint) %>%
      inner_join(parameter@Parameter, by = "Parent") %>%
      transmute_(
        AnomalyType = ~paste(Description, "random intercept"),
        Parent = ~Fingerprint
      ) %>%
      inner_join(parameter@Parameter, by = "Parent") %>%
      select_(~AnomalyType, Parameter = ~Fingerprint) %>%
      inner_join(
        parameter@ParameterEstimate %>%
          filter_(~abs(Estimate) > random.threshold) %>%
          select_(~Analysis, ~Parameter, ~Estimate),
        by = "Parameter"
      )
    if (nrow(re.anomaly) > 0) {
      re.anomaly <- re.anomaly %>%
        mutate_(Sign = ~sign(Estimate)) %>%
        arrange_(~desc(abs(Estimate))) %>%
        group_by(.data$AnomalyType, .data$Sign) %>%
        slice_(~seq_len(n)) %>%
        ungroup() %>%
        select_(~-Sign, ~-Estimate)
      anomaly.type <- re.anomaly %>%
        distinct_(~AnomalyType) %>%
        select_(Description = ~ AnomalyType) %>%
        rowwise() %>%
        mutate_(
          Fingerprint = ~sha1(c(Description = Description))
        ) %>%
        bind_rows(anomaly.type)
      anomaly <- re.anomaly %>%
        inner_join(anomaly.type, by = c("AnomalyType" = "Description")) %>%
        select_(~-AnomalyType, AnomalyType = ~ Fingerprint) %>%
        bind_rows(anomaly)
    }

    if (!is.null(analysis@RawImputed)) {
      parent <- anomaly.type %>%
        filter_(~ Description == "Unstable imputations")
      imputations <- parameter@Parameter %>%
        filter_(~Description == "Imputed value") %>%
        semi_join(
          x = parameter@Parameter,
          by = c("Parent" = "Fingerprint")
        ) %>%
        select_(~Fingerprint, Observation = ~Description) %>%
        inner_join(
          x = parameter@ParameterEstimate,
          by = c("Parameter" = "Fingerprint")
        ) %>%
        mutate_(AnomalyType = ~parent$Fingerprint)
      anomaly <- anomaly %>%
        bind_rows(
          imputations %>%
            filter_(~LowerConfidenceLimit == 0) %>%
            arrange_(~UpperConfidenceLimit) %>%
            tail(n) %>%
            select_(~Parameter, ~Analysis, ~AnomalyType, ~Observation),
          imputations %>%
            filter_(~LowerConfidenceLimit > 0) %>%
            mutate_(Ratio = ~UpperConfidenceLimit / LowerConfidenceLimit) %>%
            arrange_(~Ratio) %>%
            tail(n) %>%
            select_(~Parameter, ~Analysis, ~AnomalyType, ~Observation)
        )
    }

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
