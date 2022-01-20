#' @rdname get_anomaly
#' @aliases get_anomaly,n2kInla-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.number is.string
#' @importFrom dplyr arrange bind_cols distinct filter group_by mutate select
#' slice tibble transmute ungroup
#' @importFrom rlang !!
#' @importFrom digest sha1
#' @include n2k_inla_class.R
#' @param expected_ratio Observations that have
#' `observed / fitted > expected_ratio` or `fitted / observed > expected_ratio`
#' are potential anomalies.
#' Defaults to `5`, which implies that observed values that are 5 times higher
#' or lower than the fitted values are potential anomalies.
#' @param expected_absent Zero observations where `fitted > expected_absent` are
#' potential anomalies.
setMethod(
  f = "get_anomaly",
  signature = signature(analysis = "n2kInla"),
  definition = function(
    analysis,
    n = 20,
    expected_ratio = 5,
    expected_absent = 5,
    random_threshold = 1.05,
    verbose = TRUE,
    ...
  ) {
    assert_that(is.count(n))
    if (!is.null(analysis@Model)) {
      assert_that(
        length(analysis@Model$.args$family) == 1,
        msg = "multiple response not handled yet"
      )
      if (
        analysis@Model$.args$family %in% c(
          "poisson", "zeroinflatedpoisson1",
          "nbinomial", "zeroinflatednbinomial1"
        )
      ) {
        assert_that(
          is.number(expected_ratio),
          expected_ratio > 1
        )
        log_expected_ratio <- log(expected_ratio)
        assert_that(
          is.number(expected_absent),
          expected_absent > 1
        )
        log_expected_absent <- log(expected_absent)
        assert_that(
          is.number(random_threshold),
          random_threshold > 1
        )
        random_threshold <- log(random_threshold)
      } else {
        stop(analysis@Model$.args$family, " not handled yet")
      }
    }

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

    display(verbose, "    reading anomaly", FALSE)

    anomaly_type <- tibble(
      Description = c(
        "Large ratio of observed vs expected",
        "Small ratio of observed vs expected",
        "Zero observed and high expected",
        "Unstable imputations"
      )
    ) %>%
      mutate(
        Fingerprint = map_chr(
          .data$Description,
          ~sha1(c(Description = .x))
        )
      )
    anomaly <- tibble(
      AnomalyType = character(0),
      Analysis = character(0),
      Parameter = character(0),
      Observation = character(0),
      Datafield = character(0)
    )

    response <- as.character(get_model(analysis)$.args$formula[2])
    get_data(analysis) %>%
      transmute(
        Response = !!as.name(response),
        Expected = get_model(analysis)$summary.fitted.values[, "mean"],
        LogRatio = log(.data$Response / .data$Expected),
        Analysis = get_file_fingerprint(analysis),
        Observation = as.character(.data$ObservationID),
        Datafield = as.character(.data$DataFieldID)
      ) -> data

    parameter_id <- parameter@Parameter %>%
      filter(.data$Description == "Fitted") %>%
      select(.data$Fingerprint) %>%
      inner_join(parameter@Parameter, by = c("Fingerprint" = "Parent")) %>%
      select(Parameter = .data$Fingerprint.y, .data$Description)
    length_antijoin <- data %>%
      anti_join(parameter_id, by = c("Observation" = "Description")) %>%
      nrow()
    assert_that(length_antijoin == 0)

    data <- data %>%
      inner_join(parameter_id, by = c("Observation" = "Description")) %>%
      arrange(desc(abs(.data$LogRatio)), desc(.data$Expected))
    # check observed counts versus expected counts
    display(verbose, ": observed > 0 vs fit", FALSE)
    high_ratio <- data %>%
      select(
        "Analysis", "Parameter", "Observation", "Datafield", "LogRatio"
      ) %>%
      filter(is.finite(.data$LogRatio), .data$LogRatio > log_expected_ratio) %>%
      select(-"LogRatio") %>%
      head(n)
    if (nrow(high_ratio) > 0) {
      anomaly <- anomaly_type %>%
        filter(.data$Description == "Large ratio of observed vs expected") %>%
        select(AnomalyType = .data$Fingerprint) %>%
        merge(high_ratio) %>%
        bind_rows(anomaly)
    }
    low_ratio <- data %>%
      select(
        "Analysis", "Parameter", "Observation", "Datafield", "LogRatio"
      ) %>%
      filter(
        is.finite(.data$LogRatio),
        .data$LogRatio < -log_expected_ratio
      ) %>%
      select(-"LogRatio") %>%
      head(n)
    if (nrow(low_ratio) > 0) {
      anomaly <- anomaly_type %>%
        filter(.data$Description == "Small ratio of observed vs expected") %>%
        select(AnomalyType = "Fingerprint") %>%
        merge(low_ratio) %>%
        bind_rows(anomaly)
    }

    display(verbose, ", observed == 0 vs fit", FALSE)
    high_absent <- data %>%
      select(
        "Analysis", "Parameter", "Observation", "Datafield", "Expected",
        "Response"
      ) %>%
      filter(.data$Response == 0, .data$Expected > log_expected_absent) %>%
      select(-"Response", -"Expected") %>%
      head(n)
    if (nrow(high_absent) > 0) {
      anomaly <- anomaly_type %>%
        filter(.data$Description == "Zero observed and high expected") %>%
        select(AnomalyType = "Fingerprint") %>%
        merge(high_absent) %>%
        bind_rows(anomaly)
    }
    # select anomalies on random effects
    display(verbose, ", random effect")
    re_anomaly <- parameter@Parameter %>%
      filter(.data$Description == "Random effect BLUP") %>%
      select(Parent = "Fingerprint") %>%
      inner_join(parameter@Parameter, by = "Parent") %>%
      transmute(
        AnomalyType = paste(.data$Description, "random intercept"),
        Parent = .data$Fingerprint
      ) %>%
      inner_join(parameter@Parameter, by = "Parent") %>%
      select(.data$AnomalyType, Parameter = .data$Fingerprint) %>%
      inner_join(
        parameter@ParameterEstimate %>%
          filter(abs(.data$Estimate) > random_threshold) %>%
          select("Analysis", "Parameter", "Estimate"),
        by = "Parameter"
      )
    if (nrow(re_anomaly) > 0) {
      re_anomaly <- re_anomaly %>%
        mutate(Sign = sign(.data$Estimate)) %>%
        arrange(desc(abs(.data$Estimate))) %>%
        group_by(.data$AnomalyType, .data$Sign) %>%
        slice(seq_len(n)) %>%
        ungroup() %>%
        select(-"Sign", -"Estimate")
      anomaly_type <- re_anomaly %>%
        distinct(.data$AnomalyType) %>%
        select(Description = "AnomalyType") %>%
        mutate(
          Fingerprint = map_chr(
            .data$Description,
            ~sha1(c(Description = .x))
          )
        ) %>%
        bind_rows(anomaly_type)
      anomaly <- re_anomaly %>%
        inner_join(anomaly_type, by = c("AnomalyType" = "Description")) %>%
        select(-"AnomalyType", AnomalyType = "Fingerprint") %>%
        bind_rows(anomaly)
    }

    if (!is.null(analysis@RawImputed)) {
      parent <- anomaly_type %>%
        filter(.data$Description == "Unstable imputations")
      imputations <- parameter@Parameter %>%
        filter(.data$Description == "Imputed value") %>%
        semi_join(
          x = parameter@Parameter,
          by = c("Parent" = "Fingerprint")
        ) %>%
        select("Fingerprint", Observation = "Description") %>%
        inner_join(
          x = parameter@ParameterEstimate,
          by = c("Parameter" = "Fingerprint")
        ) %>%
        mutate(AnomalyType = parent$Fingerprint)
      anomaly <- anomaly %>%
        bind_rows(
          imputations %>%
            filter(.data$LowerConfidenceLimit == 0) %>%
            arrange(.data$UpperConfidenceLimit) %>%
            tail(n) %>%
            select("Parameter", "Analysis", "AnomalyType", "Observation"),
          imputations %>%
            filter(.data$LowerConfidenceLimit > 0) %>%
            mutate(
              Ratio = .data$UpperConfidenceLimit / .data$LowerConfidenceLimit
            ) %>%
            arrange(.data$Ratio) %>%
            tail(n) %>%
            select("Parameter", "Analysis", "AnomalyType", "Observation")
        )
    }

    return(
      new(
        "n2kAnomaly",
        Parameter = parameter@Parameter,
        ParameterEstimate = parameter@ParameterEstimate,
        AnomalyType = as.data.frame(anomaly_type),
        Anomaly = as.data.frame(anomaly)
      )
    )
  }
)
