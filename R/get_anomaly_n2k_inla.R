#' @rdname get_anomaly
#' @aliases get_anomaly,n2kInla-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.number is.string
#' @importFrom dplyr arrange bind_cols distinct filter group_by mutate select
#' slice tibble transmute ungroup
#' @importFrom rlang !!
#' @importFrom digest sha1
#' @importFrom utils head tail
#' @include n2k_inla_class.R
#' @param expected_ratio Observations that have
#' `observed / fitted > expected_ratio` or `fitted / observed > expected_ratio`
#' are potential anomalies.
#' Defaults to `5`, which implies that observed values that are 5 times higher
#' or lower than the fitted values are potential anomalies.
#' @param expected_absent Zero observations where `fitted > expected_absent` are
#' potential anomalies.
#' @param n the number of anomalies per category.
#' @param random_threshold The minimal relative effect size of a random effect.
#' Random effect with a smaller effect size will never be an anomaly.
#' Defaults to 1.05 (5%).
#' @inheritParams get_result
setMethod(
  f = "get_anomaly",
  signature = signature(analysis = "n2kInla"),
  definition = function(
    analysis, n = 20, expected_ratio = 5, expected_absent = 5,
    random_threshold = 1.05, verbose = TRUE, ...
  ) {
    assert_that(is.count(n))
    if (!is.null(analysis@Model)) {
      assert_that(
        length(analysis@Model$.args$family) == 1,
        msg = "multiple response not handled yet"
      )
      if (
        analysis@Model$.args$family %in% c(
          "poisson", "zeroinflatedpoisson0", "zeroinflatedpoisson1",
          "nbinomial", "zeroinflatednbinomial0", "zeroinflatednbinomial1"
        )
      ) {
        assert_that(is.number(expected_ratio), expected_ratio > 1)
        log_expected_ratio <- log(expected_ratio)
        assert_that(is.number(expected_absent), expected_absent > 1)
        log_expected_absent <- log(expected_absent)
        assert_that(is.number(random_threshold), random_threshold > 1)
        random_threshold <- log(random_threshold)
      } else {
        stop(analysis@Model$.args$family, " not handled yet")
      }
    }

    parameter <- get_model_parameter(
      analysis = analysis, verbose = verbose, ...
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
      description = c(
        "Large ratio of observed vs expected",
        "Small ratio of observed vs expected",
        "Zero observed and high expected", "Unstable imputations"
      )
    ) %>%
      mutate(
        fingerprint = map_chr(.data$description, ~sha1(c(description = .x)))
      )
    anomaly <- tibble(
      anomaly_type = character(0), analysis = character(0),
      parameter = character(0), observation = character(0),
      datafield = character(0)
    )

    response <- as.character(get_model(analysis)$.args$formula[2])
    get_data(analysis) %>%
      transmute(
        response = !!as.name(response),
        expected = get_model(analysis)$summary.fitted.values[, "mean"],
        log_ratio = log(.data$response / .data$expected),
        analysis = get_file_fingerprint(analysis),
        observation = as.character(.data$observation_id),
        datafield = as.character(.data$datafield_id)
      ) -> data

    parameter_id <- parameter@Parameter %>%
      filter(.data$description == "Fitted") %>%
      select(.data$fingerprint) %>%
      inner_join(parameter@Parameter, by = c("fingerprint" = "parent")) %>%
      select(parameter = .data$fingerprint.y, .data$description)
    length_antijoin <- data %>%
      anti_join(parameter_id, by = c("observation" = "description")) %>%
      nrow()
    assert_that(length_antijoin == 0)

    data <- data %>%
      inner_join(parameter_id, by = c("observation" = "description")) %>%
      arrange(desc(abs(.data$log_ratio)), desc(.data$expected))
    # check observed counts versus expected counts
    display(verbose, ": observed > 0 vs fit", FALSE)
    high_ratio <- data %>%
      select(
        "analysis", "parameter", "observation", "datafield", "log_ratio"
      ) %>%
      filter(
        is.finite(.data$log_ratio), .data$log_ratio > log_expected_ratio
      ) %>%
      select(-"log_ratio") %>%
      head(n)
    if (nrow(high_ratio) > 0) {
      anomaly <- anomaly_type %>%
        filter(.data$description == "Large ratio of observed vs expected") %>%
        select(anomaly_type = .data$fingerprint) %>%
        merge(high_ratio) %>%
        bind_rows(anomaly)
    }
    low_ratio <- data %>%
      select(
        "analysis", "parameter", "observation", "datafield", "log_ratio"
      ) %>%
      filter(
        is.finite(.data$log_ratio), .data$log_ratio < -log_expected_ratio
      ) %>%
      select(-"log_ratio") %>%
      head(n)
    if (nrow(low_ratio) > 0) {
      anomaly <- anomaly_type %>%
        filter(.data$description == "Small ratio of observed vs expected") %>%
        select(anomaly_type = "fingerprint") %>%
        merge(low_ratio) %>%
        bind_rows(anomaly)
    }

    display(verbose, ", observed == 0 vs fit", FALSE)
    high_absent <- data %>%
      select(
        "analysis", "parameter", "observation", "datafield", "expected",
        "response"
      ) %>%
      filter(.data$response == 0, .data$expected > log_expected_absent) %>%
      select(-"response", -"expected") %>%
      head(n)
    if (nrow(high_absent) > 0) {
      anomaly <- anomaly_type %>%
        filter(.data$description == "Zero observed and high expected") %>%
        select(anomaly_type = "fingerprint") %>%
        merge(high_absent) %>%
        bind_rows(anomaly)
    }
    # select anomalies on random effects
    display(verbose, ", random effect")
    re_anomaly <- parameter@Parameter %>%
      filter(.data$description == "Random effect BLUP") %>%
      select(parent = "fingerprint") %>%
      inner_join(parameter@Parameter, by = "parent") %>%
      transmute(
        anomaly_type = paste(.data$description, "random intercept"),
        parent = .data$fingerprint
      ) %>%
      inner_join(parameter@Parameter, by = "parent") %>%
      select(.data$anomaly_type, parameter = .data$fingerprint) %>%
      inner_join(
        parameter@ParameterEstimate %>%
          filter(abs(.data$estimate) > random_threshold) %>%
          select("analysis", "parameter", "estimate"),
        by = "parameter"
      )
    if (nrow(re_anomaly) > 0) {
      re_anomaly <- re_anomaly %>%
        mutate(sign = sign(.data$estimate)) %>%
        arrange(desc(abs(.data$estimate))) %>%
        group_by(.data$anomaly_type, .data$sign) %>%
        slice(seq_len(n)) %>%
        ungroup() %>%
        select(-"sign", -"estimate")
      anomaly_type <- re_anomaly %>%
        distinct(.data$anomaly_type) %>%
        select(description = "anomaly_type") %>%
        mutate(
          fingerprint = map_chr(
            .data$description,
            ~sha1(c(description = .x))
          )
        ) %>%
        bind_rows(anomaly_type)
      anomaly <- re_anomaly %>%
        inner_join(anomaly_type, by = c("anomaly_type" = "description")) %>%
        select(-"anomaly_type", anomaly_type = "fingerprint") %>%
        bind_rows(anomaly)
    }

    if (!is.null(analysis@RawImputed)) {
      parent <- anomaly_type %>%
        filter(.data$description == "Unstable imputations")
      imputations <- parameter@Parameter %>%
        filter(.data$description == "Imputed value") %>%
        semi_join(
          x = parameter@Parameter,
          by = c("parent" = "fingerprint")
        ) %>%
        select("fingerprint", observation = "description") %>%
        inner_join(
          x = parameter@ParameterEstimate,
          by = c("parameter" = "fingerprint")
        ) %>%
        mutate(anomaly_type = parent$fingerprint)
      anomaly <- anomaly %>%
        bind_rows(
          imputations %>%
            filter(.data$LowerConfidenceLimit == 0) %>%
            arrange(.data$UpperConfidenceLimit) %>%
            tail(n) %>%
            select("parameter", "analysis", "anomaly_type", "observation"),
          imputations %>%
            filter(.data$LowerConfidenceLimit > 0) %>%
            mutate(
              Ratio = .data$UpperConfidenceLimit / .data$LowerConfidenceLimit
            ) %>%
            arrange(.data$Ratio) %>%
            tail(n) %>%
            select("parameter", "analysis", "anomaly_type", "observation")
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
