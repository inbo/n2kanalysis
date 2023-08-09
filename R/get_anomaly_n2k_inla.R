#' @rdname get_anomaly
#' @aliases get_anomaly,n2kInla-methods
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that is.count is.number is.string
#' @importFrom dplyr arrange bind_cols bind_rows distinct filter group_by mutate
#' select slice_head slice_max tibble transmute ungroup
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
    assert_that(
      is.count(n), is.number(expected_ratio), expected_ratio > 1,
      is.number(expected_absent), expected_absent > 1,
      is.number(random_threshold), random_threshold > 1
    )
    if (!is.null(analysis@Model)) {
      assert_that(
        length(analysis@Model$.args$family) == 1,
        msg = "multiple response not handled yet"
      )
      assert_that(
        analysis@Model$.args$family %in% c(
          "binomial", "nbinomial", "poisson", "zeroinflatednbinomial0",
          "zeroinflatednbinomial1", "zeroinflatedpoisson0",
          "zeroinflatedpoisson1"
        ),
        msg = paste(analysis@Model$.args$family, "not handled yet")
      )
      log_expected_ratio <- log(expected_ratio)
      log_expected_absent <- log(expected_absent)
      random_threshold <- log(random_threshold)
    }

    parameter <- get_model_parameter(
      analysis = analysis, verbose = verbose, ...
    )
    if (status(analysis) != "converged") {
      return(
        new(
          "n2kAnomaly", Parameter = parameter@Parameter,
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
    ) |>
      mutate(
        fingerprint = map_chr(.data$description, ~sha1(c(description = .x)))
      )
    anomaly <- tibble(
      anomaly_type = character(0), analysis = character(0),
      parameter = character(0), observation = character(0),
      datafield = character(0)
    )

    response <- as.character(get_model(analysis)$.args$formula[2])
    get_data(analysis) |>
      transmute(
        response = !!as.name(response),
        expected = get_model(analysis)$summary.fitted.values[, "mean"],
        log_ratio = log(.data$response / .data$expected),
        analysis = get_file_fingerprint(analysis),
        observation = as.character(.data$observation_id),
        datafield = as.character(.data$datafield_id)
      ) -> data

    parameter_id <- parameter@Parameter |>
      filter(.data$description == "Fitted") |>
      select("fingerprint") |>
      inner_join(parameter@Parameter, by = c("fingerprint" = "parent")) |>
      select(parameter = "fingerprint.y", "description")
    length_antijoin <- data |>
      anti_join(parameter_id, by = c("observation" = "description")) |>
      nrow()
    assert_that(length_antijoin == 0)

    data <- data |>
      inner_join(parameter_id, by = c("observation" = "description")) |>
      arrange(desc(abs(.data$log_ratio)), desc(.data$expected))
    # check observed counts versus expected counts
    display(verbose, ": observed > 0 vs fit", FALSE)
    data |>
      filter(
        is.finite(.data$log_ratio), .data$log_ratio > log_expected_ratio
      ) |>
      select("analysis", "parameter", "observation", "datafield") |>
      slice_head(n = n) |>
      bind_cols(
        anomaly_type |>
          filter(.data$description == "Large ratio of observed vs expected") |>
          select(anomaly_type = "fingerprint")
      ) |>
      bind_rows(anomaly) -> anomaly
    data |>
      filter(
        is.finite(.data$log_ratio), .data$log_ratio < -log_expected_ratio
      ) |>
      select("analysis", "parameter", "observation", "datafield") |>
      slice_head(n = n) |>
      bind_cols(
        anomaly_type |>
          filter(.data$description == "Small ratio of observed vs expected") |>
          select(anomaly_type = "fingerprint")
      ) |>
      bind_rows(anomaly) -> anomaly

    display(verbose, ", observed == 0 vs fit", FALSE)
    data |>
      filter(.data$response == 0, .data$expected > log_expected_absent) |>
      select("analysis", "parameter", "observation", "datafield") |>
      slice_head(n = n) |>
      bind_cols(
        anomaly_type |>
          filter(.data$description == "Zero observed and high expected") |>
          select(anomaly_type = "fingerprint")
      ) |>
      bind_rows(anomaly) -> anomaly

    # select anomalies on random effects
    display(verbose, ", random effect")
    parameter@Parameter |>
      filter(.data$description == "Random effect BLUP") |>
      select(parent = "fingerprint") |>
      inner_join(parameter@Parameter, by = "parent") |>
      transmute(
        anomaly_type = paste(.data$description, "random intercept"),
        parent = .data$fingerprint
      ) |>
      inner_join(parameter@Parameter, by = "parent") |>
      select("anomaly_type", parameter = "fingerprint") |>
      inner_join(
        parameter@ParameterEstimate |>
          filter(abs(.data$estimate) > random_threshold) |>
          select("analysis", "parameter", "estimate"),
        by = "parameter"
      ) -> re_anomaly
    if (nrow(re_anomaly) > 0) {
      re_anomaly |>
        mutate(sign = sign(.data$estimate)) |>
        group_by(.data$anomaly_type, .data$sign) |>
        slice_max(abs(.data$estimate), n = n) |>
        ungroup() |>
        select(-"sign", -"estimate") -> re_anomaly
      anomaly_type <- re_anomaly |>
        distinct(.data$anomaly_type) |>
        select(description = "anomaly_type") |>
        mutate(
          fingerprint = map_chr(.data$description, ~sha1(c(description = .x)))
        ) |>
        bind_rows(anomaly_type)
      anomaly <- re_anomaly |>
        inner_join(anomaly_type, by = c("anomaly_type" = "description")) |>
        select(-"anomaly_type", anomaly_type = "fingerprint") |>
        bind_rows(anomaly)
    }

    if (!is.null(analysis@RawImputed)) {
      parent <- anomaly_type |>
        filter(.data$description == "Unstable imputations")
      imputations <- parameter@Parameter |>
        filter(.data$description == "Imputed value") |>
        semi_join(x = parameter@Parameter, by = c("parent" = "fingerprint")) |>
        select("fingerprint", observation = "description") |>
        inner_join(
          x = parameter@ParameterEstimate, by = c("parameter" = "fingerprint")
        ) |>
        mutate(anomaly_type = parent$fingerprint)
      anomaly <- anomaly |>
        bind_rows(
          imputations |>
            filter(.data$lower_confidence_limit == 0) |>
            slice_max(.data$upper_confidence_limit, n = n) |>
            select("parameter", "analysis", "anomaly_type", "observation"),
          imputations |>
            filter(.data$lower_confidence_limit > 0) |>
            mutate(
              ratio = .data$upper_confidence_limit /
                .data$lower_confidence_limit
            ) |>
            slice_max(.data$ratio, n = n) |>
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
