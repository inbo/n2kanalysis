#' The n2kAnomaly class
#' @name n2kAnomaly-class
#' @rdname n2kAnomaly-class
#' @exportClass n2kAnomaly
#' @aliases n2kAnomaly-class
#' @importFrom methods setClass
#' @docType class
#' @include n2k_parameter_class.R
setClass(
  "n2kAnomaly",
  representation = representation(
    AnomalyType = "data.frame",
    Anomaly = "data.frame"
  ),
  contains = "n2kParameter",
  prototype = prototype(
    AnomalyType = data.frame(
      Description = character(0),
      Fingerprint = character(0),
      stringsAsFactors = FALSE
    ),
    Anomaly = data.frame(
      AnomalyType = character(0),
      Analysis = character(0),
      Parameter = character(0),
      Observation = character(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom n2khelper is_chartor
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% anti_join select
#' @importFrom rlang .data
setValidity(
  "n2kAnomaly",
  function(object) {
    assert_that(has_name(object@AnomalyType, "Description"))
    assert_that(has_name(object@AnomalyType, "Fingerprint"))

    assert_that(is_chartor(object@AnomalyType$Description))
    assert_that(is_chartor(object@AnomalyType$Fingerprint))

    assert_that(
      anyDuplicated(colnames(object@Anomaly)) == 0,
      msg = "duplicated column names in Anomaly"
    )

    assert_that(has_name(object@Anomaly, "AnomalyType"))
    assert_that(has_name(object@Anomaly, "Analysis"))
    assert_that(has_name(object@Anomaly, "Parameter"))
    assert_that(has_name(object@Anomaly, "Observation"))

    assert_that(is_chartor(object@Anomaly$AnomalyType))
    assert_that(is_chartor(object@Anomaly$Analysis))
    assert_that(is_chartor(object@Anomaly$Parameter))
    assert_that(is_chartor(object@Anomaly$Observation))

    antijoin_anomalytype <- object@Anomaly %>%
      anti_join(object@AnomalyType, by = c("AnomalyType" = "Fingerprint")) %>%
      nrow()
    assert_that(
      antijoin_anomalytype == 0,
      msg = "Some Anomaly have no matching Fingerprint in 'AnomalyType'"
    )

    antijoin_anomaly <- object@Anomaly %>%
      anti_join(object@ParameterEstimate, by = c("Analysis", "Parameter")) %>%
      nrow()
    assert_that(
      antijoin_anomaly == 0, msg =
"Mismatch on Analysis and Parameter between Anomaly and ParameterEstimate slot"
    )

    anomalytype_duplicate <- object@AnomalyType %>%
      select(.data$Fingerprint) %>%
      anyDuplicated()
    assert_that(anomalytype_duplicate == 0, msg = "Duplicated anomalytypes")

    anomaly_duplicate <- object@Anomaly %>%
      select(.data$Analysis, .data$AnomalyType, .data$Parameter) %>%
      anyDuplicated()
    assert_that(anomaly_duplicate == 0, msg = "Duplicated anomalies")
    return(TRUE)
  }
)
