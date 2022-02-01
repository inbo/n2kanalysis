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
      description = character(0),
      fingerprint = character(0),
      stringsAsFactors = FALSE
    ),
    Anomaly = data.frame(
      anomaly_type = character(0),
      analysis = character(0),
      parameter = character(0),
      observation = character(0),
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
    assert_that(has_name(object@AnomalyType, "description"))
    assert_that(has_name(object@AnomalyType, "fingerprint"))

    assert_that(is_chartor(object@AnomalyType$description))
    assert_that(is_chartor(object@AnomalyType$fingerprint))

    assert_that(
      anyDuplicated(colnames(object@Anomaly)) == 0,
      msg = "duplicated column names in Anomaly"
    )

    assert_that(has_name(object@Anomaly, "anomaly_type"))
    assert_that(has_name(object@Anomaly, "analysis"))
    assert_that(has_name(object@Anomaly, "parameter"))
    assert_that(has_name(object@Anomaly, "observation"))

    assert_that(is_chartor(object@Anomaly$anomaly_type))
    assert_that(is_chartor(object@Anomaly$analysis))
    assert_that(is_chartor(object@Anomaly$parameter))
    assert_that(is_chartor(object@Anomaly$observation))

    antijoin_anomalytype <- object@Anomaly %>%
      anti_join(object@AnomalyType, by = c("anomaly_type" = "fingerprint")) %>%
      nrow()
    assert_that(
      antijoin_anomalytype == 0,
      msg = "Some Anomaly have no matching fingerprint in 'AnomalyType'"
    )

    antijoin_anomaly <- object@Anomaly %>%
      anti_join(object@ParameterEstimate, by = c("analysis", "parameter")) %>%
      nrow()
    assert_that(
      antijoin_anomaly == 0, msg =
"Mismatch on Analysis and Parameter between Anomaly and ParameterEstimate slot"
    )

    anomalytype_duplicate <- object@AnomalyType %>%
      select(.data$fingerprint) %>%
      anyDuplicated()
    assert_that(anomalytype_duplicate == 0, msg = "Duplicated anomalytypes")

    anomaly_duplicate <- object@Anomaly %>%
      select(.data$analysis, .data$anomaly_type, .data$parameter) %>%
      anyDuplicated()
    assert_that(anomaly_duplicate == 0, msg = "Duplicated anomalies")
    return(TRUE)
  }
)
