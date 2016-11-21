#' The n2kAnomaly class
#' @name n2kAnomaly-class
#' @rdname n2kAnomaly-class
#' @exportClass n2kAnomaly
#' @aliases n2kAnomaly-class
#' @importFrom methods setClass
#' @docType class
#' @include n2kParameter_class.R
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
      DatasourceID = character(0),
      Datafield = character(0),
      stringsAsFactors = FALSE
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom n2khelper is.chartor
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% anti_join select_
setValidity(
  "n2kAnomaly",
  function(object){
    assert_that(has_name(object@AnomalyType, "Description"))
    assert_that(has_name(object@AnomalyType, "Fingerprint"))

    assert_that(is.chartor(object@AnomalyType$Description))
    assert_that(is.chartor(object@AnomalyType$Fingerprint))

    assert_that(has_name(object@Anomaly, "AnomalyType"))
    assert_that(has_name(object@Anomaly, "Analysis"))
    assert_that(has_name(object@Anomaly, "Parameter"))
    assert_that(has_name(object@Anomaly, "DatasourceID"))
    assert_that(has_name(object@Anomaly, "Datafield"))

    assert_that(is.chartor(object@Anomaly$AnomalyType))
    assert_that(is.chartor(object@Anomaly$Analysis))
    assert_that(is.chartor(object@Anomaly$Parameter))
    assert_that(is.chartor(object@Anomaly$DatasourceID))
    assert_that(is.chartor(object@Anomaly$Datafield))

    antijoin.anomalytype <- object@Anomaly %>%
      anti_join(object@AnomalyType, by = c("AnomalyType" = "Fingerprint")) %>%
      nrow()
    if (antijoin.anomalytype > 0) {
      stop("Some Anomaly have no matching Fingerprint in 'AnomalyType'")
    }

    antijoin.anomaly <- object@Anomaly %>%
      anti_join(object@ParameterEstimate, by = c("Analysis", "Parameter")) %>%
      nrow()
    if (antijoin.anomaly > 0) {
      stop(
"Mismatch on Analysis and Parameter between Anomaly and ParameterEstimate slot"
      )
    }

    anomalytype.duplicate <- object@AnomalyType %>%
      select_(~Fingerprint) %>%
      anyDuplicated()
    if (anomalytype.duplicate > 0) {
      stop("Duplicated anomalytypes")
    }

    anomaly.duplicate <- object@Anomaly %>%
      select_(~Analysis, ~AnomalyType, ~Parameter) %>%
      anyDuplicated()
    if (anomaly.duplicate > 0) {
      stop("Duplicated anomalies")
    }
    return(TRUE)
  }
)
