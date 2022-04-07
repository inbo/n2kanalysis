#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @importFrom dplyr across distinct mutate rename select transmute
#' @importFrom purrr map2_chr
#' @importFrom rlang .data !!
#' @importFrom stats quantile
#' @include n2k_aggregate_class.R
#' @include n2k_parameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kAggregate"),
  definition = function(analysis, ...) {
    if (status(analysis) != "converged") {
      return(new("n2kParameter"))
    }

    parameter <- data.frame(
      description = "AggregatedImputed", parent = NA_character_,
      fingerprint = sha1(c("AggregatedImputed", NA_character_)),
      stringsAsFactors = FALSE
    )
    observations <- analysis@AggregatedImputed@Covariate %>%
      mutate(
        across(.fns = as.character),
        parent = parameter$fingerprint
      )
    for (i in colnames(analysis@AggregatedImputed@Covariate)) {
      extra <- observations %>%
        distinct(.data$parent) %>%
        mutate(
          description = i,
          fingerprint = map2_chr(
            .data$description, .data$parent,
            ~sha1(c(description = .x, parent = .y))
          )
        )
      observations <- observations %>%
        inner_join(
          extra %>%
            select(.data$parent, .data$fingerprint),
          by = "parent"
        ) %>%
        select(-.data$parent, parent = .data$fingerprint)
      parameter <- bind_rows(parameter, extra)
      extra <- observations %>%
        select(.data$parent, description = !!i) %>%
        distinct() %>%
        mutate(
          fingerprint = map2_chr(
            .data$description, .data$parent,
            ~sha1(c(description = .x, parent = .y))
          )
        )
      link <- c("parent", "description")
      names(link) <- c("parent", i)
      observations <- observations %>%
        inner_join(extra, by = link) %>%
        select(-.data$parent, parent = .data$fingerprint)
      parameter <- bind_rows(parameter, extra)
    }
    new(
      "n2kParameter",
      Parameter = parameter,
      ParameterEstimate = analysis@AggregatedImputed@Imputation %>%
        apply(1, quantile, c(0.5, 0.025, 0.975)) %>%
        t() %>%
        as.data.frame() %>%
        select(
          estimate = 1, lower_confidence_limit = 2, upper_confidence_limit = 3
        ) %>%
        mutate(
          analysis = get_file_fingerprint(analysis),
          parameter = extra$fingerprint
        )
    )
  }
)
