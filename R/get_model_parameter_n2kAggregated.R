#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @importFrom dplyr mutate_all funs select transmute mutate rename
#' @importFrom purrr map2_chr
#' @importFrom stats quantile
#' @include n2kAggregate_class.R
#' @include n2kParameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kAggregate"),
  definition = function(analysis, ...){
    if (status(analysis) != "converged") {
      return(new("n2kParameter"))
    }

    parameter <- data.frame(
      Description = "AggregatedImputed",
      Parent = NA_character_,
      Fingerprint = sha1(c("AggregatedImputed", NA_character_)),
      stringsAsFactors = FALSE
    )
    observations <- analysis@AggregatedImputed@Covariate %>%
      mutate_all(funs("as.character")) %>%
      mutate(Parent = .data$parameter$Fingerprint)
    for (i in colnames(analysis@AggregatedImputed@Covariate)) {
      extra <- observations %>%
        distinct_(~Parent) %>%
        mutate(Description = i) %>%
        mutate(
          Fingerprint = map2_chr(
            .data$Description,
            .data$Parent,
            ~sha1(c(Description = .x, Parent = .y))
          )
        )
      observations <- observations %>%
        inner_join(
          extra %>%
            select(.data$Parent, .data$Fingerprint),
          by = "Parent"
        ) %>%
        rename(Parent = "Fingerprint")
      parameter <- bind_rows(parameter, extra)
      extra <- observations %>%
        distinct_(~Parent, i) %>%
        transmute(
          .data$Parent,
          Description = i
        ) %>%
        mutate(
          Fingerprint = map2(
            .data$Description,
            .data$Parent,
            ~sha1(c(Description = .x, Parent = .y))
          )
        )
      link <- c("Parent", "Description")
      names(link) <- c("Parent", i)
      observations <- observations %>%
        inner_join(extra, by = link) %>%
        rename(Parent = "Fingerprint")
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
          Estimate = 1,
          LowerConfidenceLimit = 2,
          UpperConfidenceLimit = 3
        ) %>%
        mutate(
          Analysis = get_file_fingerprint(analysis),
          Parameter = extra$Fingerprint
        )
    )
  }
)
