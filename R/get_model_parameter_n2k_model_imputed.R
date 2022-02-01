#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @importFrom dplyr transmute mutate
#' @importFrom purrr map2_chr
#' @include n2k_model_imputed_class.R
#' @include n2k_parameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kModelImputed"),
  definition = function(analysis, ...) {
    parent <- data.frame(
      description = "ModelImputed", parent = NA_character_,
      fingerprint = sha1(c("ModelImputed", NA_character_)),
      stringsAsFactors = FALSE
    )
    parameter <- data.frame(
      description = as.character(analysis@Results$parameter),
      parent = parent$fingerprint,
      stringsAsFactors = FALSE
    ) %>%
      mutate(fingerprint = map2_chr(
        .data$description, .data$parent, ~sha1(c(description, parent)))
      )
    new(
      "n2kParameter",
      Parameter = bind_rows(parent, parameter),
      ParameterEstimate = parameter %>%
        inner_join(
          analysis@Results %>%
            mutate(parameter = as.character(.data$parameter)),
          by = c("description" = "parameter")
        ) %>%
        transmute(
          analysis = get_file_fingerprint(analysis),
          parameter = .data$fingerprint,
          .data$estimate,
          lower_confidence_limit = .data$LCL,
          upper_confidence_limit = .data$UCL
        ) %>%
        as.data.frame()
    )
  }
)
