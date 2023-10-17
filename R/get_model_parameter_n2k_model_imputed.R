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
    if (is.null(analysis@Results) || nrow(analysis@Results) == 0) {
      return(new("n2kParameter"))
    }
    parent <- data.frame(
      description = "ModelImputed", parent = NA_character_,
      fingerprint = sha1(c("ModelImputed", NA_character_)),
      stringsAsFactors = FALSE
    )
    parameter <- data.frame(
      description = as.character(analysis@Results$Parameter),
      parent = parent$fingerprint,
      stringsAsFactors = FALSE
    ) %>%
      mutate(fingerprint = map2_chr(
        .data$parent, .data$description, ~sha1(c(.x, .y)))
      )
    new(
      "n2kParameter",
      Parameter = bind_rows(parent, parameter),
      ParameterEstimate = parameter %>%
        inner_join(
          analysis@Results %>%
            mutate(
              Parameter = as.character(.data$Parameter)),
          by = c("description" = "Parameter")
        ) %>%
        transmute(
          analysis = get_file_fingerprint(analysis),
          parameter = .data$fingerprint, estimate = .data$Estimate,
          lower_confidence_limit = .data$LCL, upper_confidence_limit = .data$UCL
        ) %>%
        as.data.frame()
    )
  }
)
