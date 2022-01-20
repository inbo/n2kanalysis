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
      Description = "ModelImputed",
      Parent = NA_character_,
      Fingerprint = sha1(c("ModelImputed", NA_character_)),
      stringsAsFactors = FALSE
    )
    parameter <- data.frame(
      Description = as.character(analysis@Results$Parameter),
      Parent = parent$Fingerprint,
      stringsAsFactors = FALSE
    ) %>%
      mutate(Fingerprint = map2_chr(
        .data$Description,
        .data$Parent,
        ~sha1(c(Description, Parent)))
      )
    new(
      "n2kParameter",
      Parameter = bind_rows(
        parent,
        parameter
      ),
      ParameterEstimate = parameter %>%
        inner_join(
          analysis@Results %>%
            mutate(Parameter = as.character(.data$Parameter)),
          by = c("Description" = "Parameter")
        ) %>%
        transmute(
          Analysis = get_file_fingerprint(analysis),
          Parameter = .data$Fingerprint,
          .data$Estimate,
          LowerConfidenceLimit = .data$LCL,
          UpperConfidenceLimit = .data$UCL
        ) %>%
        as.data.frame()
    )
  }
)
