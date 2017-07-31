#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @include n2kModelImputed_class.R
#' @include n2kParameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kModelImputed"),
  definition = function(analysis, ...){
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
      rowwise() %>%
      mutate_(Fingerprint = ~sha1(c(Description, Parent)))
    new(
      "n2kParameter",
      Parameter = bind_rows(
        parent,
        parameter
      ),
      ParameterEstimate = parameter %>%
        inner_join(
          analysis@Results %>%
            mutate_(Parameter = ~as.character(Parameter)),
          by = c("Description" = "Parameter")
        ) %>%
        transmute_(
          Analysis = ~get_file_fingerprint(analysis),
          Parameter = ~Fingerprint,
          ~Estimate,
          LowerConfidenceLimit = ~LCL,
          UpperConfidenceLimit = ~UCL
        ) %>%
        as.data.frame()
    )
  }
)
