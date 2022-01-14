#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @importFrom tibble tibble rownames_to_column
#' @importFrom dplyr %>% bind_rows distinct filter left_join mutate n n_distinct
#' pull select semi_join transmute
#' @importFrom rlang .data
#' @importFrom purrr map_chr map2_chr map_df
#' @importFrom digest sha1
#' @importFrom INLA inla.tmarginal inla.qmarginal
#' @importFrom stats terms
#' @include n2kInla_class.R
#' @include n2kParameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kInla"),
  definition = function(analysis, verbose = TRUE, ...) {
    if (analysis@AnalysisMetadata$Status != "converged") {
      return(new("n2kParameter"))
    }
    parameter <- tibble(
      Description = c(
        "Fixed effect", "Random effect BLUP", "Random effect variance",
        "Fitted", "Overdispersion", "WAIC", "Imputed value"
      ),
      Parent = NA_character_
    ) %>%
      mutate(
        Fingerprint = map_chr(
          .data$Description,
          ~sha1(c(Description = .x, Parent = NA_character_))
        )
      )

    # add fixed effect parameters
    display(verbose, "    reading model parameters: fixed effects", FALSE)

    variable <- c(
      "Intercept",
      attr(terms(analysis@AnalysisFormula[[1]]), "term.labels")
    )
    variable <- variable[!grepl("f\\(", variable)]

    get_model(analysis)$summary.fixed %>%
      rownames_to_column("Parameter") %>%
      transmute(
        Analysis = analysis@AnalysisMetadata$FileFingerprint,
        Parameter = gsub("[\\(|\\)]", "", .data$Parameter),
        Estimate = .data$mean,
        LowerConfidenceLimit = .data$`0.025quant`,
        UpperConfidenceLimit = .data$`0.975quant`
      ) -> parameter_estimate
    attr(parameter_estimate, "parameter") <- parameter
    fixed_parent <- parameter$Fingerprint[
      parameter$Description == "Fixed effect"
    ]
    interaction <- grepl(":", variable)

    parameter_estimate <- model_parameter_main(
      parameter_estimate = parameter_estimate,
      main_effect = variable[!interaction],
      fixed_parent = fixed_parent
    )
    parameter_estimate <- model_parameter_interaction(
      parameter_estimate = parameter_estimate,
      interaction = variable[interaction],
      fixed_parent = fixed_parent
    )
    parameter <- attr(parameter_estimate, "parameter")

    # add random effect variance
    display(verbose, ", random effect variance", FALSE)

    re_names <- names(get_model(analysis)$marginals.hyperpar)
    re_names <- re_names[grepl("^Precision for ", re_names)]
    if (length(re_names) > 0) {
      map_df(
        get_model(analysis)$marginals.hyperpar[re_names],
        inla_inverse
      ) %>%
        mutate(
          Parameter = gsub("^Precision for ", "", re_names),
          Analysis = analysis@AnalysisMetadata$FileFingerprint
        ) -> re_variance
      parameter %>%
        filter(
          is.na(.data$Parent),
          .data$Description == "Random effect variance"
        ) %>%
        select(Parent = "Fingerprint") %>%
        merge(
          tibble(Description = re_variance$Parameter)
        ) %>%
        mutate(
          Fingerprint = map2_chr(
            .data$Description,
            .data$Parent,
            ~sha1(c(Description = .x, Parent = .y))
          )
        ) -> extra
      extra %>%
        select(-"Parent") %>%
        inner_join(re_variance, by = c("Description" = "Parameter")) %>%
        select(-"Description", Parameter = "Fingerprint") %>%
        bind_rows(parameter_estimate) -> parameter_estimate
      parameter <- parameter %>% bind_rows(extra)
    }

    # add overdispersion
    if (get_model(analysis)$.args$family == "nbinomial") {
      display(verbose, ", overdispersion", FALSE)
      overdispersion <- get_model(analysis)$summary.hyperpar
      overdispersion <- overdispersion[
        grep("size for the nbinomial observations", rownames(overdispersion)),
      ]
      parent <- parameter %>%
        filter(is.na(.data$Parent), .data$Description == "Overdispersion")
      parameter_estimate <- parameter_estimate %>%
        bind_rows(
          tibble(
            Analysis = analysis@AnalysisMetadata$FileFingerprint,
            Parameter = parent$Fingerprint,
            Estimate = overdispersion[, "mean"],
            LowerConfidenceLimit = overdispersion[, "0.025quant"],
            UpperConfidenceLimit = overdispersion[, "0.975quant"]
          )
        )
    }

    # add WAIC
    display(verbose, ", WAIC", FALSE)
    parent <- parameter %>%
      filter(is.na(.data$Parent), .data$Description == "WAIC")
    parameter_estimate <- parameter_estimate %>%
      bind_rows(
        tibble(
          Analysis = analysis@AnalysisMetadata$FileFingerprint,
          Parameter = parent$Fingerprint,
          Estimate = get_model(analysis)$waic$waic,
          LowerConfidenceLimit = NA_real_,
          UpperConfidenceLimit = NA_real_
        )
      )

    # add random effect BLUP's
    display(verbose, ", random effect BLUP's", FALSE)

    if (length(re_names) > 0) {
      lapply(
        names(get_model(analysis)$summary.random),
        function(i) {
          get_model(analysis)$summary.random[[i]] %>%
            transmute(
              Analysis = analysis@AnalysisMetadata$FileFingerprint,
              Parent = i,
              Parameter = as.character(.data$ID),
              Estimate = .data$mean,
              LowerConfidenceLimit = .data$`0.025quant`,
              UpperConfidenceLimit = .data$`0.975quant`
            ) -> random_effect
          if (anyDuplicated(random_effect$Parameter) == 0) {
            return(random_effect)
          }
          if (is.null(analysis@ReplicateName[[i]])) {
            random_effect %>%
              mutate(
                Replicate = rep(
                  as.character(seq_len(n() / n_distinct(.data$Parameter))),
                  each = n_distinct(.data$Parameter)
                )
              )
          } else {
            random_effect %>%
              mutate(
                Replicate = rep(
                  analysis@ReplicateName[[i]],
                  each = n_distinct(.data$Parameter)
                )
              )
          }
        }
      ) %>%
        bind_rows() -> blup
      blup_fingerprint <- parameter %>%
        semi_join(
          tibble(
            Description = "Random effect BLUP",
            Parent = NA_character_
          ),
          by = c("Description", "Parent")
        ) %>%
        pull(.data$Fingerprint)
      blup_parent <- blup %>%
        select(Original = "Parent") %>%
        distinct() %>%
        mutate(
          Parent = gsub(" .*$", "", .data$Original),
          Description = gsub("^.* ", "", .data$Original),
          Parent = ifelse(
            .data$Parent == .data$Description,
            blup_fingerprint,
            .data$Parent
          ),
          Fingerprint = map2_chr(
            .data$Description,
            .data$Parent,
            ~sha1(c(Description = .x, Parent = .y))
          )
        )
      parameter <- blup_parent %>%
        select(-"Original") %>%
        bind_rows(parameter)
      blup <- blup_parent %>%
        select("Original", Parent = "Fingerprint") %>%
        inner_join(
          blup,
          by = c("Original" = "Parent")
        ) %>%
        select(-"Original")

      if ("Replicate" %in% colnames(blup)) {
        blup_parent <- blup %>%
          filter(!is.na(.data$Replicate)) %>%
          select("Parent", Description = "Replicate") %>%
          distinct() %>%
          mutate(
            Fingerprint = map2_chr(
              .data$Description,
              .data$Parent,
              ~sha1(c(Description = .x, Parent = .y))
            )
          )
        parameter <- bind_rows(parameter, blup_parent)
        blup <- blup %>%
          left_join(
            blup_parent,
            by = c("Replicate" = "Description", "Parent")
          ) %>%
          mutate(
            Parent = ifelse(
              is.na(.data$Fingerprint),
              .data$Parent,
              .data$Fingerprint
            )
          ) %>%
          select(-"Replicate", -"Fingerprint")
      }
      parameter <- blup %>%
        select("Parent", Description = "Parameter") %>%
        distinct() %>%
        mutate(
          Fingerprint = map2_chr(
            .data$Description,
            .data$Parent,
            ~sha1(c(Description = .x, Parent = .y))
          )
        ) %>%
        bind_rows(parameter)
      parameter_estimate <- blup %>%
        inner_join(parameter, by = c("Parent", "Parameter" = "Description")) %>%
        select(-"Parent", -"Parameter", Parameter = "Fingerprint") %>%
        bind_rows(parameter_estimate)
    }

    # add fitted values
    display(verbose, ", fitted values")

    fitted_parent <- parameter %>%
      filter(is.na(.data$Parent), .data$Description == "Fitted") %>%
      pull(.data$Fingerprint)
    get_model(analysis)$summary.fitted.values %>%
      transmute(
        Analysis = analysis@AnalysisMetadata$FileFingerprint,
        Parent = fitted_parent,
        Estimate = .data$mean,
        LowerConfidenceLimit = .data$`0.025quant`,
        UpperConfidenceLimit = ifelse(
          .data$mean > .data$`0.975quant`,
          NA_real_,
          .data$`0.975quant`
        )
      ) %>%
      bind_cols(
        get_data(analysis) %>%
          transmute(Parameter = as.character(.data$ObservationID))
      ) -> fitted
    tibble(
      Description = fitted$Parameter,
      Parent = fitted_parent
    ) %>%
      mutate(
        Fingerprint = map2_chr(
          .data$Description,
          .data$Parent,
          ~sha1(c(Description = .x, Parent = .y))
        )
      ) %>%
      bind_rows(parameter) -> parameter
    tmp <- fitted %>%
      inner_join(parameter, by = c("Parent", "Parameter" = "Description")) %>%
      select(-"Parent", -"Parameter", Parameter = "Fingerprint")
    parameter_estimate <- bind_rows(tmp, parameter_estimate)

    # imputed values
    if (!is.null(analysis@RawImputed)) {
      ri <- analysis@RawImputed
      extra <- ri@Data %>%
        mutate(Response = ri@Response) %>%
        filter(is.na(.data$Response)) %>%
        select(.data$ObservationID) %>%
        mutate(
          Analysis = get_file_fingerprint(analysis),
          Estimate = apply(ri@Imputation, 1, quantile, probs = 0.500),
          LowerConfidenceLimit =
            apply(ri@Imputation, 1, quantile, probs = 0.025),
          UpperConfidenceLimit =
            apply(ri@Imputation, 1, quantile, probs = 0.975)
        )
      parent <- parameter %>%
        filter(.data$Description == "Imputed value", is.na(.data$Parent))
      impute_parameter <- extra %>%
        distinct(.data$ObservationID) %>%
        transmute(
          Parent = parent$Fingerprint,
          Description = .data$ObservationID
        ) %>%
        mutate(
          Fingerprint = map2_chr(
            .data$Description,
            .data$Parent,
            ~sha1(c(Description = .x, Parent = .y))
          )
        )
      parameter <- parameter %>%
        bind_rows(impute_parameter)
      parameter_estimate <- extra %>%
        inner_join(
          impute_parameter,
          by = c("ObservationID" = "Description")
        ) %>%
        select(
          "Analysis",
          "Estimate",
          "LowerConfidenceLimit",
          "UpperConfidenceLimit",
          Parameter = "Fingerprint"
        ) %>%
        bind_rows(parameter_estimate)
    }

    new(
      "n2kParameter",
      Parameter = as.data.frame(parameter),
      ParameterEstimate = as.data.frame(parameter_estimate)
    )
  }
)

model_parameter_main <- function(
  parameter_estimate, main_effect, fixed_parent
) {
  attr(parameter_estimate, "parameter") -> parameter
  for (i in main_effect) {
    present <- grep(paste0("^", i), parameter_estimate$Parameter)
    present <- present[!grepl(":", parameter_estimate$Parameter[present])]
    if (length(present) == 0) {
      next
    }
    extra <- tibble(
      Description = i,
      Parent = fixed_parent
    ) %>%
      mutate(
        Fingerprint = map2_chr(
          .data$Description,
          .data$Parent,
          ~sha1(c(Description = .x, Parent = .y))
        )
      )
    extra_factor <- tibble(
      Description = gsub(
        paste0("^", i),
        "",
        parameter_estimate$Parameter[present]
      ),
      Parent = extra$Fingerprint
    ) %>%
      filter(.data$Description != "")
    if (nrow(extra_factor) == 0) {
      to_merge <- extra %>%
        select(-"Parent")
    } else {
      extra_factor <- extra_factor %>%
        mutate(
          Fingerprint = map2_chr(
            .data$Description,
            .data$Parent,
            ~sha1(c(Description = .x, Parent = .y))
          )
        )
      extra_factor %>%
        select(-"Parent") %>%
        mutate(Description = paste0(i, .data$Description)) -> to_merge
    }
    left_join(
      parameter_estimate,
      to_merge,
      by = c("Parameter" = "Description")
    ) %>%
      mutate(
        Parameter = ifelse(
          is.na(.data$Fingerprint), .data$Parameter, .data$Fingerprint
        )
      ) %>%
      select(-"Fingerprint") -> parameter_estimate
    parameter <- bind_rows(parameter, extra, extra_factor)
  }
  attr(parameter_estimate, "parameter") <- parameter
  return(parameter_estimate)
}

model_parameter_interaction <- function(
  parameter_estimate, interaction, fixed_parent
) {
  attr(parameter_estimate, "parameter") -> parameter
  for (i in interaction) {
    pattern <- paste0("^", gsub(":", ".*:", i))
    present <- grep(pattern, parameter_estimate$Parameter)
    if (length(present) == 0) {
      next
    }
    extra <- tibble(
      Description = i,
      Parent = fixed_parent
    ) %>%
      mutate(
        Fingerprint = map2_chr(
          .data$Description,
          .data$Parent,
          ~sha1(c(Description = .x, Parent = .y))
        )
      )
    parts <- strsplit(i, ":")[[1]]
    level_name <- gsub(
      paste0("^", parts[1]),
      "",
      parameter_estimate$Parameter[present]
    )
    for (j in parts[-1]) {
      level_name <- gsub(paste0(":", j), ":", level_name)
    }
    extra_factor <- tibble(
      Description = level_name,
      Parent = extra$Fingerprint
    ) %>%
      filter(!grepl("^:*$", .data$Description)) %>%
      mutate(
        Fingerprint = map2_chr(
          .data$Description,
          .data$Parent,
          ~sha1(c(Description = .x, Parent = .y))
        )
      )
    if (nrow(extra_factor) > 0) {
      to_merge <- extra_factor %>%
        select(-"Parent") %>%
        inner_join(
          tibble(
            Description = level_name,
            Original = parameter_estimate$Parameter[present]
          ),
          by = "Description"
        ) %>%
        select(
          -"Description",
          Description = "Original"
        )
    } else {
      to_merge <- extra %>%
        select(-"Parent")
    }
    parameter_estimate <- left_join(
      parameter_estimate,
      to_merge,
      by = c("Parameter" = "Description")
    ) %>%
      mutate(
        Parameter = ifelse(
          is.na(.data$Fingerprint),
          .data$Parameter,
          .data$Fingerprint
        )
      ) %>%
      select(-"Fingerprint")
    parameter <- bind_rows(parameter, extra, extra_factor)
  }
  attr(parameter_estimate, "parameter") <- parameter
  return(parameter_estimate)
}
