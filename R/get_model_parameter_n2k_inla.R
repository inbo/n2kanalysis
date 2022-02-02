#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @importFrom tibble tibble rownames_to_column
#' @importFrom dplyr %>% bind_rows distinct filter left_join mutate n n_distinct
#' pull select semi_join transmute
#' @importFrom rlang .data
#' @importFrom purrr map_chr map2_chr map_df
#' @importFrom digest sha1
#' @importFrom stats terms
#' @include n2k_inla_class.R
#' @include n2k_parameter_class.R
#' @inheritParams get_result
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kInla"),
  definition = function(analysis, verbose = TRUE, ...) {
    if (analysis@AnalysisMetadata$status != "converged") {
      return(new("n2kParameter"))
    }
    parameter <- tibble(
      description = c(
        "Fixed effect", "Random effect BLUP", "Random effect variance",
        "Fitted", "Overdispersion", "WAIC", "Imputed value"
      ),
      parent = NA_character_
    ) %>%
      mutate(
        fingerprint = map_chr(
          .data$description,
          ~sha1(c(description = .x, parent = NA_character_))
        )
      )

    # add fixed effect parameters
    display(verbose, "    reading model parameters: fixed effects", FALSE)

    variable <- c(
      "Intercept", attr(terms(analysis@AnalysisFormula[[1]]), "term.labels")
    )
    variable <- variable[!grepl("f\\(", variable)]

    get_model(analysis)$summary.fixed %>%
      rownames_to_column("parameter") %>%
      transmute(
        analysis = analysis@AnalysisMetadata$file_fingerprint,
        parameter = gsub("[\\(|\\)]", "", .data$parameter),
        estimate = .data$mean,
        lower_confidence_limit = .data$`0.025quant`,
        upper_confidence_limit = .data$`0.975quant`
      ) -> parameter_estimate
    attr(parameter_estimate, "parameter") <- parameter
    fixed_parent <- parameter$fingerprint[
      parameter$description == "Fixed effect"
    ]
    interaction <- grepl(":", variable)

    parameter_estimate <- model_parameter_main(
      parameter_estimate = parameter_estimate,
      main_effect = variable[!interaction], fixed_parent = fixed_parent
    )
    parameter_estimate <- model_parameter_interaction(
      parameter_estimate = parameter_estimate,
      interaction = variable[interaction], fixed_parent = fixed_parent
    )
    parameter <- attr(parameter_estimate, "parameter")

    # add random effect variance
    display(verbose, ", random effect variance", FALSE)

    re_names <- names(get_model(analysis)$marginals.hyperpar)
    re_names <- re_names[grepl("^Precision for ", re_names)]
    if (length(re_names) > 0) {
      map_df(
        get_model(analysis)$marginals.hyperpar[re_names], inla_inverse
      ) %>%
        mutate(
          parameter = gsub("^Precision for ", "", re_names),
          analysis = analysis@AnalysisMetadata$file_fingerprint
        ) -> re_variance
      parameter %>%
        filter(
          is.na(.data$parent), .data$description == "Random effect variance"
        ) %>%
        select(parent = "fingerprint") %>%
        merge(tibble(description = re_variance$parameter)) %>%
        mutate(
          fingerprint = map2_chr(
            .data$description, .data$parent,
            ~sha1(c(description = .x, parent = .y))
          )
        ) -> extra
      extra %>%
        select(-"parent") %>%
        inner_join(re_variance, by = c("description" = "parameter")) %>%
        select(-"description", parameter = "fingerprint") %>%
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
        filter(is.na(.data$parent), .data$description == "Overdispersion")
      parameter_estimate <- parameter_estimate %>%
        bind_rows(
          tibble(
            analysis = analysis@AnalysisMetadata$file_fingerprint,
            parameter = parent$fingerprint,
            estimate = overdispersion[, "mean"],
            lower_confidence_limit = overdispersion[, "0.025quant"],
            upper_confidence_limit = overdispersion[, "0.975quant"]
          )
        )
    }

    # add WAIC
    display(verbose, ", WAIC", FALSE)
    parent <- parameter %>%
      filter(is.na(.data$parent), .data$description == "WAIC")
    parameter_estimate <- parameter_estimate %>%
      bind_rows(
        tibble(
          estimate = get_model(analysis)$waic$waic,
          analysis = analysis@AnalysisMetadata$file_fingerprint,
          parameter = parent$fingerprint, lower_confidence_limit = NA_real_,
          upper_confidence_limit = NA_real_
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
              analysis = analysis@AnalysisMetadata$file_fingerprint,
              parent = i, parameter = as.character(.data$ID),
              estimate = .data$mean,
              lower_confidence_limit = .data$`0.025quant`,
              upper_confidence_limit = .data$`0.975quant`
            ) -> random_effect
          if (anyDuplicated(random_effect$parameter) == 0) {
            return(random_effect)
          }
          if (is.null(analysis@ReplicateName[[i]])) {
            random_effect %>%
              mutate(
                replicate = rep(
                  as.character(seq_len(n() / n_distinct(.data$parameter))),
                  each = n_distinct(.data$parameter)
                )
              )
          } else {
            random_effect %>%
              mutate(
                replicate = rep(
                  !!analysis@ReplicateName[[i]],
                  each = n_distinct(.data$parameter)
                )
              )
          }
        }
      ) %>%
        bind_rows() -> blup
      blup_fingerprint <- parameter %>%
        semi_join(
          tibble(
            description = "Random effect BLUP",
            parent = NA_character_
          ),
          by = c("description", "parent")
        ) %>%
        pull(.data$fingerprint)
      blup_parent <- blup %>%
        select(original = "parent") %>%
        distinct() %>%
        mutate(
          parent = gsub(" .*$", "", .data$original),
          description = gsub("^.* ", "", .data$original),
          parent = ifelse(
            .data$parent == .data$description,
            blup_fingerprint,
            .data$parent
          ),
          fingerprint = map2_chr(
            .data$description,
            .data$parent,
            ~sha1(c(description = .x, parent = .y))
          )
        )
      parameter <- blup_parent %>%
        select(-"original") %>%
        bind_rows(parameter)
      blup <- blup_parent %>%
        select("original", parent = "fingerprint") %>%
        inner_join(
          blup,
          by = c("original" = "parent")
        ) %>%
        select(-"original")

      if ("replicate" %in% colnames(blup)) {
        blup_parent <- blup %>%
          filter(!is.na(.data$replicate)) %>%
          select("parent", description = "replicate") %>%
          distinct() %>%
          mutate(
            fingerprint = map2_chr(
              .data$description,
              .data$parent,
              ~sha1(c(description = .x, parent = .y))
            )
          )
        parameter <- bind_rows(parameter, blup_parent)
        blup <- blup %>%
          left_join(
            blup_parent,
            by = c("replicate" = "description", "parent")
          ) %>%
          mutate(
            parent = ifelse(
              is.na(.data$fingerprint),
              .data$parent,
              .data$fingerprint
            )
          ) %>%
          select(-"replicate", -"fingerprint")
      }
      parameter <- blup %>%
        select("parent", description = "parameter") %>%
        distinct() %>%
        mutate(
          fingerprint = map2_chr(
            .data$description,
            .data$parent,
            ~sha1(c(description = .x, parent = .y))
          )
        ) %>%
        bind_rows(parameter)
      parameter_estimate <- blup %>%
        inner_join(parameter, by = c("parent", "parameter" = "description")) %>%
        select(-"parent", -"parameter", parameter = "fingerprint") %>%
        bind_rows(parameter_estimate)
    }

    # add fitted values
    display(verbose, ", fitted values")

    fitted_parent <- parameter %>%
      filter(is.na(.data$parent), .data$description == "Fitted") %>%
      pull(.data$fingerprint)
    get_model(analysis)$summary.fitted.values %>%
      transmute(
        analysis = analysis@AnalysisMetadata$file_fingerprint,
        parent = fitted_parent,
        estimate = .data$mean,
        lower_confidence_limit = .data$`0.025quant`,
        upper_confidence_limit = ifelse(
          .data$mean > .data$`0.975quant`,
          NA_real_,
          .data$`0.975quant`
        )
      ) %>%
      bind_cols(
        get_data(analysis) %>%
          transmute(parameter = as.character(.data$observation_id))
      ) -> fitted
    tibble(
      description = fitted$parameter,
      parent = fitted_parent
    ) %>%
      mutate(
        fingerprint = map2_chr(
          .data$description,
          .data$parent,
          ~sha1(c(description = .x, parent = .y))
        )
      ) %>%
      bind_rows(parameter) -> parameter
    tmp <- fitted %>%
      inner_join(parameter, by = c("parent", "parameter" = "description")) %>%
      select(-"parent", -"parameter", parameter = "fingerprint")
    parameter_estimate <- bind_rows(tmp, parameter_estimate)

    # imputed values
    if (!is.null(analysis@RawImputed)) {
      ri <- analysis@RawImputed
      extra <- ri@Data %>%
        mutate(response = ri@Response) %>%
        filter(is.na(.data$response)) %>%
        select(.data$observation_id) %>%
        mutate(
          analysis = get_file_fingerprint(analysis),
          estimate = apply(ri@Imputation, 1, quantile, probs = 0.500),
          lower_confidence_limit =
            apply(ri@Imputation, 1, quantile, probs = 0.025),
          upper_confidence_limit =
            apply(ri@Imputation, 1, quantile, probs = 0.975)
        )
      parent <- parameter %>%
        filter(.data$description == "Imputed value", is.na(.data$parent))
      impute_parameter <- extra %>%
        distinct(.data$observation_id) %>%
        transmute(
          parent = parent$fingerprint,
          description = .data$observation_id
        ) %>%
        mutate(
          fingerprint = map2_chr(
            .data$description,
            .data$parent,
            ~sha1(c(description = .x, parent = .y))
          )
        )
      parameter <- parameter %>%
        bind_rows(impute_parameter)
      parameter_estimate <- extra %>%
        inner_join(
          impute_parameter,
          by = c("observation_id" = "description")
        ) %>%
        select(
          "analysis",
          "Estimate",
          "LowerConfidenceLimit",
          "UpperConfidenceLimit",
          parameter = "fingerprint"
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
    present <- grep(paste0("^", i), parameter_estimate$parameter)
    present <- present[!grepl(":", parameter_estimate$parameter[present])]
    if (length(present) == 0) {
      next
    }
    extra <- tibble(
      description = i,
      parent = fixed_parent
    ) %>%
      mutate(
        fingerprint = map2_chr(
          .data$description,
          .data$parent,
          ~sha1(c(description = .x, parent = .y))
        )
      )
    extra_factor <- tibble(
      description = gsub(
        paste0("^", i),
        "",
        parameter_estimate$parameter[present]
      ),
      parent = extra$fingerprint
    ) %>%
      filter(.data$description != "")
    if (nrow(extra_factor) == 0) {
      to_merge <- extra %>%
        select(-"parent")
    } else {
      extra_factor <- extra_factor %>%
        mutate(
          fingerprint = map2_chr(
            .data$description,
            .data$parent,
            ~sha1(c(description = .x, parent = .y))
          )
        )
      extra_factor %>%
        select(-"parent") %>%
        mutate(description = paste0(i, .data$description)) -> to_merge
    }
    left_join(
      parameter_estimate,
      to_merge,
      by = c("parameter" = "description")
    ) %>%
      mutate(
        parameter = ifelse(
          is.na(.data$fingerprint), .data$parameter, .data$fingerprint
        )
      ) %>%
      select(-"fingerprint") -> parameter_estimate
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
    present <- grep(pattern, parameter_estimate$parameter)
    if (length(present) == 0) {
      next
    }
    extra <- tibble(
      description = i,
      parent = fixed_parent
    ) %>%
      mutate(
        fingerprint = map2_chr(
          .data$description,
          .data$parent,
          ~sha1(c(description = .x, parent = .y))
        )
      )
    parts <- strsplit(i, ":")[[1]]
    level_name <- gsub(
      paste0("^", parts[1]),
      "",
      parameter_estimate$parameter[present]
    )
    for (j in parts[-1]) {
      level_name <- gsub(paste0(":", j), ":", level_name)
    }
    extra_factor <- tibble(
      description = level_name,
      parent = extra$fingerprint
    ) %>%
      filter(!grepl("^:*$", .data$description)) %>%
      mutate(
        fingerprint = map2_chr(
          .data$description,
          .data$parent,
          ~sha1(c(description = .x, parent = .y))
        )
      )
    if (nrow(extra_factor) > 0) {
      to_merge <- extra_factor %>%
        select(-"parent") %>%
        inner_join(
          tibble(
            description = level_name,
            original = parameter_estimate$parameter[present]
          ),
          by = "description"
        ) %>%
        select(
          -"description",
          description = "original"
        )
    } else {
      to_merge <- extra %>%
        select(-"parent")
    }
    parameter_estimate <- left_join(
      parameter_estimate,
      to_merge,
      by = c("parameter" = "description")
    ) %>%
      mutate(
        parameter = ifelse(
          is.na(.data$fingerprint),
          .data$parameter,
          .data$fingerprint
        )
      ) %>%
      select(-"fingerprint")
    parameter <- bind_rows(parameter, extra, extra_factor)
  }
  attr(parameter_estimate, "parameter") <- parameter
  return(parameter_estimate)
}
