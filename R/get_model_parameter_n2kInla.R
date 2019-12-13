#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @importFrom tibble tibble rownames_to_column
#' @importFrom dplyr %>% bind_rows distinct filter left_join mutate n n_distinct
#' pull select semi_join transmute
#' @importFrom rlang .data
#' @importFrom purrr map_chr map2_chr map_df
#' @importFrom digest sha1
#' @importFrom INLA inla.tmarginal inla.qmarginal
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom stats terms
#' @importFrom utils flush.console
#' @include n2kInla_class.R
#' @include n2kParameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kInla"),
  definition = function(analysis, verbose = TRUE, ...) {
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

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
    if (verbose) {
      message("    reading model parameters: fixed effects", appendLF = FALSE)
    }
    flush.console()

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
      ) -> parameter.estimate
    fixed.parent <- parameter$Fingerprint[
      parameter$Description == "Fixed effect"
    ]
    interaction <- grepl(":", variable)
    main.effect <- variable[!interaction]
    interaction <- variable[interaction]
    for (i in main.effect) {
      present <- grep(paste0("^", i), parameter.estimate$Parameter)
      present <- present[!grepl(":", parameter.estimate$Parameter[present])]
      if (length(present) == 0) {
        next
      }
      extra <- tibble(
        Description = i,
        Parent = fixed.parent
      ) %>%
        mutate(
          Fingerprint = map2_chr(
            .data$Description,
            .data$Parent,
            ~sha1(c(Description = .x, Parent = .y))
          )
        )
      extra.factor <- tibble(
        Description = gsub(
          paste0("^", i),
          "",
          parameter.estimate$Parameter[present]
        ),
        Parent = extra$Fingerprint
      ) %>%
        filter(.data$Description != "")
      if (nrow(extra.factor) == 0) {
        to.merge <- extra %>%
          select(-"Parent")
      } else {
        extra.factor <- extra.factor %>%
          mutate(
            Fingerprint = map2_chr(
              .data$Description,
              .data$Parent,
              ~sha1(c(Description = .x, Parent = .y))
            )
          )
        extra.factor %>%
          select(-"Parent") %>%
          mutate(Description = paste0(i, .data$Description)) -> to.merge
      }
      left_join(
        parameter.estimate,
        to.merge,
        by = c("Parameter" = "Description")
      ) %>%
        mutate(
          Parameter = ifelse(
            is.na(.data$Fingerprint), .data$Parameter, .data$Fingerprint
          )
        ) %>%
        select(-"Fingerprint") -> parameter.estimate
      parameter <- bind_rows(parameter, extra, extra.factor)
    }
    for (i in interaction) {
      pattern <- paste0("^", gsub(":", ".*:", i))
      present <- grep(pattern, parameter.estimate$Parameter)
      if (length(present) == 0) {
        next
      }
      extra <- tibble(
        Description = i,
        Parent = fixed.parent
      ) %>%
        mutate(
          Fingerprint = map2_chr(
            .data$Description,
            .data$Parent,
            ~sha1(c(Description = .x, Parent = .y))
          )
        )
      parts <- strsplit(i, ":")[[1]]
      level.name <- gsub(
        paste0("^", parts[1]),
        "",
        parameter.estimate$Parameter[present]
      )
      for (j in parts[-1]) {
        level.name <- gsub(paste0(":", j), ":", level.name)
      }
      extra.factor <- tibble(
        Description = level.name,
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
      if (nrow(extra.factor) > 0) {
        to.merge <- extra.factor %>%
          select(-"Parent") %>%
          inner_join(
            tibble(
              Description = level.name,
              Original = parameter.estimate$Parameter[present]
            ),
            by = "Description"
          ) %>%
          select(
            -"Description",
            Description = "Original"
          )
      } else {
        to.merge <- extra %>%
          select(-"Parent")
      }
      parameter.estimate <- left_join(
        parameter.estimate,
        to.merge,
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
      parameter <- bind_rows(parameter, extra, extra.factor)
    }

    # add random effect variance
    if (verbose) {
      message(", random effect variance", appendLF = FALSE)
    }
    flush.console()

    re.names <- names(get_model(analysis)$marginals.hyperpar)
    re.names <- re.names[grepl("^Precision for ", re.names)]
    if (length(re.names) > 0) {
      map_df(
        get_model(analysis)$marginals.hyperpar[re.names],
        inla_inverse
      ) %>%
        mutate(
          Parameter = gsub("^Precision for ", "", re.names),
          Analysis = analysis@AnalysisMetadata$FileFingerprint
        ) -> re.variance
      parameter %>%
        filter(
          is.na(.data$Parent),
          .data$Description == "Random effect variance"
        ) %>%
        select(Parent = "Fingerprint") %>%
        merge(
          tibble(Description = re.variance$Parameter)
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
        inner_join(re.variance, by = c("Description" = "Parameter")) %>%
        select(-"Description", Parameter = "Fingerprint") %>%
        bind_rows(parameter.estimate) -> parameter.estimate
      parameter <- parameter %>% bind_rows(extra)
    }

    # add overdispersion
    if (get_model(analysis)$.args$family == "nbinomial") {
      if (verbose) {
        message(", overdispersion", appendLF = FALSE)
      }
      flush.console()
      overdispersion <- get_model(analysis)$summary.hyperpar
      overdispersion <- overdispersion[
        grep("size for the nbinomial observations", rownames(overdispersion)),
      ]
      parent <- parameter %>%
        filter(is.na(.data$Parent), .data$Description == "Overdispersion")
      parameter.estimate <- parameter.estimate %>%
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
    if (verbose) {
      message(", WAIC", appendLF = FALSE)
    }
    flush.console()
    parent <- parameter %>%
      filter(is.na(.data$Parent), .data$Description == "WAIC")
    parameter.estimate <- parameter.estimate %>%
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
    if (verbose) {
      message(", random effect BLUP's", appendLF = FALSE)
    }
    flush.console()

    if (length(re.names) > 0) {
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
            ) -> random.effect
          if (anyDuplicated(random.effect$Parameter) == 0) {
            return(random.effect)
          }
          if (is.null(analysis@ReplicateName[[i]])) {
            random.effect %>%
              mutate(
                Replicate = rep(
                  as.character(seq_len(n() / n_distinct(.data$Parameter))),
                  each = n_distinct(.data$Parameter)
                )
              )
          } else {
            random.effect %>%
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
      blup.fingerprint <- parameter %>%
        semi_join(
          tibble(
            Description = "Random effect BLUP",
            Parent = NA_character_
          ),
          by = c("Description", "Parent")
        ) %>%
        pull(.data$Fingerprint)
      blup.parent <- blup %>%
        select(Original = "Parent") %>%
        distinct() %>%
        mutate(
          Parent = gsub(" .*$", "", .data$Original),
          Description = gsub("^.* ", "", .data$Original),
          Parent = ifelse(
            .data$Parent == .data$Description,
            blup.fingerprint,
            .data$Parent
          ),
          Fingerprint = map2_chr(
            .data$Description,
            .data$Parent,
            ~sha1(c(Description = .x, Parent = .y))
          )
        )
      parameter <- blup.parent %>%
        select(-"Original") %>%
        bind_rows(parameter)
      blup <- blup.parent %>%
        select("Original", Parent = "Fingerprint") %>%
        inner_join(
          blup,
          by = c("Original" = "Parent")
        ) %>%
        select(-"Original")

      if ("Replicate" %in% colnames(blup)) {
        blup.parent <- blup %>%
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
        parameter <- bind_rows(parameter, blup.parent)
        blup <- blup %>%
          left_join(
            blup.parent,
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
      parameter.estimate <- blup %>%
        inner_join(parameter, by = c("Parent", "Parameter" = "Description")) %>%
        select(-"Parent", -"Parameter", Parameter = "Fingerprint") %>%
        bind_rows(parameter.estimate)
    }

    # add fitted values
    if (verbose) {
      message(", fitted values")
    }
    flush.console()

    fitted.parent <- parameter %>%
      filter(is.na(.data$Parent), .data$Description == "Fitted") %>%
      pull(.data$Fingerprint)
    get_model(analysis)$summary.fitted.values %>%
      transmute(
        Analysis = analysis@AnalysisMetadata$FileFingerprint,
        Parent = fitted.parent,
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
      Parent = fitted.parent
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
    parameter.estimate <- bind_rows(tmp, parameter.estimate)

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
      impute.parameter <- extra %>%
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
        bind_rows(impute.parameter)
      parameter.estimate <- extra %>%
        inner_join(
          impute.parameter,
          by = c("ObservationID" = "Description")
        ) %>%
        select(
          "Analysis",
          "Estimate",
          "LowerConfidenceLimit",
          "UpperConfidenceLimit",
          Parameter = "Fingerprint"
        ) %>%
        bind_rows(parameter.estimate)
    }

    new(
      "n2kParameter",
      Parameter = as.data.frame(parameter),
      ParameterEstimate = as.data.frame(parameter.estimate)
    )
  }
)
