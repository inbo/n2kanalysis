#' Add the model parameters from a model
#' @param analysis The model to add
#' @param ... extra options
#' @name get_model_parameter
#' @rdname get_model_parameter
#' @exportMethod get_model_parameter
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_model_parameter",
  def = function(analysis, ...){
    standardGeneric("get_model_parameter") # nocov
  }
)

#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @importFrom lme4 ranef VarCorr
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom digest sha1
#' @importFrom stats qnorm fitted
#' @include n2kGlmerPoisson_class.R
#' @include n2kParameter_class.R
#' @param verbose Print extra information on the screen
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kGlmerPoisson"),
  definition = function(analysis, verbose = TRUE, ...){
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

    if (analysis@AnalysisMetadata$Status != "converged") {
      return(new("n2kParameter"))
    }
    parameter <- data.frame(
      Description = c(
        "Fixed effect", "Random effect BLUP", "Random effect variance", "Fitted"
      ),
      Parent = NA,
      stringsAsFactors = FALSE
    )
    parameter$Fingerprint <- apply(parameter, 1, sha1)

    # add fixed effect parameters
    if (verbose) {
      message("    reading model parameters: fixed effects", appendLF = FALSE)
    }
    utils::flush.console()
    variable <- c(
      "\\(Intercept\\)",
      attr(attr(get_model(analysis)@frame, "terms"), "term.labels")
    )
    fixed.effect <- summary(get_model(analysis))$coefficients
    parameter.estimate <- data.frame(
      Analysis = analysis@AnalysisMetadata$FileFingerprint,
      Parameter = row.names(fixed.effect),
      Estimate = fixed.effect[, "Estimate"],
      LowerConfidenceLimit = qnorm(
        p = 0.025,
        mean = fixed.effect[, "Estimate"],
        sd = fixed.effect[, "Std. Error"]
      ),
      UpperConfidenceLimit = qnorm(
        p = 0.975,
        mean = fixed.effect[, "Estimate"],
        sd = fixed.effect[, "Std. Error"]
      ),
      stringsAsFactors = FALSE
    )
    row.names(parameter.estimate) <- NULL
    fixed.parent <- parameter$Fingerprint[
      parameter$Description == "Fixed effect"
    ]
    for (i in variable) {
      present <- grep(paste0("^", i), parameter.estimate$Parameter)
      if (length(present) == 0) {
        next
      }
      extra <- data.frame(
        Description = gsub("(\\\\|\\(|\\))", "", i),
        Parent = fixed.parent,
        stringsAsFactors = FALSE
      )
      extra$Fingerprint <- apply(extra, 1, sha1)
      parameter <- rbind(parameter, extra)
      continuous <- grep(paste0("^", i, "$"), parameter.estimate$Parameter)
      if (length(continuous) == 1) {
        parameter.estimate$Parameter[continuous] <- extra$Fingerprint
        next
      }
      extra.factor <- data.frame(
        Description = gsub(
          paste0("^", i),
          "",
          parameter.estimate$Parameter[present]
        ),
        Parent = extra$Fingerprint,
        stringsAsFactors = FALSE
      )
      extra.factor$Fingerprint <- apply(extra.factor, 1, sha1)
      parameter <- rbind(parameter, extra.factor)
      parameter.estimate$Parameter[present] <- extra.factor$Fingerprint
    }

    # add random effect variance
    if (verbose) {
      message(", random effect variance", appendLF = FALSE)
    }
    utils::flush.console()
    random.variance <- VarCorr(get_model(analysis))
    if (any(sapply(random.variance, length) > 1)) {
      stop("get_model_parameters doesn't handle random slopes yet.")
    }
    extra <- data.frame(
      Description = names(random.variance),
      Parent = parameter$Fingerprint[
        parameter$Description == "Random effect variance"
      ],
      stringsAsFactors = FALSE
    )
    extra$Fingerprint <- apply(extra, 1, sha1)
    parameter <- rbind(parameter, extra)
    tmp <- data.frame(
      Analysis = analysis@AnalysisMetadata$FileFingerprint,
      Parameter = extra$Fingerprint,
      Estimate = unlist(random.variance),
      LowerConfidenceLimit = NA,
      UpperConfidenceLimit = NA,
      stringsAsFactors = FALSE
    )
    rownames(tmp) <- NULL
    parameter.estimate <- rbind(parameter.estimate, tmp)

    # add random effect BLUP's
    if (verbose) {
      message(", random effect BLUP's", appendLF = FALSE)
    }
    utils::flush.console()
    random.effect <- ranef(get_model(analysis), condVar = TRUE)
    extra <- data.frame(
      Description = names(random.effect),
      Parent = parameter$Fingerprint[
        parameter$Description == "Random effect BLUP"
      ],
      stringsAsFactors = FALSE
    )
    extra$Fingerprint <- apply(extra, 1, sha1)
    parameter <- rbind(parameter, extra)
    for (i in seq_along(random.effect)) {
      extra.blup <- data.frame(
        Description = rownames(random.effect[[i]]),
        Parent = extra$Fingerprint[i],
        stringsAsFactors = FALSE
      )
      extra.blup$Fingerprint <- apply(extra.blup, 1, sha1)
      parameter <- rbind(parameter, extra.blup)

      re <- random.effect[[i]][, 1]
      re.sd <- sqrt(attr(random.effect[[i]], "postVar")[1, 1, ])
      tmp <- data.frame(
        Analysis = analysis@AnalysisMetadata$FileFingerprint,
        Parameter = extra.blup$Fingerprint,
        Estimate = re,
        LowerConfidenceLimit = qnorm(0.025, mean = re, sd = re.sd),
        UpperConfidenceLimit = qnorm(0.975, mean = re, sd = re.sd),
        stringsAsFactors = FALSE
      )
      rownames(tmp) <- NULL
      parameter.estimate <- rbind(parameter.estimate, tmp)
    }

    # add fitted values
    if (verbose) {
      message(", fitted values")
    }
    utils::flush.console()

    extra.fitted <- data.frame(
      Description = analysis@Data$ObservationID,
      Parent = parameter$Fingerprint[parameter$Description == "Fitted"],
      stringsAsFactors = FALSE
    )
    extra.fitted$Fingerprint <- apply(extra.fitted, 1, sha1)
    parameter <- rbind(parameter, extra.fitted)
    tmp <- data.frame(
      Analysis = analysis@AnalysisMetadata$FileFingerprint,
      Parameter = extra.fitted$Fingerprint,
      Estimate = fitted(get_model(analysis)),
      LowerConfidenceLimit = NA,
      UpperConfidenceLimit = NA,
      stringsAsFactors = FALSE
    )
    rownames(tmp) <- NULL
    parameter.estimate <- rbind(parameter.estimate, tmp)

    new(
      "n2kParameter",
      Parameter = parameter,
      ParameterEstimate = parameter.estimate
    )
  }
)

#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kLrtGlmer"),
  definition = function(analysis, ...){
    if (analysis@AnalysisMetadata$Status != "converged") {
      return(new("n2kParameter"))
    }

    parameter <- data.frame(
      Description = "LRT test",
      Parent = NA,
      stringsAsFactors = FALSE
    )
    parameter$Fingerprint <- apply(parameter, 1, sha1)

    parameter.estimate <- data.frame(
      Analysis = analysis@AnalysisMetadata$FileFingerprint,
      Parameter = parameter$Fingerprint,
      Estimate = analysis@Anova[2, "Pr(>Chisq)"],
      LowerConfidenceLimit = NA,
      UpperConfidenceLimit = NA,
      stringsAsFactors = FALSE
    )
    row.names(parameter.estimate) <- NULL

    new(
      "n2kParameter",
      Parameter = parameter,
      ParameterEstimate = parameter.estimate
    )
  }
)

#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @include n2kComposite_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kComposite"),
  definition = function(analysis, ...){
    if (analysis@AnalysisMetadata$Status != "converged") {
      return(new("n2kParameter"))
    }

    parameter <- data.frame(
      Description = "Composite index",
      Parent = NA,
      stringsAsFactors = FALSE
    )
    parameter$Fingerprint <- apply(parameter, 1, sha1)

    parameter.estimate <- cbind(
      Analysis = analysis@AnalysisMetadata$FileFingerprint,
      analysis@Index,
      stringsAsFactors = FALSE
    )
    colnames(parameter.estimate)[2] <- "Parameter"
    row.names(parameter.estimate) <- NULL

    extra <- data.frame(
      Description = parameter.estimate$Parameter,
      Parent = parameter$Fingerprint,
      stringsAsFactors = FALSE
    )
    extra$Fingerprint <- apply(extra, 1, sha1)
    parameter <- rbind(parameter, extra)

    parameter.estimate$Parameter <- extra$Fingerprint

    new(
      "n2kParameter",
      Parameter = parameter,
      ParameterEstimate = parameter.estimate
    )
  }
)

#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr data_frame rowwise mutate_ filter_ select_ left_join mutate_ bind_rows transmute_ semi_join
#' @importFrom digest sha1
#' @importFrom INLA inla.tmarginal inla.qmarginal
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom stats terms
#' @include n2kInlaNbinomial_class.R
#' @include n2kParameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kInlaNbinomial"),
  definition = function(analysis, verbose = TRUE, ...){
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

    if (analysis@AnalysisMetadata$Status != "converged") {
      return(new("n2kParameter"))
    }
    parameter <- data_frame(
      Description = c(
        "Fixed effect", "Random effect BLUP", "Random effect variance",
        "Fitted", "Overdispersion", "WAIC"
      ),
      Parent = NA
    ) %>%
      rowwise() %>%
      mutate_(
        Fingerprint = ~sha1(c(Description = Description, Parent = Parent))
      )

    # add fixed effect parameters
    if (verbose) {
      message("    reading model parameters: fixed effects", appendLF = FALSE)
    }
    utils::flush.console()


    variable <- c(
      "Intercept",
      attr(terms(analysis@AnalysisFormula[[1]]), "term.labels")
    )
    variable <- variable[!grepl("f\\(", variable)]

    fixed.effect <- get_model(analysis)$summary.fixed
    row.names(fixed.effect) <- gsub("[\\(|\\)]", "", row.names(fixed.effect))
    parameter.estimate <- data_frame(
      Analysis = analysis@AnalysisMetadata$FileFingerprint,
      Parameter = row.names(fixed.effect),
      Estimate = fixed.effect[, "mean"],
      LowerConfidenceLimit = fixed.effect[, "0.025quant"],
      UpperConfidenceLimit = fixed.effect[, "0.975quant"]
    )
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
      extra <- data_frame(
        Description = i,
        Parent = fixed.parent
      ) %>%
        rowwise() %>%
        mutate_(
          Fingerprint = ~sha1(c(Description = Description, Parent = Parent))
        )
      extra.factor <- data_frame(
        Description = gsub(
          paste0("^", i),
          "",
          parameter.estimate$Parameter[present]
        ),
        Parent = extra$Fingerprint
      ) %>%
        filter_(~Description != "")
      if (nrow(extra.factor) == 0) {
        to.merge <- extra %>%
          select_(~-Parent)
      } else {
        extra.factor <- extra.factor %>%
          rowwise() %>%
          mutate_(
            Fingerprint = ~sha1(
              c(Description = Description, Parent = Parent)
            )
          )
        to.merge <- extra.factor %>%
          select_(~-Parent) %>%
          mutate_(Description = ~paste0(i, Description))
      }
      parameter.estimate <- left_join(
        parameter.estimate,
        to.merge,
        by = c("Parameter" = "Description")
      ) %>%
        mutate_(
          Parameter = ~ifelse(is.na(Fingerprint), Parameter, Fingerprint)
        ) %>%
        select_(~-Fingerprint)
      parameter <- bind_rows(parameter, extra, extra.factor)
    }
    for (i in interaction) {
      pattern <- paste0("^", gsub(":", ".*:", i))
      present <- grep(pattern, parameter.estimate$Parameter)
      if (length(present) == 0) {
        next
      }
      extra <- data_frame(
        Description = i,
        Parent = fixed.parent
      ) %>%
        rowwise() %>%
        mutate_(
          Fingerprint = ~sha1(c(Description = Description, Parent = Parent))
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
      extra.factor <- data_frame(
        Description = level.name,
        Parent = extra$Fingerprint
      ) %>%
        filter_(~!grepl("^:*$", Description)) %>%
        rowwise() %>%
        mutate_(
          Fingerprint = ~sha1(c(Description = Description, Parent = Parent))
        )
      if (nrow(extra.factor) > 0) {
        to.merge <- extra.factor %>%
          select_(~-Parent) %>%
          inner_join(
            data_frame(
              Description = level.name,
              Original = parameter.estimate$Parameter[present]
            ),
            by = "Description"
          ) %>%
          select_(
            ~-Description,
            Description = ~Original
          )
      } else {
        to.merge <- extra %>%
          select_(~-Parent)
      }
      parameter.estimate <- left_join(
        parameter.estimate,
        to.merge,
        by = c("Parameter" = "Description")
      ) %>%
        mutate_(
          Parameter = ~ifelse(is.na(Fingerprint), Parameter, Fingerprint)
        ) %>%
        select_(~-Fingerprint)
      parameter <- bind_rows(parameter, extra, extra.factor)
    }

    # add random effect variance
    if (verbose) {
      message(", random effect variance", appendLF = FALSE)
    }
    utils::flush.console()
    re.names <- names(get_model(analysis)$marginals.hyperpar)
    re.names <- re.names[grepl("^Precision for ", re.names)]
    if (length(re.names) > 0) {
      re.variance <- sapply(
        get_model(analysis)$marginals.hyperpar[re.names],
        function(x){
          tryCatch(
            x %>%
              inla.tmarginal(fun = function(x){
                1 / x
              }) %>%
              inla.qmarginal(p = c(
                Estimate = .5,
                LowerConfidenceLimit = .025,
                UpperConfidenceLimit = .975
              )),
            error = function(e){
              c(
                Estimate = NA,
                LowerConfidenceLimit = NA,
                UpperConfidenceLimit = NA
              )
            }
          )
        }
      ) %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column("Parameter") %>%
        mutate_(
          Parameter = ~gsub("^Precision for ", "", Parameter),
          Analysis = ~analysis@AnalysisMetadata$FileFingerprint
        )
      extra <- parameter %>%
        filter_(~is.na(Parent), ~Description == "Random effect variance") %>%
        select_(Parent = ~Fingerprint) %>%
        merge(
          data_frame(Description = re.variance$Parameter)
        ) %>%
        rowwise() %>%
        mutate_(
          Fingerprint = ~sha1(c(Description = Description, Parent = Parent))
        )
      parameter.estimate <- extra %>%
        select_(~-Parent) %>%
        inner_join(re.variance, by = c("Description" = "Parameter")) %>%
        select_(~-Description, Parameter = ~Fingerprint) %>%
        bind_rows(parameter.estimate)
      parameter <- parameter %>% bind_rows(extra)
    }

    # add overdispersion
    if (verbose) {
      message(", overdispersion", appendLF = FALSE)
    }
    utils::flush.console()
    overdispersion <- get_model(analysis)$summary.hyperpar
    overdispersion <- overdispersion[
      grep("size for the nbinomial observations", rownames(overdispersion)),
    ]
    parent <- parameter %>%
      filter_(~is.na(Parent), ~Description == "Overdispersion")
    parameter.estimate <- parameter.estimate %>%
      bind_rows(
        data_frame(
          Analysis = analysis@AnalysisMetadata$FileFingerprint,
          Parameter = parent$Fingerprint,
          Estimate = overdispersion[, "mean"],
          LowerConfidenceLimit = overdispersion[, "0.025quant"],
          UpperConfidenceLimit = overdispersion[, "0.975quant"]
        )
      )


    # add WAIC
    if (verbose) {
      message(", WAIC", appendLF = FALSE)
    }
    utils::flush.console()
    parent <- parameter %>%
      filter_(~is.na(Parent), ~Description == "WAIC")
    parameter.estimate <- parameter.estimate %>%
      bind_rows(
        data_frame(
          Analysis = analysis@AnalysisMetadata$FileFingerprint,
          Parameter = parent$Fingerprint,
          Estimate = get_model(analysis)$waic$waic,
          LowerConfidenceLimit = NA,
          UpperConfidenceLimit = NA
        )
      )

    # add random effect BLUP's
    if (verbose) {
      message(", random effect BLUP's", appendLF = FALSE)
    }
    utils::flush.console()
    if (length(re.names) > 0) {
      blup <- lapply(
        names(get_model(analysis)$summary.random),
        function(i){
          random.effect <- get_model(analysis)$summary.random[[i]]
          if (anyDuplicated(random.effect$ID) == 0) {
            data_frame(
              Analysis = analysis@AnalysisMetadata$FileFingerprint,
              Parent = gsub("^(f|c)", "", i),
              Parameter = as.character(random.effect$ID),
              Estimate = random.effect[, "mean"],
              LowerConfidenceLimit = random.effect[, "0.025quant"],
              UpperConfidenceLimit = random.effect[, "0.975quant"]
            )
          } else {
            if (is.null(analysis@ReplicateName[[i]])) {
              random.effect <- random.effect %>%
                mutate_(
                  Replicate = ~rep(
                    seq_len(n() / n_distinct(ID)),
                    each = n_distinct(ID)
                  )
                )
            } else {
              random.effect <- random.effect %>%
                mutate_(
                  Replicate = ~rep(
                    analysis@ReplicateName[[i]],
                    each = n_distinct(ID)
                  )
                )
            }
            data_frame(
              Analysis = analysis@AnalysisMetadata$FileFingerprint,
              Parent = paste(gsub("^(f|c)", "", i)),
              Replicate = as.character(random.effect[, "Replicate"]),
              Parameter = as.character(random.effect$ID),
              Estimate = random.effect[, "mean"],
              LowerConfidenceLimit = random.effect[, "0.025quant"],
              UpperConfidenceLimit = random.effect[, "0.975quant"]
            )
          }
        }
      ) %>%
        bind_rows()
      blup.fingerprint <- parameter %>%
        semi_join(
          data_frame(
            Description = "Random effect BLUP",
            Parent = NA_character_
          ),
          by = c("Description", "Parent")
        ) %>%
        select_(~Fingerprint) %>%
        unlist()
      blup.parent <- blup %>%
        select_(Original = ~Parent) %>%
        distinct_() %>%
        mutate_(
          Parent = ~gsub(" .*$", "", Original),
          Description = ~gsub("^.* ", "", Original),
          Parent = ~ifelse(Parent == Description, blup.fingerprint, Parent)
        ) %>%
        rowwise() %>%
        mutate_(
          Fingerprint = ~sha1(c(Description = Description, Parent = Parent))
        )
      parameter <- blup.parent %>%
        select_(~- Original) %>%
        bind_rows(parameter)
      blup <- blup.parent %>%
        select_(~Original, Parent = ~Fingerprint) %>%
        inner_join(
          blup,
          by = c("Original" = "Parent")
        ) %>%
        select_(~-Original)

      if ("Replicate" %in% colnames(blup)) {
        blup.parent <- blup %>%
          filter_(~!is.na(Replicate)) %>%
          select_(~Parent, Description = ~Replicate) %>%
          distinct_() %>%
          rowwise() %>%
          mutate_(
            Fingerprint = ~sha1(c(Description = Description, Parent = Parent))
          )
        parameter <- bind_rows(parameter, blup.parent)
        blup <- blup %>%
          left_join(
            blup.parent,
            by = c("Replicate" = "Description", "Parent")
          ) %>%
          mutate_(
            Parent = ~ifelse(is.na(Fingerprint), Parent, Fingerprint)
          ) %>%
          select_(~-Replicate, ~-Fingerprint)
      }
      parameter <- blup %>%
        select_(~Parent, Description = ~Parameter) %>%
        distinct_() %>%
        rowwise() %>%
        mutate_(
          Fingerprint = ~sha1(c(Description = Description, Parent = Parent))
        ) %>%
        bind_rows(parameter)
      parameter.estimate <- blup %>%
        inner_join(parameter, by = c("Parent", "Parameter" = "Description")) %>%
        select_(~-Parent, ~-Parameter, Parameter = ~Fingerprint) %>%
        bind_rows(parameter.estimate)
    }

    # add fitted values
    if (verbose) {
      message(", fitted values")
    }
    utils::flush.console()

    fitted.parent <- parameter %>%
      filter_(~is.na(Parent), ~Description == "Fitted") %>%
      select_(Parent = ~Fingerprint)
    fitted <- get_model(analysis)$summary.fitted.values
    fitted <- data_frame(
      Analysis = analysis@AnalysisMetadata$FileFingerprint,
      Parent = fitted.parent$Parent,
      Estimate = fitted[, "mean"],
      LowerConfidenceLimit = fitted[, "0.025quant"],
      UpperConfidenceLimit = ifelse(
        fitted[, "mean"] > fitted[, "0.975quant"],
        NA,
        fitted[, "0.975quant"]
      )
    ) %>%
      bind_cols(
        get_data(analysis) %>%
          transmute_(Parameter = ~as.character(ObservationID))
      )

    parameter <- fitted.parent %>%
      merge(
        data_frame(Description = fitted$Parameter)
      ) %>%
      rowwise() %>%
      mutate_(
        Fingerprint = ~sha1(c(Description = Description, Parent = Parent))
      ) %>%
      bind_rows(parameter)
    parameter.estimate <- fitted %>%
      inner_join(parameter, by = c("Parent", "Parameter" = "Description")) %>%
      select_(~-Parent, ~-Parameter, Parameter = ~Fingerprint) %>%
      bind_rows(parameter.estimate)

    new(
      "n2kParameter",
      Parameter = as.data.frame(parameter),
      ParameterEstimate = as.data.frame(parameter.estimate)
    )
  }
)


#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @include n2kInlaComparison_class.R
#' @include n2kParameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kInlaComparison"),
  definition = function(analysis, ...){
    warning("reading model parameters on n2kInlaComparison is to do")
    return(new("n2kParameter"))
  }
)

#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @include n2kAggregate_class.R
#' @include n2kParameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kAggregate"),
  definition = function(analysis, ...){
    return(new("n2kParameter"))
  }
)

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
