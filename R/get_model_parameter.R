#' Add the model parameters from a model
#' @param analysis The model to add
#' @name get_model_parameter
#' @rdname get_model_parameter
#' @exportMethod get_model_parameter
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_model_parameter",
  def = function(analysis){
    standard.generic("get_model_parameter")
  }
)

#' @rdname get_model_parameter
#' @aliases get_model_parameter,n2kGlmerPoisson-methods
#' @importFrom methods setMethod
#' @importFrom lme4 ranef VarCorr
#' @include n2kGlmerPoisson_class.R
#' @include n2kParameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kGlmerPoisson"),
  definition = function(analysis){
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
    parameter$Fingerprint <- apply(parameter, 1, get_sha1)

    # add fixed effect parameters
    message("    reading model parameters: fixed effects", appendLF = FALSE)
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
      extra$Fingerprint <- apply(extra, 1, get_sha1)
      parameter <- rbind(parameter, extra)
      continuous <- grep(paste0("^", i, "$"), parameter.estimate$Parameter)
      if (length(continuous) == 1) {
        parameter.estimate$Parameter[continuous] <- extra$Fingerprint
        next
      }
      extra.factor <- data.frame(
        Description = gsub(
          paste0("^", i),
          "" ,
          parameter.estimate$Parameter[present]
        ),
        Parent = extra$Fingerprint,
        stringsAsFactors = FALSE
      )
      extra.factor$Fingerprint <- apply(extra.factor, 1, get_sha1)
      parameter <- rbind(parameter, extra.factor)
      parameter.estimate$Parameter[present] <- extra.factor$Fingerprint
    }

    # add random effect variance
    message(", random effect variance", appendLF = FALSE)
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
    extra$Fingerprint <- apply(extra, 1, get_sha1)
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
    message(", random effect BLUP's", appendLF = FALSE)
    utils::flush.console()
    random.effect <- ranef(get_model(analysis), condVar = TRUE)
    extra <- data.frame(
      Description = names(random.effect),
      Parent = parameter$Fingerprint[
        parameter$Description == "Random effect BLUP"
      ],
      stringsAsFactors = FALSE
    )
    extra$Fingerprint <- apply(extra, 1, get_sha1)
    parameter <- rbind(parameter, extra)
    for (i in seq_along(random.effect)) {
      extra.blup <- data.frame(
        Description = rownames(random.effect[[i]]),
        Parent = extra$Fingerprint[i],
        stringsAsFactors = FALSE
      )
      extra.blup$Fingerprint <- apply(extra.blup, 1, get_sha1)
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
    message(", fitted values")
    utils::flush.console()

    extra.fitted <- data.frame(
      Description = analysis@Data$ObservationID,
      Parent = parameter$Fingerprint[parameter$Description == "Fitted"],
      stringsAsFactors = FALSE
    )
    extra.fitted$Fingerprint <- apply(extra.fitted, 1, get_sha1)
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
#' @aliases get_model_parameter,n2kLrtGlmer-methods
#' @importFrom methods setMethod
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kLrtGlmer"),
  definition = function(analysis){
    if (analysis@AnalysisMetadata$Status != "converged") {
      return(new("n2kParameter"))
    }

    parameter <- data.frame(
      Description = "LRT test",
      Parent = NA,
      stringsAsFactors = FALSE
    )
    parameter$Fingerprint <- apply(parameter, 1, get_sha1)

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
#' @aliases get_model_parameter,n2kComposite-methods
#' @importFrom methods setMethod
#' @include n2kComposite_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kComposite"),
  definition = function(analysis){
    if (analysis@AnalysisMetadata$Status != "converged") {
      return(new("n2kParameter"))
    }

    parameter <- data.frame(
      Description = "Composite index",
      Parent = NA,
      stringsAsFactors = FALSE
    )
    parameter$Fingerprint <- apply(parameter, 1, get_sha1)

    parameter.estimate <- cbind(
      Analysis = analysis@AnalysisMetadata$FileFingerprint,
      analysis@Index,
      stringsAsFactors = FALSE
    )
    colnames(parameter.estimate)[2] <- "Parameter"
    row.names(parameter.estimate) <- NULL
    parameter.estimate$Parameter <- levels(parameter.estimate$Parameter)[
      parameter.estimate$Parameter
    ]

    extra <- data.frame(
      Description = parameter.estimate$Parameter,
      Parent = parameter$Fingerprint,
      stringsAsFactors = FALSE
    )
    extra$Fingerprint <- apply(extra, 1, get_sha1)
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
#' @aliases get_model_parameter,n2kInlaNbinomial-methods
#' @importFrom methods setMethod
#' @importFrom dplyr data_frame rowwise mutate_ filter_ select_ left_join mutate_ bind_rows add_rownames
#' @importFrom n2khelper get_sha1
#' @include n2kInlaNbinomial_class.R
#' @include n2kParameter_class.R
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kInlaNbinomial"),
  definition = function(analysis){
    if (analysis@AnalysisMetadata$Status != "converged") {
      return(new("n2kParameter"))
    }
    parameter <- data_frame(
      Description = c(
        "Fixed effect", "Random effect BLUP", "Random effect variance", "Fitted",
        "Overdispersion", "WAIC"
      ),
      Parent = NA
    ) %>%
      rowwise() %>%
      mutate_(
        Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
      )

    # add fixed effect parameters
    message("    reading model parameters: fixed effects", appendLF = FALSE)
    utils::flush.console()


    variable <- c(
      "\\(Intercept\\)",
      attr(terms(analysis@AnalysisFormula[[1]]), "term.labels")
    )
    variable <- variable[-grep("f\\(", variable)]

    fixed.effect <- get_model(analysis)$summary.fixed
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
    interaction <- grep(":", variable)
    main.effect <- variable[-interaction]
    interaction <- variable[interaction]
    for (i in main.effect) {
      present <- grep(paste0("^", i), parameter.estimate$Parameter)
      present <- present[!grepl(":", parameter.estimate$Parameter[present])]
      if (length(present) == 0) {
        next
      }
      extra <- data_frame(
        Description = gsub("(\\\\|\\(|\\))", "", i),
        Parent = fixed.parent
      ) %>%
        rowwise() %>%
        mutate_(
          Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
        )
      extra.factor <- data_frame(
        Description = gsub(
          paste0("^", i),
          "" ,
          parameter.estimate$Parameter[present]
        ),
        Parent = extra$Fingerprint
      ) %>%
        filter_(~Description != "") %>%
        rowwise() %>%
        mutate_(
          Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
        )
      if (nrow(extra.factor) > 0) {
        to.merge <- extra.factor %>%
          select_(~-Parent) %>%
          mutate_(Description = ~paste0(i, Description))
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
          Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
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
          Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
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
    message(", random effect variance", appendLF = FALSE)
    utils::flush.console()
    re.names <- names(get_model(analysis)$marginals.hyperpar)
    re.names <- re.names[grep("^Precision for ", re.names)]
    re.variance <- t(sapply(
      get_model(analysis)$marginals.hyperpar[re.names],
      inla_inverse
    )) %>%
      as.data.frame() %>%
      add_rownames("Parameter") %>%
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
        Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
      )
    parameter.estimate <- extra %>%
      select_(~-Parent) %>%
      inner_join(re.variance, by = c("Description" = "Parameter")) %>%
      select_(~-Description, Parameter = ~Fingerprint) %>%
      bind_rows(parameter.estimate)
    parameter <- parameter %>% bind_rows(extra)

    # add overdispersion
    message(", overdispersion", appendLF = FALSE)
    utils::flush.console()
    overdispersion <- get_model(analysis)$summary.hyperpar[
      "size for the nbinomial observations (overdispersion)",
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
    message(", WAIC", appendLF = FALSE)
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
    message(", random effect BLUP's", appendLF = FALSE)
    utils::flush.console()
    blup <- do.call(
      rbind,
      lapply(
        names(get_model(analysis)$summary.random),
        function(i){
          random.effect <- get_model(analysis)$summary.random[[i]]
          blup <- data_frame(
            Analysis = analysis@AnalysisMetadata$FileFingerprint,
            Parent = gsub("^f", "", i),
            Parameter = random.effect$ID,
            Estimate = random.effect[, "mean"],
            LowerConfidenceLimit = random.effect[, "0.025quant"],
            UpperConfidenceLimit = random.effect[, "0.975quant"]
          )
        }
      )
    )
    extra <- parameter %>%
      filter_(~is.na(Parent), ~Description == "Random effect BLUP") %>%
      select_(Parent = ~Fingerprint) %>%
      merge(
        data_frame(Description = unique(blup$Parent))
      ) %>%
      rowwise() %>%
      mutate_(
        Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
      )
    blup <- extra %>%
      select_(~-Parent) %>%
      inner_join(blup, by = c("Description" = "Parent")) %>%
      select_(~-Description, Parent = ~Fingerprint)
    parameter <- blup %>%
      select_(~Parent, Description = ~ Parameter) %>%
      rowwise() %>%
      mutate_(
        Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
      ) %>%
      bind_rows(parameter, extra)
    parameter.estimate <- blup %>%
      inner_join(parameter, by = c("Parent", "Parameter" = "Description")) %>%
      select_(~-Parent, ~-Parameter, Parameter = ~Fingerprint) %>%
      bind_rows(parameter.estimate)

    # add fitted values
    message(", fitted values")
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
      UpperConfidenceLimit = fitted[, "0.975quant"]
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
        Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
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
