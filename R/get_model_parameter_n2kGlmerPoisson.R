#' @rdname get_model_parameter
#' @importFrom methods setMethod new
#' @importFrom lme4 ranef VarCorr
#' @importFrom digest sha1
#' @importFrom stats qnorm fitted
#' @include n2kGlmerPoisson_class.R
#' @include n2kParameter_class.R
#' @param verbose Print extra information on the screen
setMethod(
  f = "get_model_parameter",
  signature = signature(analysis = "n2kGlmerPoisson"),
  definition = function(analysis, verbose = TRUE, ...) {

    if (analysis@AnalysisMetadata$Status != "converged") {
      return(new("n2kParameter"))
    }
    parameter <- data.frame(
      Description = c(
        "Fixed effect", "Random effect BLUP", "Random effect variance", "Fitted"
      ),
      Parent = NA_character_,
      stringsAsFactors = FALSE
    )
    parameter$Fingerprint <- apply(parameter, 1, sha1)

    # add fixed effect parameters
    display(verbose, "    reading model parameters: fixed effects", FALSE)
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
    display(verbose, ", random effect variance", FALSE)
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
      LowerConfidenceLimit = NA_real_,
      UpperConfidenceLimit = NA_real_,
      stringsAsFactors = FALSE
    )
    rownames(tmp) <- NULL
    parameter.estimate <- rbind(parameter.estimate, tmp)

    # add random effect BLUP's
    display(verbose, ", random effect BLUP's", FALSE)
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
    display(verbose, ", fitted values")

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
      LowerConfidenceLimit = NA_real_,
      UpperConfidenceLimit = NA_real_,
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
