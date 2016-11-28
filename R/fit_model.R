#' Fit a n2kModel object
#' @param x the n2kModel
#' @param ... other arguments. See details
#' @name fit_model
#' @rdname fit_model
#' @exportMethod fit_model
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "fit_model",
  def = function(x, ...){
    standardGeneric("fit_model") # nocov
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom n2khelper check_path
#' @importFrom assertthat assert_that is.flag noNA
#' @details
#' \describe{
#'  \item{\code{status}}{A vector with status levels naming the levels which should be recalculated. Defaults to \code{"new"}}
#'  \item{\code{verbose}}{A logical indicating if the function should display the name of the file and the status. Defaults to \code{TRUE}}
#' }
setMethod(
  f = "fit_model",
  signature = signature(x = "character"),
  definition = function(x, ...){
    x <- check_path(x, type = "file")
    dots <- list(...)
    if (is.null(dots$verbose)) {
      dots$verbose <- TRUE
    } else {
      assert_that(is.flag(dots$verbose))
      assert_that(noNA(dots$verbose))
    }
    if (dots$verbose) {
      message(x)
    }
    analysis <- readRDS(x)
    if (dots$verbose) {
      message(status(analysis), " -> ", appendLF = FALSE)
      utils::flush.console()
    }
    analysis.fitted <- fit_model(
      x = analysis,
      status = dots$status,
      path = dirname(x)
    )
    validObject(analysis.fitted)
    if (dots$verbose) {
      message(status(analysis.fitted))
      utils::flush.console()
    }
    saveRDS(analysis.fitted, file = x)
    return(invisible(NULL))
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom lme4 glmer glmerControl
#' @importFrom stats poisson qnorm
#' @importFrom utils sessionInfo
#' @include n2kGlmerPoisson_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kGlmerPoisson"),
  definition = function(x, ...){
    validObject(x)

    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }
    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    set.seed(get_seed(x))

    data <- get_data(x)
    model.formula <- x@AnalysisFormula[[1]]

    controls <- list(
      glmerControl(optimizer = "bobyqa"),
      glmerControl(optimizer = "optimx", optCtrl = list(method = "nlminb"))
    )
    for (control in controls) {
      if ("optimx" %in% control$optimizer) {
        requireNamespace("optimx", quietly = TRUE)
      }

      if (grepl("^weighted", get_model_type(x))) {
        model <- try(glmer(
          formula = model.formula,
          data = data,
          family = poisson,
          weights = data$Weight,
          control = control
        ))
      } else {
        model <- try(glmer(
          formula = model.formula,
          data = data,
          family = poisson,
          control = control
        ))
      }
      if ("try-error" %in% class(model)) {
        next
      }
      if (length(model@optinfo$conv$lme4) == 0) {
        break
      }
    }
    if ("try-error" %in% class(model)) {
      status(x) <- "error"
      return(x)
    }
    if (length(model@optinfo$conv$lme4) > 0) {
      status(x) <- "false convergence"
      return(x)
    }
    vc <- VarCorr(model)
    if ("fObservation" %in% names(vc)) {
      olre.ratio <- exp(
        diff(
          qnorm(
            p = c(0.025, 0.975),
            mean = 0,
            sd = sqrt(vc[["fObservation"]])
          )
        )
      )
      if (olre.ratio > 1e4) {
        status(x) <- "unstable"
        return(x)
      }
    }
    x@Model <- model

    version <- get_analysis_version(sessionInfo())
    new.version <- union(x, version)
    x@AnalysisVersion <- new.version$Union@AnalysisVersion
    x@RPackage <- new.version$Union@RPackage
    x@AnalysisVersionRPackage <- new.version$Union@AnalysisVersionRPackage

    x@AnalysisMetadata$AnalysisVersion <- new.version$UnionFingerprint

    status(x) <- "converged"
    return(x)
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom assertthat assert_that
#' @include n2kInlaNbinomial_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kInlaNbinomial"),
  definition = function(x, ...){
    validObject(x)

    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }
    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    if (!require("INLA")) {
      stop("The INLA package is required but not installed.") #nocov
    }

    set.seed(get_seed(x))

    data <- get_data(x)
    model.formula <- x@AnalysisFormula[[1]]

    response <- data[, as.character(x@AnalysisFormula[[1]][[2]])]
    link <- ifelse(is.na(response), 1, NA)

    if (is.null(x@LinearCombination)) {
      lc <- NULL
    } else {
      lincomb <- x@LinearCombination
      if (class(lincomb) == "matrix") {
        lc <- lincomb %>%
          as.data.frame() %>%
          as.list() %>%
          INLA::inla.make.lincombs()
        names(lc) <- rownames(lincomb)
      } else {
        lc <- INLA::inla.make.lincombs(lincomb)
        if (is.matrix(lincomb[[1]])) {
          names(lc) <- rownames(lincomb[[1]])
        } else {
          names(lc) <- names(lincomb[[1]])
        }
      }
    }
    model <- try(
      INLA::inla(
        formula = model.formula,
        family = "nbinomial",
        data = data,
        lincomb = lc,
        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
        control.predictor = list(compute = TRUE, link = link),
        control.fixed = list(prec.intercept = 1)
      )
    )
    if ("try-error" %in% class(model)) {
      status(x) <- "error"
      return(x)
    }
    return(
      n2k_inla_nbinomial(data = x, model.fit = model, status = "converged")
    )
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom utils file_test
#' @importFrom stats anova
#' @include n2kLrtGlmer_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kLrtGlmer"),
  definition = function(x, ...){
    validObject(x)
    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }

    # stop if status doesn't require (re-)fitting the model
    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    # do calculation when all parents are available
    if (status(x) == "new") {
      #check for incorrect "new" status
      if (any(is.null(x@Model), is.null(x@Model0))) {
        x@AnalysisRelation$ParentStatusFingerprint <- "zzz"
        status(x) <- "waiting"
        return(fit_model(x, status = "waiting", ...))
      }
      x@Anova <- anova(x@Model, x@Model0)
      status(x) <- "converged"
      return(x)
    }

    # check if parents are available
    if (is.null(dots$path)) {
      dots$path <- "."
    }
    old.parent.status <- parent_status(x)
    colnames(old.parent.status)[3:4] <- c("OldStatusFingerprint", "OldStatus")
    files.to.check <- normalizePath(
      paste0(dots$path, "/", old.parent.status$ParentAnalysis, ".rds"),
      winslash = "/",
      mustWork = FALSE
    )
    if (!all(file_test("-f", files.to.check))) {
      status(x) <- "error"
      return(x)
    }

    #check if parents have changed
    current.parent.status <- status(files.to.check)[
      , c("FileFingerprint", "StatusFingerprint", "Status")
    ]
    colnames(current.parent.status)[1] <- "ParentAnalysis"
    compare <- merge(old.parent.status, current.parent.status)
    changes <- which(compare$OldStatusFingerprint != compare$StatusFingerprint)
    colnames(compare)[5:6] <- c("ParentStatusFingerprint", "ParentStatus")
    x@AnalysisRelation <- compare[
      order(compare$ParentAnalysis),
      c("Analysis", "ParentAnalysis", "ParentStatusFingerprint", "ParentStatus")
    ]
    if (any(current.parent.status == "error")) {
      status(x) <- "error"
      return(x)
    }
    if (any(current.parent.status == "false convergence")) {
      status(x) <- "false convergence"
      return(x)
    }
    if (any(current.parent.status == "unstable")) {
      status(x) <- "unstable"
      return(x)
    }
    if (length(changes) == 0) {
      if (all(current.parent.status$Status == "converged")) {
        status(x) <- "new"
      }
      return(x)
    }

    if (length(changes) == 2) {
      file <- paste0(dots$path, "/", x@Parent0, ".rds")
      x@Model0 <- get_model(file)
      parent.1 <- compare$ParentAnalysis[compare$ParentAnalysis != x@Parent0]
      file <- paste0(dots$path, "/", parent.1, ".rds")
      x@Model <- get_model(file)
    } else {
      file <- paste0(dots$path, "/", compare$ParentAnalysis[changes], ".rds")
      if (x@Parent0 == compare$ParentAnalysis[changes]) {
        x@Model0 <- get_model(file)
      } else {
        x@Model <- get_model(file)
      }
    }
    if (all(current.parent.status$Status == "converged")) {
      status(x) <- "new"
    } else {
      status(x) <- "waiting"
    }
    if (status(x) == "new") {
      return(fit_model(x, status = "new", ...))
    }
    return(x)
  }
)

#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom dplyr %>% select_ group_by_ summarise_ distinct_ filter_ anti_join arrange_ inner_join
#' @importFrom utils file_test
#' @importFrom stats qnorm
#' @include n2kComposite_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kComposite"),
  definition = function(x, ...){
    validObject(x)
    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }
    if (!(status(x) %in% dots$status)) {
      return(x)
    }
    if (status(x) == "new") {
      parameter <- x@Parameter
      if (nrow(parameter) == 0) {
        status(x) <- "error"
        return(x)
      }
      # ignore parents which are missing in one of more years
      missing.parent <- parameter %>%
        filter_(~Estimate < -10) %>%
        select_(~Parent) %>%
        distinct_()
      x@Index <- anti_join(parameter, missing.parent, by = "Parent") %>%
        group_by_(~Value) %>%
        summarise_(
          Estimate = ~mean(Estimate),
          SE = ~ sqrt(sum(Variance) / n()),
          LowerConfidenceLimit = ~qnorm(0.025, mean = Estimate, sd = SE),
          UpperConfidenceLimit = ~qnorm(0.975, mean = Estimate, sd = SE)
        ) %>%
        select_(~-SE) %>%
        as.data.frame()
      status(x) <- "converged"
      return(x)
    }

    # status: "waiting"
    if (is.null(dots$path)) {
      dots$path <- "."
    }
    old.parent.status <- parent_status(x)
    files.to.check <- normalizePath(
      paste0(dots$path, "/", old.parent.status$ParentAnalysis, ".rds"),
      winslash = "/",
      mustWork = FALSE
    )
    if (!all(file_test("-f", files.to.check))) {
      status(x) <- "error"
      return(x)
    }
    old.parent.status <- old.parent.status %>%
      rename_(
        OldStatusFingerprint = ~ParentStatusFingerprint,
        OldStatus = ~ParentStatus
      )
    compare <- status(files.to.check) %>%
      select_(
        ParentAnalysis = ~FileFingerprint,
        ParentStatusFingerprint = ~StatusFingerprint,
        ParentStatus = ~Status
      ) %>%
      inner_join(old.parent.status, by = "ParentAnalysis") %>%
      arrange_(~ParentAnalysis)

    to.update <- compare %>%
      filter_(~ParentStatus == "converged")
    x@AnalysisRelation <- compare %>%
      select_(
        ~Analysis,
        ~ParentAnalysis,
        ~ParentStatusFingerprint,
        ~ParentStatus
      )

    if (any(x@AnalysisRelation$ParentStatus == "error")) {
      status(x) <- "error"
      return(x)
    }

    if (nrow(to.update) > 0) {
      x@Parameter <- extract(
        extractor = x@Extractor,
        object = to.update$ParentAnalysis,
        path = dots$path
      ) %>%
        arrange_(~Parent, ~Value)
    }

    if (all(
      x@AnalysisRelation$ParentStatus %in% c("converged", "unstable")
    )) {
      status(x) <- "new"
      return(fit_model(x, status = "new", ...))
    }
    status(x) <- "waiting"
    return(x)
  }
)


#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom dplyr rename_ select_ inner_join arrange_ filter_ mutate_ bind_rows
#' @importFrom utils file_test
#' @include n2kInlaComparison_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kInlaComparison"),
  definition = function(x, ...){
    validObject(x)
    dots <- list(...)
    if (is.null(dots$status)) {
      dots$status <- c("new", "waiting")
    }
    if (!(status(x) %in% dots$status)) {
      return(x)
    }

    # status: "new"
    if (status(x) == "new") {
      x@WAIC <- lapply(
          names(x@Models),
          function(parent){
            data.frame(
              Parent = parent,
              WAIC = x@Models[[parent]]$waic$waic,
              Peff = x@Models[[parent]]$waic$p.eff,
              stringsAsFactors = FALSE
            )
          }
        ) %>%
        bind_rows() %>%
        as.data.frame()
      status(x) <- "converged"
      return(x)
    }

    # status: "waiting"
    if (is.null(dots$path)) {
      dots$path <- "."
    }
    old.parent.status <- parent_status(x)
    files.to.check <- normalizePath(
      paste0(dots$path, "/", old.parent.status$ParentAnalysis, ".rds"),
      winslash = "/",
      mustWork = FALSE
    )
    if (!all(file_test("-f", files.to.check))) {
      status(x) <- "error"
      return(x)
    }
    old.parent.status <- old.parent.status %>%
      rename_(
        OldStatusFingerprint = ~ParentStatusFingerprint,
        OldStatus = ~ParentStatus
      )
    compare <-
      status(files.to.check) %>%
      select_(
        ParentAnalysis = ~FileFingerprint,
        ParentStatusFingerprint = ~StatusFingerprint,
        ParentStatus = ~Status
      ) %>%
      inner_join(old.parent.status, by = "ParentAnalysis") %>%
      arrange_(~ParentAnalysis)

    to.update <- compare %>% filter_(~ParentStatus == "converged")
    x@AnalysisRelation <- compare %>%
      select_(
        ~Analysis,
        ~ParentAnalysis,
        ~ParentStatusFingerprint,
        ~ParentStatus
      )

    if (any(x@AnalysisRelation$ParentStatus == "error")) {
      status(x) <- "error"
      return(x)
    }
    if (any(x@AnalysisRelation$ParentStatus == "false convergence")) {
      status(x) <- "false convergence"
      return(x)
    }
    if (nrow(to.update) == 0) {
      if (all(x@AnalysisRelation$ParentStatus == "unstable")) {
        status(x) <- "unstable"
      }
      return(x)
    }

    to.update <- to.update %>%
      mutate_(Filename = ~ paste0(dots$path, "/", ParentAnalysis, ".rds")) %>%
      select_(~ParentAnalysis, ~Filename)
    models <- lapply(to.update$Filename, get_model)
    names(models) <- to.update$ParentAnalysis
    x@Models <- models

    if (all(
      x@AnalysisRelation$ParentStatus %in% c("converged", "unstable")
    )) {
      status(x) <- "new"
      return(fit_model(x, status = "new", ...))
    }
    status(x) <- "waiting"
    return(x)
  }
)
