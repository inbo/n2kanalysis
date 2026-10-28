#' @rdname fit_model
#' @importFrom methods setMethod new
#' @importFrom dplyr arrange filter group_by mutate n summarise transmute
#' @importFrom rlang .data
#' @importFrom utils file_test
#' @importFrom stats qnorm
#' @include n2k_aggregate_composite_class.R
setMethod(
  f = "fit_model",
  signature = signature(x = "n2kAggregateComposite"),
  definition = function(x, base, project, status = "new", ...) {
    validObject(x)
    assert_that(is.character(status), length(status) >= 1)
    if (!(status(x) %in% status)) {
      return(x)
    }

    if (status(x) == "new") {
      input <- x@Input
      sapply(
        input,
        function(i) {
          ncol(i@Imputation)
        }
      ) |>
        unique() -> n_imp
      stopifnot(
        "different number of imputations in parent analysis" =
          length(n_imp) == 1
      )
      covars <- lapply(
        input,
        FUN = function(i) {
          colnames(i@Covariate) |>
            sort()
        }
      )
      unlist(covars) |>
        unique() -> all_covars
      vapply(
        covars, FUN.VALUE = logical(length(all_covars)), common = all_covars,
        FUN = function(z, common) {
          common %in% z
        }
      ) -> relevant
      if (!is.null(dim(relevant))){
        relevant <- apply(relevant, 1, all)
      }
      stopifnot("no common covariates" = any(relevant))
      common <- all_covars[relevant]

      for (i in names(input)) {
        extra_col <- paste0("input_", i)
        input[[i]]@Covariate[, extra_col] <- apply(input[[i]]@Covariate, 1, sha1)
        rownames(input[[i]]@Imputation) <- input[[i]]@Covariate[, extra_col]
      }
      covariates <- lapply(input, slot, "Covariate")
      while (length(covariates) > 1) {
        covariates[[1]] <- merge(
          covariates[[1]], covariates[[length(covariates)]], all = TRUE
        )
        covariates <- head(covariates, -1)
      }
      covariates <- covariates[[1]]
      imputations <- array(
        dim = c(nrow(covariates), n_imp, length(input)),
        dimnames = list(
          NULL,
          sprintf("Imputation%04i", seq_len(n_imp)),
          names(input)
        )
      )
      for (i in names(input)) {
        hashes <- covariates[, paste0("input_", i)]
        relevant <- hashes %in% rownames(input[[i]]@Imputation)
        imputations[relevant, , i] <- input[[i]]@Imputation[hashes[relevant], ]
      }
      x@AggregatedImputed <- new(
        "aggregatedImputed",
        Covariate = covariates[
          , !colnames(covariates) %in% sprintf("input_%s", names(input))
        ],
        Imputation = apply(imputations, 1:2, x@Fun)
      )
      status(x) <- "converged"
      return(x)
    }

    status(x) <- "waiting"
    parent_status <- parent_status(x)
    parent_status |>
      filter(.data$parent_status %in% c("new", "waiting", status)) |>
      pull("parent_analysis") -> todo

    for (this_parent in todo) {
      model <- try(
        read_model(x = this_parent, base = base, project = project),
        silent = TRUE
      )
      if (inherits(model, "try-error")) {
        parent_status[
          parent_status$parent_analysis == this_parent,
          "parent_status"
        ] <- "error"
        next
      }
      parent_status[
        parent_status$parent_analysis == this_parent,
        "parent_status"
      ] <- status(model)
      parent_status[
        parent_status$parent_analysis == this_parent,
        "parentstatus_fingerprint"
      ] <- get_status_fingerprint(model)
      x@AnalysisRelation <- parent_status
      if (status(model) == "converged") {
        x@Input[[get_file_fingerprint(model)]] <- model@AggregatedImputed
      }
    }
    if (all(parent_status$parent_status == "converged")) {
      status(x) <- "new"
    } else if (any(parent_status$parent_status == "error")) {
      status(x) <- "error"
    } else {
      status(x) <- "waiting"
    }

    fit_model(x, status = "new", base = base, project = project, ...)
  }
)
