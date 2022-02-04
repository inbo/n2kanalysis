#' @rdname get_result
#' @importFrom methods setMethod new
#' @importFrom dplyr %>% arrange filter inner_join mutate rename select
#' semi_join tibble transmute
#' @importFrom rlang .data
#' @importFrom digest sha1
#' @importFrom tidyr gather_
#' @importFrom stats as.formula
#' @include n2k_result_class.R
#' @include n2k_inla_class.R
setMethod(
  f = "get_result",
  signature = signature(x = "n2kInla"),
  definition = function(x, verbose = TRUE, ...) {
    validObject(x)
    anomaly <- get_anomaly(analysis = x, verbose = verbose, ...)
    if (is.null(x@LinearCombination)) {
      return(
        new(
          "n2kResult",
          AnalysisMetadata = x@AnalysisMetadata,
          AnalysisFormula = lapply(x@AnalysisMetadata$formula, as.formula),
          AnalysisRelation = x@AnalysisRelation,
          AnalysisVersion = x@AnalysisVersion,
          RPackage = x@RPackage,
          AnalysisVersionRPackage = x@AnalysisVersionRPackage,
          Parameter = anomaly@Parameter,
          ParameterEstimate = anomaly@ParameterEstimate,
          AnomalyType = anomaly@AnomalyType,
          Anomaly = anomaly@Anomaly
        )
      )
    }
    if (is.matrix(x@LinearCombination)) {
      description <- rownames(x@LinearCombination)
    } else {
      if (is.matrix(x@LinearCombination[[1]])) {
        description <- rownames(x@LinearCombination[[1]])
      } else {
        description <- names(x@LinearCombination[[1]])
      }
    }
    contrast <- tibble(
      description = description, analysis = get_file_fingerprint(x)
    ) %>%
      mutate(
        fingerprint = map2_chr(
          .data$description, .data$analysis,
          ~sha1(c(description = .x, analysis = .y))
        )
      ) %>%
      select("fingerprint", "description", "analysis") %>%
      as.data.frame()
    if (is.null(get_model(x))) {
      return(
        new(
          "n2kResult",
          AnalysisMetadata = x@AnalysisMetadata,
          AnalysisFormula = lapply(x@AnalysisMetadata$formula, as.formula),
          AnalysisRelation = x@AnalysisRelation,
          AnalysisVersion = x@AnalysisVersion,
          RPackage = x@RPackage,
          AnalysisVersionRPackage = x@AnalysisVersionRPackage,
          Parameter = anomaly@Parameter,
          ParameterEstimate = anomaly@ParameterEstimate,
          AnomalyType = anomaly@AnomalyType,
          Anomaly = anomaly@Anomaly,
          Contrast = contrast
        )
      )
    }

    concat <- function(parent, child) {
      child[is.na(child)] <- ""
      parent_split <- strsplit(parent, ":")
      child_split <- strsplit(child, ":")
      too_short <- sapply(child_split, length) < sapply(parent_split, length)
      child_split[too_short] <- lapply(child_split[too_short], c, "")
      sapply(
        seq_along(parent_split),
        function(i) {
          rbind(parent_split[[i]], child_split[[i]])
        }
      )
      apply(
        cbind(parent_split, child_split),
        1,
        function(z) {
          do.call(
            function(...) {
              paste0(..., collapse = ":")
            },
            z
          )
        }
      )
    }

    fixed_parameter_id <- anomaly@Parameter %>%
      semi_join(
        anomaly@Parameter %>%
          filter(.data$description == "Fixed effect"),
        by = c("parent" = "fingerprint")
      ) %>%
      select(
        parent_description = .data$description,
        parent = .data$fingerprint
      ) %>%
      left_join(anomaly@Parameter, by = "parent") %>%
      transmute(
        parameter = ifelse(
          is.na(.data$fingerprint), .data$parent, .data$fingerprint
        ),
        parameter_id = concat(
          child = .data$description, parent = .data$parent_description
        )
      )

    if (is.matrix(x@LinearCombination)) {
      contrast_coefficient <- x@LinearCombination
      contrast_coefficient[abs(contrast_coefficient) < 1e-8] <- NA
      contrast_coefficient <- contrast_coefficient %>%
        as.data.frame() %>%
        rownames_to_column("description") %>%
        gather_(
          "parameter_id",
          "coefficient",
          colnames(contrast_coefficient)[
            !grepl("description", colnames(contrast_coefficient))
          ],
          na.rm = TRUE
        ) %>%
        inner_join(
          contrast %>%
            select(-"analysis"),
          by = "description"
        ) %>%
        select(-"description", contrast = .data$fingerprint) %>%
        mutate(parameter_id = gsub("[\\(|\\)]", "", .data$parameter_id)) %>%
        inner_join(fixed_parameter_id, by = "parameter_id") %>%
        select(.data$contrast, .data$parameter, .data$coefficient) %>%
        arrange(.data$contrast, .data$parameter) %>%
        as.data.frame()
    } else {
      contrast_coefficient <- lapply(
        names(x@LinearCombination),
        function(y) {
          if (is.vector(x@LinearCombination[[y]])) {
            data.frame(
              contrast = contrast$fingerprint,
              parameter_id = gsub("[\\(|\\)]", "", y),
              coefficient = x@LinearCombination[[y]],
              stringsAsFactors = FALSE
            ) %>%
              filter(abs(.data$coefficient) >= 1e-8) %>%
              inner_join(fixed_parameter_id, by = "parameter_id") %>%
              select(.data$contrast, .data$parameter, .data$coefficient)
          } else {
            random_id <- anomaly@Parameter %>%
              semi_join(
                anomaly@Parameter %>%
                semi_join(
                  anomaly@Parameter %>%
                    semi_join(
                      data.frame(
                        description = "Random effect BLUP",
                        stringsAsFactors = FALSE
                      ),
                      by = "description"
                    ) %>%
                    mutate(description = y),
                  by = c("parent" = "fingerprint", "description")
                ),
                by = c("parent" = "fingerprint")
              ) %>%
              select(-"parent", parameter = .data$fingerprint)
            lc <- x@LinearCombination[[y]] %>%
              as.data.frame()
            lc[abs(lc) < 1e-8] <- NA
            if (anyDuplicated(x@Model$summary.random[[y]]$ID) == 0) {
              lc %>%
                mutate(contrast = contrast$fingerprint) %>%
                gather_(
                  "description", "coefficient",
                  colnames(lc)[!grepl("contrast", colnames(lc))],
                  na.rm = TRUE, factor_key = TRUE
                ) %>%
                mutate(
                  description = as.character(
                    x@Model$summary.random[[y]]$ID[.data$description]
                  )
                ) %>%
                inner_join(random_id, by = "description") %>%
                select(-"description")
            } else {
              lc %>%
                mutate(contrast = contrast$fingerprint) %>%
                gather_(
                  "description", "coefficient",
                  colnames(lc)[!grepl("contrast", colnames(lc))],
                  na.rm = TRUE
                ) %>%
                inner_join(
                  anomaly@Parameter %>%
                    inner_join(
                      random_id %>%
                        rename(main = "description"),
                      by = c("parent" = "parameter")
                    ) %>%
                    mutate(
                      description = sprintf(
                        "%s:%s", .data$main, .data$description
                      )
                    ) %>%
                    select(parameter = .data$fingerprint, .data$description),
                  by = "description"
                ) %>%
                select(-"description")
            }
          }
        }
      ) %>%
        bind_rows() %>%
        arrange(.data$contrast, .data$parameter) %>%
        as.data.frame()
    }
    if (nrow(x@Model$summary.lincomb) == 0) {
      lc <- x@Model$summary.lincomb.derived
    } else {
      lc <- x@Model$summary.lincomb
    }
    contrast_estimate <- tibble(
      description = rownames(lc), estimate = lc$mean,
      lower_confidence_limit = lc[, "0.025quant"],
      upper_confidence_limit = lc[, "0.975quant"]
    ) %>%
      inner_join(
        contrast %>%
          select(-"analysis"),
        by = "description"
      ) %>%
      select(
        contrast = .data$fingerprint, .data$estimate,
        .data$lower_confidence_limit, .data$upper_confidence_limit
      ) %>%
      arrange(.data$contrast) %>%
      as.data.frame()
    new(
      "n2kResult",
      AnalysisMetadata = x@AnalysisMetadata,
      AnalysisFormula = lapply(x@AnalysisMetadata$formula, as.formula),
      AnalysisRelation = x@AnalysisRelation,
      AnalysisVersion = x@AnalysisVersion,
      RPackage = x@RPackage,
      AnalysisVersionRPackage = x@AnalysisVersionRPackage,
      Parameter = anomaly@Parameter,
      ParameterEstimate = anomaly@ParameterEstimate,
      AnomalyType = anomaly@AnomalyType,
      Anomaly = anomaly@Anomaly,
      Contrast = contrast,
      ContrastCoefficient = contrast_coefficient,
      ContrastEstimate = contrast_estimate
    )
  }
)