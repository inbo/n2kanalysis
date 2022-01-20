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
          AnalysisFormula = lapply(x@AnalysisMetadata$Formula, as.formula),
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
        Description = description,
        Analysis = get_file_fingerprint(x)
      ) %>%
      mutate(
        Fingerprint = map2_chr(
          .data$Description,
          .data$Analysis,
          ~sha1(c(Description = .x, Analysis = .y))
        )
      ) %>%
      select("Fingerprint", "Description", "Analysis") %>%
      as.data.frame()
    if (is.null(get_model(x))) {
      return(
        new(
          "n2kResult",
          AnalysisMetadata = x@AnalysisMetadata,
          AnalysisFormula = lapply(x@AnalysisMetadata$Formula, as.formula),
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

    fixed_parameterid <- anomaly@Parameter %>%
      semi_join(
        anomaly@Parameter %>%
          filter(.data$Description == "Fixed effect"),
        by = c("Parent" = "Fingerprint")
      ) %>%
      select(
        ParentDescription = .data$Description,
        Parent = .data$Fingerprint
      ) %>%
      left_join(anomaly@Parameter, by = "Parent") %>%
      transmute(
        Parameter = ifelse(
          is.na(.data$Fingerprint),
          .data$Parent,
          .data$Fingerprint
        ),
        ParameterID = concat(
          child = .data$Description,
          parent = .data$ParentDescription
        )
      )

    if (is.matrix(x@LinearCombination)) {
      contrast_coefficient <- x@LinearCombination
      contrast_coefficient[abs(contrast_coefficient) < 1e-8] <- NA
      contrast_coefficient <- contrast_coefficient %>%
        as.data.frame() %>%
        rownames_to_column("Description") %>%
        gather_(
          "ParameterID",
          "Coefficient",
          colnames(contrast_coefficient)[
            !grepl("Description", colnames(contrast_coefficient))
          ],
          na.rm = TRUE
        ) %>%
        inner_join(
          contrast %>%
            select(-"Analysis"),
          by = "Description"
        ) %>%
        select(-"Description", Contrast = .data$Fingerprint) %>%
        mutate(ParameterID = gsub("[\\(|\\)]", "", .data$ParameterID)) %>%
        inner_join(fixed_parameterid, by = "ParameterID") %>%
        select(.data$Contrast, .data$Parameter, .data$Coefficient) %>%
        arrange(.data$Contrast, .data$Parameter) %>%
        as.data.frame()
    } else {
      contrast_coefficient <- lapply(
        names(x@LinearCombination),
        function(y) {
          if (is.vector(x@LinearCombination[[y]])) {
            data.frame(
              Contrast = contrast$Fingerprint,
              ParameterID = gsub("[\\(|\\)]", "", y),
              Coefficient = x@LinearCombination[[y]],
              stringsAsFactors = FALSE
            ) %>%
              filter(abs(.data$Coefficient) >= 1e-8) %>%
              inner_join(fixed_parameterid, by = "ParameterID") %>%
              select(.data$Contrast, .data$Parameter, .data$Coefficient)
          } else {
            random_id <- anomaly@Parameter %>%
              semi_join(
                anomaly@Parameter %>%
                semi_join(
                  anomaly@Parameter %>%
                    semi_join(
                      data.frame(
                        Description = "Random effect BLUP",
                        stringsAsFactors = FALSE
                      ),
                      by = "Description"
                    ) %>%
                    mutate(Description = y),
                  by = c("Parent" = "Fingerprint", "Description")
                ),
                by = c("Parent" = "Fingerprint")
              ) %>%
              select(-"Parent", Parameter = .data$Fingerprint)
            lc <- x@LinearCombination[[y]] %>%
              as.data.frame()
            lc[abs(lc) < 1e-8] <- NA
            if (anyDuplicated(x@Model$summary.random[[y]]$ID) == 0) {
              lc %>%
                mutate(Contrast = contrast$Fingerprint) %>%
                gather_(
                  "Description",
                  "Coefficient",
                  colnames(lc)[
                    !grepl("Contrast", colnames(lc))
                  ],
                  na.rm = TRUE,
                  factor_key = TRUE
                ) %>%
                mutate(
                  Description = as.character(
                    x@Model$summary.random[[y]]$ID[.data$Description]
                  )
                ) %>%
                inner_join(random_id, by = "Description") %>%
                select(-"Description")
            } else {
              lc %>%
                mutate(Contrast = contrast$Fingerprint) %>%
                gather_(
                  "Description",
                  "Coefficient",
                  colnames(lc)[
                    !grepl("Contrast", colnames(lc))
                  ],
                  na.rm = TRUE
                ) %>%
                inner_join(
                  anomaly@Parameter %>%
                    inner_join(
                      random_id %>%
                        rename(Main = "Description"),
                      by = c("Parent" = "Parameter")
                    ) %>%
                    mutate(
                      Description = sprintf(
                        "%s:%s", .data$Main, .data$Description
                      )
                    ) %>%
                    select(Parameter = .data$Fingerprint, .data$Description),
                  by = "Description"
                ) %>%
                select(-"Description")
            }
          }
        }
      ) %>%
        bind_rows() %>%
        arrange(.data$Contrast, .data$Parameter) %>%
        as.data.frame()
    }
    if (nrow(x@Model$summary.lincomb) == 0) {
      lc <- x@Model$summary.lincomb.derived
    } else {
      lc <- x@Model$summary.lincomb
    }
    contrast_estimate <- tibble(
      Description = rownames(lc),
      Estimate = lc$mean,
      LowerConfidenceLimit = lc[, "0.025quant"],
      UpperConfidenceLimit = lc[, "0.975quant"]
    ) %>%
      inner_join(
        contrast %>%
          select(-"Analysis"),
        by = "Description"
      ) %>%
      select(
        Contrast = .data$Fingerprint,
        .data$Estimate,
        .data$LowerConfidenceLimit,
        .data$UpperConfidenceLimit
      ) %>%
      arrange(.data$Contrast) %>%
      as.data.frame()
    new(
      "n2kResult",
      AnalysisMetadata = x@AnalysisMetadata,
      AnalysisFormula = lapply(x@AnalysisMetadata$Formula, as.formula),
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
