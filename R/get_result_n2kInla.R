#' @rdname get_result
#' @importFrom methods setMethod new
#' @importFrom dplyr %>% data_frame rowwise mutate_ inner_join select_ transmute_ arrange_ filter_ semi_join rename
#' @importFrom digest sha1
#' @importFrom tidyr gather_
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom stats as.formula
#' @include n2kResult_class.R
#' @include n2kInla_class.R
setMethod(
  f = "get_result",
  signature = signature(x = "n2kInla"),
  definition = function(x, verbose = TRUE, ...){
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

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
    contrast <- data_frame(
        Description = description,
        Analysis = get_file_fingerprint(x)
      ) %>%
      rowwise() %>%
      mutate_(
        Fingerprint = ~sha1(
          c(Description = Description, Analysis = Analysis)
        )
      ) %>%
      select_(~Fingerprint, ~Description, ~Analysis) %>%
      as.data.frame()
    if (is.null(x@Model)) {
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

    concat <- function(parent, child){
      child[is.na(child)] <- ""
      parent.split <- strsplit(parent, ":")
      child.split <- strsplit(child, ":")
      too.short <- sapply(child.split, length) < sapply(parent.split, length)
      child.split[too.short] <- lapply(child.split[too.short], c, "")
      sapply(
        seq_along(parent.split),
        function(i){
          rbind(parent.split[[i]], child.split[[i]])
        }
      )
      apply(
        cbind(parent.split, child.split),
        1,
        function(z){
          do.call(
            function(...){
              paste0(..., collapse = ":")
            },
            z
          )
        }
      )
    }

    fixed.parameterid <- anomaly@Parameter %>%
      semi_join(
        anomaly@Parameter %>%
          filter_(~Description == "Fixed effect"),
        by = c("Parent" = "Fingerprint")
      ) %>%
      select_(ParentDescription = ~Description, Parent = ~Fingerprint) %>%
      left_join(anomaly@Parameter, by = "Parent") %>%
      transmute_(
        Parameter = ~ifelse(is.na(Fingerprint), Parent, Fingerprint),
        ParameterID = ~concat(child = Description, parent = ParentDescription)
      )

    if (is.matrix(x@LinearCombination)) {
      contrast.coefficient <- x@LinearCombination
      contrast.coefficient[abs(contrast.coefficient) < 1e-8] <- NA
      contrast.coefficient <- contrast.coefficient %>%
        as.data.frame() %>%
        rownames_to_column("Description") %>%
        gather_(
          "ParameterID",
          "Coefficient",
          colnames(contrast.coefficient)[
            !grepl("Description", colnames(contrast.coefficient))
          ],
          na.rm = TRUE
        ) %>%
        inner_join(
          contrast %>%
            select_(~-Analysis),
          by = "Description"
        ) %>%
        select_(~-Description, Contrast = ~Fingerprint) %>%
        mutate_(ParameterID = ~gsub("[\\(|\\)]", "", ParameterID)) %>%
        inner_join(fixed.parameterid, by = "ParameterID") %>%
        select_(~Contrast, ~Parameter, ~Coefficient) %>%
        arrange_(~Contrast, ~Parameter) %>%
        as.data.frame()
    } else {
      contrast.coefficient <- lapply(
        names(x@LinearCombination),
        function(y){
          if (is.vector(x@LinearCombination[[y]])) {
            data.frame(
              Contrast = contrast$Fingerprint,
              ParameterID = gsub("[\\(|\\)]", "", y),
              Coefficient = x@LinearCombination[[y]],
              stringsAsFactors = FALSE
            ) %>%
              filter_(~abs(Coefficient) >= 1e-8) %>%
              inner_join(fixed.parameterid, by = "ParameterID") %>%
              select_(~Contrast, ~Parameter, ~Coefficient)
          } else {
            random.id <- anomaly@Parameter %>%
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
                    mutate_(Description = ~y),
                  by = c("Parent" = "Fingerprint", "Description")
                ),
                by = c("Parent" = "Fingerprint")
              ) %>%
              select_(~-Parent, Parameter = ~Fingerprint)
            lc <- x@LinearCombination[[y]] %>%
              as.data.frame()
            lc[abs(lc) < 1e-8] <- NA
            if (anyDuplicated(x@Model$summary.random[[y]]$ID) == 0) {
              lc %>%
                mutate_(Contrast = ~contrast$Fingerprint) %>%
                gather_(
                  "Description",
                  "Coefficient",
                  colnames(lc)[
                    !grepl("Contrast", colnames(lc))
                  ],
                  na.rm = TRUE,
                  factor_key = TRUE
                ) %>%
                mutate_(
                  Description = ~ as.character(
                    x@Model$summary.random[[y]]$ID[Description]
                  )
                ) %>%
                inner_join(random.id, by = "Description") %>%
                select_(~-Description)
            } else {
              lc %>%
                mutate_(Contrast = ~contrast$Fingerprint) %>%
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
                      random.id %>%
                        rename(Main = "Description"),
                      by = c("Parent" = "Parameter")
                    ) %>%
                    mutate_(
                      Description = ~ paste(Main, Description, sep = ":")
                    ) %>%
                    select_(Parameter = ~Fingerprint, ~Description),
                  by = "Description"
                ) %>%
                select_(~-Description)
            }
          }
        }
      ) %>%
        bind_rows() %>%
        arrange_(~Contrast, ~Parameter) %>%
        as.data.frame()
    }
    if (nrow(x@Model$summary.lincomb) == 0) {
      lc <- x@Model$summary.lincomb.derived
    } else {
      lc <- x@Model$summary.lincomb
    }
    contrast.estimate <- data_frame(
      Description = rownames(lc),
      Estimate = lc$mean,
      LowerConfidenceLimit = lc[, "0.025quant"],
      UpperConfidenceLimit = lc[, "0.975quant"]
    ) %>%
      inner_join(
        contrast %>%
          select_(~-Analysis),
        by = "Description"
      ) %>%
      select_(
        Contrast = ~Fingerprint,
        ~Estimate,
        ~LowerConfidenceLimit,
        ~UpperConfidenceLimit
      ) %>%
      arrange_(~Contrast) %>%
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
      ContrastCoefficient = contrast.coefficient,
      ContrastEstimate = contrast.estimate
    )
  }
)
