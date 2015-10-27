#' Import the results into the database
#'
#' @param result A n2kResult object (e.g. output of get_result())
#' @param result.channel An open ODBC connection to the database
#'
#' @return invisble NULL
#' @export
#' @importFrom assertthat assert_that
#' @importFrom n2khelper odbc_get_multi_id odbc_get_id
#' @importFrom dplyr %>% anti_join inner_join select_ rename_ distinct_ slice_
import_result <- function(result, result.channel){
  assert_that(inherits(result, "n2kResult"))
  assert_that(validObject(result))
  # nocov start

  # Store R package versions
  message("Storing ", nrow(result@RPackage), " used R packages")
  r.package <- odbc_get_multi_id(
    data = result@RPackage[, c("Description", "Version")],
    id.field = "ID",
    merge.field = c("Description", "Version"),
    table = "RPackage",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  )
  antijoin.rp <- result@RPackage %>%
    anti_join(r.package, by = c("Description", "Version")) %>%
    nrow()
  assert_that(antijoin.rp == 0)

  # Store analysis versions
  message("Storing ", nrow(result@AnalysisVersion), " analysis versions")
  if (inherits(result@AnalysisVersion$Fingerprint, "factor")) {
    result@AnalysisVersion <- result@AnalysisVersion %>%
      mutate_(Fingerprint = ~levels(Fingerprint)[Fingerprint])
  }
  analysis.version <- odbc_get_multi_id(
    result@AnalysisVersion %>%
      select_(Description = ~Fingerprint),
    id.field = "ID",
    merge.field = "Description",
    table = "AnalysisVersion",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  ) %>%
    rename_(AnalysisVersionID = ~ID, AnalysisVersion = ~Description)
  antijoin.av <- result@AnalysisVersion %>%
    anti_join(analysis.version, by = c("Fingerprint" = "AnalysisVersion")) %>%
    nrow()
  assert_that(antijoin.av == 0)
  if (inherits(result@AnalysisVersionRPackage$AnalysisVersion, "factor")) {
    result@AnalysisVersionRPackage <- result@AnalysisVersionRPackage %>%
      mutate_(AnalysisVersion = ~levels(AnalysisVersion)[AnalysisVersion])
  }
  analysisversion.rpackage <- result@RPackage %>%
    inner_join(r.package, by = c("Description", "Version")) %>%
    select_(RPackage = ~Fingerprint, RPackageID = ~ID) %>%
    inner_join(result@AnalysisVersionRPackage, by = "RPackage") %>%
    inner_join(analysis.version, by = "AnalysisVersion") %>%
    select_(~AnalysisVersionID, ~RPackageID)
  rm(r.package)
  gc()

  # store used R packages per analysis version
  message(
    "Storing used packages per analysis version: ",
    nrow(analysisversion.rpackage), " rows"
  )
  odbc_get_multi_id(
    analysisversion.rpackage,
    id.field = "ID",
    merge.field = c("AnalysisVersionID", "RPackageID"),
    table = "AnalysisVersionRPackage",
    channel = result.channel,
    create = TRUE,
    select = FALSE
  )
  rm(analysisversion.rpackage)
  gc()

  # store model type
  if (inherits(result@AnalysisMetadata$ModelType, "factor")) {
    result@AnalysisMetadata <- result@AnalysisMetadata %>%
      mutate_(ModelType = ~levels(ModelType)[ModelType])
  }
  modeltype <- result@AnalysisMetadata %>%
    select_(Description = ~ModelType) %>%
    distinct_()
  message("Storing ", nrow(modeltype), " model types")
  modeltype <- odbc_get_multi_id(
    modeltype,
    id.field = "ID",
    merge.field = "Description",
    table = "ModelType",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  ) %>%
    rename_(ModelType = ~Description, ModelTypeID = ~ID)
  antijoin.ammt <- result@AnalysisMetadata %>%
    anti_join(modeltype, by = "ModelType") %>%
    nrow()
  assert_that(antijoin.ammt == 0)

  # store model set
  modelset <- result@AnalysisMetadata %>%
    select_(
      ~ModelType,
      FirstYear = ~FirstImportedYear,
      LastYear = ~LastImportedYear,
      ~Duration
    ) %>%
    distinct_() %>%
    inner_join(modeltype, by = "ModelType") %>%
    select_(~-ModelType)
  message("Storing ", nrow(modelset), " model sets")
  modelset <- odbc_get_multi_id(
    modelset,
    id.field = "ID",
    merge.field = c("FirstYear", "LastYear", "Duration", "ModelTypeID"),
    table = "ModelSet",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  ) %>%
    rename_(
      ModelSetID = ~ID,
      FirstImportedYear = ~FirstYear,
      LastImportedYear = ~LastYear
    )
  antijoin.amms <- result@AnalysisMetadata %>%
    anti_join(
      modelset %>%
        inner_join(modeltype, by = "ModelTypeID"),
      by = c("ModelType", "FirstImportedYear", "LastImportedYear", "Duration")
    ) %>%
    nrow()
  assert_that(antijoin.amms == 0)

  # get status id
  if (inherits(result@AnalysisMetadata$Status, "factor")) {
    result@AnalysisMetadata <- result@AnalysisMetadata %>%
      mutate_(Status = ~levels(Status)[Status])
  }
  status <- result@AnalysisMetadata %>%
    select_(Description = ~Status) %>%
    distinct_()
  message("Storing ", nrow(status), " statuses")
  status <- odbc_get_multi_id(
    status,
    id.field = "ID",
    merge.field = "Description",
    table = "AnalysisStatus",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  ) %>%
    rename_(Status = ~Description, StatusID = ~ID)
  antijoin.ams <- result@AnalysisMetadata %>%
    anti_join(status, by = "Status") %>%
    nrow()
  assert_that(antijoin.ams == 0)

  # store analysis metadata
  if (inherits(result@AnalysisMetadata$AnalysisVersion, "factor")) {
    result@AnalysisMetadata <- result@AnalysisMetadata %>%
      mutate_(AnalysisVersion = ~levels(AnalysisVersion)[AnalysisVersion])
  }
  if (inherits(result@AnalysisMetadata$FileFingerprint, "factor")) {
    result@AnalysisMetadata <- result@AnalysisMetadata %>%
      mutate_(FileFingerprint = ~levels(FileFingerprint)[FileFingerprint])
  }
  metadata <- result@AnalysisMetadata %>%
    inner_join(analysis.version, by = "AnalysisVersion") %>%
    inner_join(modeltype, by = "ModelType") %>%
    inner_join(
      modelset,
      by = c("ModelTypeID", "FirstImportedYear", "LastImportedYear", "Duration")
    ) %>%
    inner_join(status, by = "Status") %>%
    select_(
      ~SpeciesGroupID, ~LocationGroupID, LastYear = ~LastAnalysedYear,
      Fingerprint = ~FileFingerprint, ~AnalysisDate, ~Seed, ~StatusFingerprint,
      ~AnalysisVersionID, ~ModelSetID, ~StatusID
    )
  rm(analysis.version, modeltype, modelset, status)
  gc()
  message("Storing metadata of ", nrow(metadata), " analysis")
  metadata <- odbc_get_multi_id(
    metadata,
    id.field = "ID",
    merge.field = "Fingerprint",
    table = "Analysis",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  ) %>%
    rename_(AnalysisID = ~ID, Analysis = ~Fingerprint)
  antijoin.amam <- result@AnalysisMetadata %>%
    anti_join(metadata, by = c("FileFingerprint" = "Analysis")) %>%
    nrow()
  assert_that(antijoin.amam == 0)

  # store analysis relation
  message("Storing ", nrow(result@AnalysisRelation), " analysis relations")
  if (inherits(result@AnalysisRelation$Analysis, "factor")) {
    result@AnalysisRelation  <- result@AnalysisRelation %>%
      mutate_(Analysis = ~levels(Analysis)[Analysis])
  }
  if (inherits(result@AnalysisRelation$ParentAnalysis, "factor")) {
    result@AnalysisRelation  <- result@AnalysisRelation %>%
      mutate_(ParentAnalysis = ~levels(ParentAnalysis)[ParentAnalysis])
  }
  parentanalysis <- odbc_get_multi_id(
    data = result@AnalysisRelation %>%
      select_(Fingerprint = ~ParentAnalysis) %>%
      distinct_(),
    id.field = "ID",
    merge.field = "Fingerprint",
    table = "Analysis",
    channel = result.channel,
    create = FALSE,
    select = TRUE
  ) %>%
    rename_(SourceAnalysisID = ~ID, ParentAnalysis = ~Fingerprint)
  antijoin.arpa <- result@AnalysisRelation %>%
    anti_join(parentanalysis, by = "ParentAnalysis") %>%
    nrow()
  assert_that(antijoin.arpa == 0)
  relation <-
    result@AnalysisRelation %>%
    inner_join(parentanalysis, by = "ParentAnalysis") %>%
    inner_join(metadata, by = "Analysis") %>%
    select_(~AnalysisID, ~SourceAnalysisID)
  odbc_get_multi_id(
    relation,
    id.field = "ID",
    merge.field = c("AnalysisID", "SourceAnalysisID"),
    table = "AnalysisRelation",
    channel = result.channel,
    create = TRUE,
    select = FALSE
  )
  rm(parentanalysis, relation)
  gc()


  # Store the contrast
  message("Storing ", nrow(result@Contrast), " contrasts")
  if (inherits(result@Contrast$Analysis, "factor")) {
    result@Contrast <- result@Contrast %>%
      mutate_(Analysis = ~levels(Analysis)[Analysis])
  }
  contrast <- result@Contrast %>%
    inner_join(metadata, by = "Analysis") %>%
    select_(~-Analysis)
  contrastid <- odbc_get_multi_id(
    contrast %>% select_(~-Fingerprint),
    id.field = "ID",
    merge.field = c("AnalysisID", "Description"),
    table = "Contrast",
    channel = result.channel,
    create = TRUE
  ) %>%
    rename_(ContrastID = ~ID)
  antijoin.cc <- contrast %>%
    anti_join(contrastid, by = c("AnalysisID", "Description")) %>%
    nrow()
  assert_that(antijoin.cc == 0)




  # Store the contrast estimate
  contrastestimate <- contrast %>%
    inner_join(contrastid, by = c("AnalysisID", "Description")) %>%
    select_(~ContrastID, Contrast = ~Fingerprint) %>%
    inner_join(result@ContrastEstimate, by = "Contrast") %>%
    select_(
      ~-Contrast,
      LCL = ~LowerConfidenceLimit,
      UCL = ~UpperConfidenceLimit
    )

  message("Storing ", nrow(contrastestimate), " contrast estimates")
  odbc_get_multi_id(
    contrastestimate,
    id.field = "ID",
    merge.field = "ContrastID",
    table = "ContrastEstimate",
    channel = result.channel,
    create = TRUE,
    select = FALSE
  )
  rm(contrastestimate)
  gc()

  # Store the anomaly type
  message("Storing ", nrow(result@AnomalyType), " anomaly types")
  anomalytype <- odbc_get_multi_id(
    result@AnomalyType %>%
      select_(~Description),
    id.field = "ID",
    merge.field = "Description",
    table = "AnomalyType",
    channel = result.channel,
    create = TRUE
  ) %>%
    rename_(AnomalyTypeID = ~ID)
  antijoin.atat <- result@AnomalyType %>%
    anti_join(anomalytype, by = "Description") %>%
    nrow()
  assert_that(antijoin.atat == 0)
  anomalytype <- result@AnomalyType %>%
    inner_join(anomalytype, by = "Description") %>%
    select_(~-Description, AnomalyType = ~Fingerprint)


  # set the datafield
  if (inherits(result@Anomaly$Datafield, "factor")) {
    result@Anomaly <- result@Anomaly %>%
      mutate_(Datafield = ~levels(Datafield)[Datafield])
  }
  datafield.type <- odbc_get_multi_id(
    data = result@Anomaly %>%
      select_(Description = ~Datafield) %>%
      distinct_(),
    id.field = "ID",
    merge.field = "Description",
    table = "DatafieldType",
    channel = result.channel,
    create = FALSE,
    select = TRUE
  ) %>%
    rename_(TypeID = ~ID, Datafield = ~Description)
  antijoin.adf <- result@Anomaly %>%
    anti_join(datafield.type, by = "Datafield") %>%
    nrow()
  assert_that(antijoin.adf == 0)

  datafield <- result@Anomaly %>%
    select_(~DatasourceID, ~Datafield) %>%
    distinct_() %>%
    inner_join(datafield.type, by = "Datafield")
  df2 <- odbc_get_multi_id(
    data = datafield %>%
      select_(~-Datafield),
    id.field = "ID",
    merge.field = c("DatasourceID", "TypeID"),
    table = "Datafield",
    channel = result.channel,
    create = FALSE,
    select = TRUE
  )
  antijoin.df <- datafield %>%
    anti_join(df2, by = c("DatasourceID", "TypeID")) %>%
    nrow()
  assert_that(antijoin.df == 0)
  datafield <- datafield %>%
    inner_join(df2, by = c("DatasourceID", "TypeID")) %>%
    rename_(DatafieldID = ~ID)

  # store parameters: first parents, then childern, then grandchildren, ...
  message("Storing ", nrow(result@Parameter), " parameters: ", appendLF = FALSE)
  parameter <- result@Parameter %>%
    mutate_(ID = NA, ParentParameterID = NA)
  while (TRUE) {
    selection <- parameter %>%
      filter_(~is.na(ID), ~is.na(Parent) | !is.na(ParentParameterID))
    if (nrow(selection) == 0) {
      if (anyNA(parameter$ID)) {
        stop("Some parameters have missing ID")
      }
      break
    }
    message(nrow(selection), " ", appendLF = FALSE)
    parameter.id <- odbc_get_multi_id(
      selection %>%
        select_(~ParentParameterID, ~Description),
      id.field = "ID",
      merge.field = c("ParentParameterID", "Description"),
      table = "Parameter",
      channel = result.channel,
      create = TRUE,
      select = TRUE
    ) %>%
      rename_(newID = ~ID)
    parameter <- parameter %>%
      left_join(parameter.id, by = c("ParentParameterID", "Description")) %>%
      mutate_(ID = ~ifelse(is.na(newID), ID, newID)) %>%
      select_(~-newID) %>%
      left_join(
        selection %>%
          select_(Parent = ~Fingerprint, ~ParentParameterID, ~Description) %>%
          inner_join(
            parameter.id,
            by = c("ParentParameterID", "Description")
          ) %>%
          select_(~Parent, ~newID),
        by = "Parent"
      ) %>%
      mutate_(
        ParentParameterID = ~ifelse(is.na(newID), ParentParameterID, newID)
      ) %>%
      select_(~-newID)
  }
  rm(parameter.id)
  gc()
  parameter <- parameter %>% select_(Parameter = ~Fingerprint, ParameterID = ~ID)

  # Storing contrast coefficients
  message("\nStoring ", nrow(result@ContrastCoefficient), " contrast coefficients")
  odbc_get_multi_id(
    result@ContrastCoefficient %>%
      inner_join(
        contrast %>%
          inner_join(contrastid, by = c("AnalysisID", "Description")) %>%
          select_(Contrast = ~Fingerprint, ~ContrastID),
        by = "Contrast"
      ) %>%
      inner_join(parameter, by = "Parameter") %>%
      select_(~-Contrast, ~-Parameter, Constant = ~Coefficient)
,
    id.field = "ID",
    merge.field = c("ContrastID", "ParameterID"),
    table = "ContrastCoefficient",
    channel = result.channel,
    create = TRUE,
    select = FALSE
  )
  rm(contrast, contrastid)
  gc()

  # Storing parameter estimates
  message("Storing ", nrow(result@ParameterEstimate), " parameter estimates")
  if (inherits(result@ParameterEstimate$Analysis, "factor")) {
    result@ParameterEstimate <- result@ParameterEstimate %>%
      mutate_(Analysis = ~levels(Analysis)[Analysis])
  }
  parameterestimate <- odbc_get_multi_id(
    result@ParameterEstimate %>%
      inner_join(metadata, by = "Analysis") %>%
      inner_join(parameter, by = "Parameter") %>%
      select_(
        ~AnalysisID, ~ParameterID, ~Estimate,
        LCL = ~LowerConfidenceLimit, UCL = ~UpperConfidenceLimit
      ),
    id.field = "ID",
    merge.field = c("AnalysisID", "ParameterID"),
    table = "ParameterEstimate",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  ) %>%
    rename_(ParameterEstimateID = ~ID)

  # Storing anomalies
  message("Storing ", nrow(result@Anomaly), " anomalies")
  if (result@Anomaly$Analysis %>% inherits("factor")) {
    result@Anomaly <- result@Anomaly %>%
      mutate_(Analysis = ~levels(Analysis)[Analysis])
  }
  odbc_get_multi_id(
    result@Anomaly %>%
      inner_join(metadata, by = "Analysis") %>%
      inner_join(anomalytype, by = "AnomalyType") %>%
      inner_join(datafield, by = c("DatasourceID", "Datafield")) %>%
      inner_join(parameter, by = "Parameter") %>%
      inner_join(parameterestimate, by = c("AnalysisID", "ParameterID")) %>%
      select_(~AnalysisID, ~ParameterEstimateID, TypeID = ~AnomalyTypeID, ~DatafieldID),
    id.field = "ID",
    merge.field = c(
      "AnalysisID", "TypeID", "ParameterEstimateID", "DatafieldID"
    ),
    table = "Anomaly",
    channel = result.channel,
    create = TRUE,
    select = FALSE
  )

  rm(metadata, anomalytype, datafield.type, parameter)
  gc()

  return(invisible(NULL))

  # nocov end
}
