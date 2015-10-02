#' Import the results into the database
#'
#' @param result A n2kResult object (e.g. output of get_result())
#' @param result.channel An open ODBC connection to the database
#'
#' @return invisble NULL
#' @export
#' @importFrom n2khelper odbc_get_multi_id odbc_get_id
import_result <- function(result, result.channel){
  if (!inherits(result, "n2kResult")) {
    stop("result must be a 'n2kResult' object")
  }

  # Store R package versions
  message("Storing ", nrow(result@RPackage), " used R packages")
  r.package <- odbc_get_multi_id(
    result@RPackage[, c("Description", "Version")],
    id.field = "ID",
    merge.field = c("Description", "Version"),
    table = "RPackage",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  )
  r.package <- merge(result@RPackage, r.package)[, c("Fingerprint", "ID")]
  analysis.version.r.package <- merge(
    result@AnalysisVersionRPackage,
    r.package,
    by.x = "RPackage",
    by.y = "Fingerprint"
  )
  analysis.version.r.package$RPackage <- analysis.version.r.package$ID
  analysis.version.r.package$ID <- NULL
  rm(r.package)
  gc()

  # Store analysis versions
  message("Storing ", nrow(result@AnalysisVersion), " analysis versions")
  analysis.version <- result@AnalysisVersion
  colnames(analysis.version) <- "Description"
  analysis.version <- odbc_get_multi_id(
    analysis.version,
    id.field = "ID",
    merge.field = "Description",
    table = "AnalysisVersion",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  )
  analysis.version.r.package <- merge(
    analysis.version.r.package,
    analysis.version,
    by.x = "AnalysisVersion",
    by.y = "Description"
  )
  analysis.version.r.package$AnalysisVersion <- analysis.version.r.package$ID
  analysis.version.r.package$ID <- NULL
  colnames(analysis.version.r.package) <- paste0(
    colnames(analysis.version.r.package),
    "ID"
  )

  # store used R packages per analysis version
  message(
    "Storing used packages per analysis version: ",
    nrow(analysis.version.r.package), " rows"
  )
  odbc_get_multi_id(
    analysis.version.r.package,
    id.field = "ID",
    merge.field = c("AnalysisVersionID", "RPackageID"),
    table = "AnalysisVersionRPackage",
    channel = result.channel,
    create = TRUE,
    select = FALSE
  )
  rm(analysis.version.r.package)
  gc()

  # add analysis version ID to metadata
  analysis.metadata <- merge(
    result@AnalysisMetadata,
    analysis.version,
    by.x = "AnalysisVersion",
    by.y = "Description"
  )
  analysis.metadata$AnalysisVersionID <- analysis.metadata$ID
  analysis.metadata$ID <- NULL
  analysis.metadata$AnalysisVersion <- NULL
  analysis.metadata$Formula <- NULL
  analysis.metadata$SchemeID <- NULL
  rm(analysis.version)
  gc()

  # store model type
  model.type <- unique(analysis.metadata[, "ModelType", drop = FALSE])
  message("Storing ", nrow(model.type), " model types")
  colnames(model.type) <- "Description"
  model.type <- odbc_get_multi_id(
    model.type,
    id.field = "ID",
    merge.field = "Description",
    table = "ModelType",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  )
  analysis.metadata <- merge(
    analysis.metadata,
    model.type,
    by.x = "ModelType",
    by.y = "Description"
  )
  analysis.metadata$ModelTypeID <- analysis.metadata$ID
  analysis.metadata$ID <- NULL
  analysis.metadata$ModelType <- NULL
  rm(model.type)
  gc()

  # store model set
  model.set <- unique(analysis.metadata[
    , c("ModelTypeID", "FirstImportedYear", "LastImportedYear", "Duration")
  ])
  message("Storing ", nrow(model.set), " model sets")
  colnames(model.set)[2:3] <- c("FirstYear", "LastYear")
  model.set <- odbc_get_multi_id(
    model.set,
    id.field = "ID",
    merge.field = c("FirstYear", "LastYear", "Duration", "ModelTypeID"),
    table = "ModelSet",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  )
  colnames(model.set)[1:3] <- c(
    "ModelSetID", "FirstImportedYear", "LastImportedYear"
  )
  analysis.metadata <- merge(analysis.metadata, model.set)
  rm(model.set)
  analysis.metadata$ModelTypeID <- NULL
  analysis.metadata$FirstImportedYear <- NULL
  analysis.metadata$LastImportedYear <- NULL
  analysis.metadata$Duration <- NULL
  gc()

  # get status id
  status.id <- data.frame(
    Description = unique(analysis.metadata$Status)
  )
  message("Storing ", nrow(status.id), " statuses")
  status.id <- odbc_get_multi_id(
    status.id,
    id.field = "ID",
    merge.field = "Description",
    table = "AnalysisStatus",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  )
  colnames(status.id) <- c("StatusID", "Status")
  analysis.metadata <- merge(analysis.metadata, status.id)
  rm(status.id)
  analysis.metadata$Status <- NULL
  gc()

  # store analysis metadata
  message("Storing metadata of ", nrow(analysis.metadata), " analysis")
  colnames(analysis.metadata) <- gsub(
    "^LastAnalysedYear",
    "LastYear",
    colnames(analysis.metadata)
  )
  colnames(analysis.metadata) <- gsub(
    "^FileFingerprint",
    "Fingerprint",
    colnames(analysis.metadata)
  )
  analysis.metadata <- odbc_get_multi_id(
    analysis.metadata,
    id.field = "ID",
    merge.field = "Fingerprint",
    table = "Analysis",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  )

  # store analysis relation
  message("Storing ", nrow(result@AnalysisRelation), " analysis relations")
  analysis.relation <- merge(
    result@AnalysisRelation[, c("Analysis", "ParentAnalysis")],
    analysis.metadata,
    by.x = "Analysis",
    by.y = "Fingerprint"
  )
  analysis.relation$AnalysisID <- analysis.relation$ID
  analysis.relation$ID <- NULL
  parent.analysis <- odbc_get_multi_id(
    data = data.frame(
      Fingerprint = unique(analysis.relation$ParentAnalysis)
    ),
    id.field = "ID",
    merge.field = "Fingerprint",
    table = "Analysis",
    channel = result.channel,
    create = FALSE,
    select = TRUE
  )
  analysis.relation <- merge(
    analysis.relation,
    parent.analysis,
    by.x = "ParentAnalysis",
    by.y = "Fingerprint"
  )
  analysis.relation$SourceAnalysisID <- analysis.relation$ID
  analysis.relation <- odbc_get_multi_id(
    analysis.relation[, c("AnalysisID", "SourceAnalysisID")],
    id.field = "ID",
    merge.field = c("AnalysisID", "SourceAnalysisID"),
    table = "AnalysisRelation",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  )
  rm(parent.analysis, analysis.relation)
  gc()
  parameter.estimate <- merge(
    result@ParameterEstimate,
    analysis.metadata,
    by.x = "Analysis",
    by.y = "Fingerprint"
  )
  colnames(parameter.estimate) <- gsub(
    "^ID$",
    "AnalysisID",
    colnames(parameter.estimate)
  )
  parameter.estimate$Analysis <- NULL
  anomaly <- merge(
    result@Anomaly,
    analysis.metadata,
    by.x = "Analysis",
    by.y = "Fingerprint"
  )
  colnames(anomaly) <- gsub("^ID$", "AnalysisID", colnames(anomaly))
  anomaly$Analysis <- NULL
  contrast <- merge(
    result@Contrast,
    analysis.metadata,
    by.x = "Analysis",
    by.y = "Fingerprint"
  )
  colnames(contrast) <- gsub("^ID$", "AnalysisID", colnames(contrast))
  contrast$Analysis <- NULL
  rm(analysis.metadata)
  gc()

  # Store the contrast
  message("Storing ", nrow(contrast), " contrasts")
  contrast.id <- odbc_get_multi_id(
    contrast[, c("AnalysisID", "Description")],
    id.field = "ID",
    merge.field = c("AnalysisID", "Description"),
    table = "Contrast",
    channel = result.channel,
    create = TRUE
  )
  contrast <- merge(contrast, contrast.id)[, c("Fingerprint", "ID")]
  rm(contrast.id)
  colnames(contrast) <- gsub("^ID$", "ContrastID", colnames(contrast))
  contrast.estimate <- merge(
    result@ContrastEstimate,
    contrast,
    by.x = "Contrast",
    by.y = "Fingerprint"
  )
  contrast.estimate$Contrast <- NULL
  contrast.coefficient <- merge(
    result@ContrastCoefficient,
    contrast,
    by.x = "Contrast",
    by.y = "Fingerprint"
  )
  contrast.coefficient$Contrast <- NULL
  rm(contrast)
  gc()

  # Store the contrast estimate
  message("Storing ", nrow(contrast.estimate), " contrast estimates")
  colnames(contrast.estimate) <- gsub(
    "^LowerConfidenceLimit$",
    "LCL",
    colnames(contrast.estimate)
  )
  colnames(contrast.estimate) <- gsub(
    "^UpperConfidenceLimit$",
    "UCL",
    colnames(contrast.estimate)
  )
  odbc_get_multi_id(
    contrast.estimate,
    id.field = "ID",
    merge.field = "ContrastID",
    table = "ContrastEstimate",
    channel = result.channel,
    create = TRUE,
    select = FALSE
  )
  rm(contrast.estimate)
  gc()

  # Store the anomaly type
  message("Storing ", nrow(result@AnomalyType), " anomaly types")
  anomaly.type <- odbc_get_multi_id(
    result@AnomalyType[, "Description", drop = FALSE],
    id.field = "ID",
    merge.field = "Description",
    table = "AnomalyType",
    channel = result.channel,
    create = TRUE
  )
  anomaly.type <- merge(result@AnomalyType, anomaly.type)
  colnames(anomaly.type) <- gsub(
    "^Fingerprint$",
    "AnomalyType",
    colnames(anomaly.type)
  )
  colnames(anomaly.type) <- gsub("^ID$", "TypeID", colnames(anomaly.type))
  anomaly <- merge(anomaly, anomaly.type[, c("AnomalyType", "TypeID")])
  rm(anomaly.type)
  gc()
  anomaly$AnomalyType <- NULL

  # set the datafield
  datafield.type <- odbc_get_multi_id(
    data = data.frame(Description = unique(anomaly$Datafield)),
    id.field = "ID",
    merge.field = "Description",
    table = "DatafieldType",
    channel = result.channel,
    create = FALSE,
    select = TRUE
  )
  anomaly <- merge(
    anomaly,
    datafield.type,
    by.x = "Datafield",
    by.y = "Description"
  )
  rm(datafield.type)
  gc()
  anomaly$Datafield <- anomaly$ID
  anomaly$ID <- NULL

  anomaly.df <- unique(anomaly[, c("DatasourceID", "Datafield")])
  colnames(anomaly.df) <- gsub("^Datafield$", "TypeID", colnames(anomaly.df))
  anomaly.df <- odbc_get_multi_id(
    data = anomaly.df,
    id.field = "ID",
    merge.field = c("DatasourceID", "TypeID"),
    table = "Datafield",
    channel = result.channel,
    create = FALSE,
    select = TRUE
  )
  anomaly <- merge(
    anomaly,
    anomaly.df,
    by.x = c("DatasourceID", "Datafield"),
    by.y = c("DatasourceID", "TypeID")
  )
  rm(anomaly.df)
  gc()
  anomaly$DatasourceID <- NULL
  anomaly$Datafield <- NULL
  colnames(anomaly) <- gsub("^ID$", "DatafieldID", colnames(anomaly))

  # store parameters: first parents, then childern, then grandchildren, ...
  message("Storing ", nrow(result@Parameter), " parameters: ", appendLF = FALSE)
  parameter <- result@Parameter
  parameter$ID <- NA
  parameter$ParentParameterID <- NA
  while (TRUE) {
    selection <- which(
      is.na(parameter$ID) &
      (is.na(parameter$Parent) | !is.na(parameter$ParentParameterID))
    )
    if (length(selection) == 0) {
      if (anyNA(parameter$ID)) {
        stop("Some parameters have missing ID")
      }
      break
    }
    message(length(selection), " ", appendLF = FALSE)
    parameter.id <- odbc_get_multi_id(
      parameter[selection, c("ParentParameterID", "Description")],
      id.field = "ID",
      merge.field = c("ParentParameterID", "Description"),
      table = "Parameter",
      channel = result.channel,
      create = TRUE,
      select = TRUE
    )
    parameter$ID[selection] <- parameter.id$ID
    parent <- parameter[selection, c("Fingerprint", "ID")]
    colnames(parent) <- c("Parent", "ParentID")
    parameter <- merge(parameter, parent, all.x = TRUE)
    selection <- which(!is.na(parameter$ParentID))
    parameter$ParentParameterID[selection] <- parameter$ParentID[selection]
    parameter$ParentID <- NULL
  }
  rm(parameter.id, parent)
  colnames(parameter) <- gsub("^ID$", "ParameterID", colnames(parameter))
  parameter <- parameter[, c("Fingerprint", "ParameterID")]
  gc()
  parameter.estimate <- merge(
    parameter,
    parameter.estimate,
    by.x = "Fingerprint",
    by.y = "Parameter"
  )
  parameter.estimate$Fingerprint <- NULL
  anomaly <- merge(
    parameter,
    anomaly,
    by.x = "Fingerprint",
    by.y = "Parameter"
  )
  anomaly$Fingerprint <- NULL
  contrast.coefficient <- merge(
    parameter,
    contrast.coefficient,
    by.x = "Fingerprint",
    by.y = "Parameter"
  )
  contrast.coefficient$Fingerprint <- NULL
  rm(parameter)
  gc()

  # Storing contrast coefficients
  message("Storing ", nrow(contrast.coefficient), " contrast coefficients")
  colnames(contrast.coefficient) <- gsub(
    "^Coefficient$",
    "Constant",
    colnames(contrast.coefficient)
  )
  odbc_get_multi_id(
    contrast.coefficient,
    id.field = "ID",
    merge.field = c("ContrastID", "ParameterID"),
    table = "ContrastCoefficient",
    channel = result.channel,
    create = TRUE,
    select = FALSE
  )
  rm(contrast.coefficient)
  gc()

  # Storing parameter estimates
  message("\nStoring ", nrow(parameter.estimate), " parameter estimates")
  colnames(parameter.estimate) <- gsub(
    "^LowerConfidenceLimit$",
    "LCL",
    colnames(parameter.estimate)
  )
  colnames(parameter.estimate) <- gsub(
    "^UpperConfidenceLimit$",
    "UCL",
    colnames(parameter.estimate)
  )
  parameter.estimate <- odbc_get_multi_id(
    parameter.estimate,
    id.field = "ID",
    merge.field = c("AnalysisID", "ParameterID"),
    table = "ParameterEstimate",
    channel = result.channel,
    create = TRUE,
    select = TRUE
  )
  colnames(parameter.estimate) <- gsub(
    "^ID$",
    "ParameterEstimateID",
    colnames(parameter.estimate)
  )
  anomaly <- merge(anomaly, parameter.estimate)
  rm(parameter.estimate)
  anomaly$ParameterID <- NULL
  gc()

  # Storing anomalies
  message("Storing ", nrow(anomaly), " anomalies")
  anomaly$Estimate <- NULL
  odbc_get_multi_id(
    anomaly,
    id.field = "ID",
    merge.field = c(
      "AnalysisID", "TypeID", "ParameterEstimateID", "DatafieldID"
    ),
    table = "Anomaly",
    channel = result.channel,
    create = TRUE,
    select = FALSE
  )

  return(invisible(NULL))
}
