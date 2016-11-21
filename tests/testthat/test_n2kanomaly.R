context("n2kAnomaly")
require(dplyr)
schemeid <- 1L
speciesid <- 2:4
locationgroupid <- 5L
version <- get_analysis_version(sessionInfo())
analysisdate <- runif(
  length(speciesid),
  min = as.POSIXct("2000-01-01", tz = "UTC"),
  max = as.POSIXct("2015-01-01", tz = "UTC")
) %>%
  as.POSIXct(origin = "1970-01-01 00:00.00 UTC", tz = "Europe/Brussels")
metadata <- data.frame(
  SchemeID = schemeid,
  SpeciesGroupID = speciesid,
  LocationGroupID = locationgroupid,
  ModelType = "Unit test",
  Formula = "y ~ x",
  FirstImportedYear = 2000L,
  LastImportedYear = 2010L,
  Duration = 11L,
  LastAnalysedYear = 2010L,
  AnalysisDate = analysisdate,
  Seed = 12345L,
  AnalysisVersion = version@AnalysisVersion$Fingerprint,
  Status = "converged",
  stringsAsFactors = FALSE
)
metadata$FileFingerprint <- metadata %>%
  select_(~-Status) %>%
  apply(1, sha1)
metadata$StatusFingerprint <- metadata %>%
  select_(~FileFingerprint, ~Status) %>%
  apply(1, sha1)

datasourceid <- sha1(letters)

parameter <- data.frame(
  Description = c("Unit test", "Unit test letters"),
  Parent = NA,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate_(
    Fingerprint = ~sha1(c(Description = Description, Parent = Parent))
  )
parameter <- expand.grid(
    Description = seq_len(10),
    Parent = parameter$Description,
    stringsAsFactors = FALSE
  ) %>%
  mutate_(
    Description = ~ifelse(
      grepl("letters", Parent),
      LETTERS[Description],
      Description
    ),
    Description = ~paste("Unit test", Description)
  ) %>%
  inner_join(parameter, by = c("Parent" = "Description")) %>%
  select_(~Description, Parent = ~Fingerprint) %>%
  rowwise() %>%
  mutate_(
    Fingerprint = ~sha1(c(Description = Description, Parent = Parent))
  ) %>%
  bind_rows(parameter) %>%
  as.data.frame()
parameterestimate <- expand.grid(
  Analysis = metadata$FileFingerprint,
  Parameter = parameter$Fingerprint,
  stringsAsFactors = FALSE
) %>%
  mutate_(
    Estimate = ~rnorm(n()),
    SE = ~runif(n()),
    LowerConfidenceLimit = ~Estimate - SE,
    UpperConfidenceLimit = ~Estimate + SE
  ) %>%
  select_(~-SE)

anomalytype <- data.frame(
  Description = c("Unit test", "Unit test 2"),
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate_(
    Fingerprint = ~sha1(c(Description = Description))
  ) %>%
  as.data.frame()
anomaly <- expand.grid(
  AnomalyType = anomalytype$Fingerprint,
  Analysis = metadata$FileFingerprint,
  Parameter = sample(parameter$Fingerprint, min(5, nrow(parameter))),
  DatasourceID = datasourceid,
  Datafield = "UnitTest",
  stringsAsFactors = FALSE
) %>%
  mutate_(Estimate = ~seq_along(Analysis))
expect_is(
  new(
    "n2kAnomaly",
    Parameter = parameter,
    ParameterEstimate = parameterestimate
  ),
  "n2kAnomaly"
)
expect_is(
  new(
    "n2kAnomaly",
    Parameter = parameter,
    ParameterEstimate = parameterestimate,
    AnomalyType = anomalytype
  ),
  "n2kAnomaly"
)
expect_is(
  new(
    "n2kAnomaly",
    Parameter = parameter,
    ParameterEstimate = parameterestimate,
    AnomalyType = anomalytype,
    Anomaly = anomaly
  ),
  "n2kAnomaly"
)
# check for duplicates
expect_error(
  new(
    "n2kAnomaly",
    Parameter = parameter,
    ParameterEstimate = parameterestimate,
    AnomalyType = anomalytype,
    Anomaly = cbind(anomaly, anomaly)
  ),
  paste(
    "Each variable must have a unique name"
  )
)
expect_error(
  new(
    "n2kAnomaly",
    Parameter = parameter,
    ParameterEstimate = parameterestimate,
    AnomalyType = anomalytype,
    Anomaly = rbind(anomaly, anomaly)
  ),
  "Duplicated anomalies"
)
expect_error(
  new(
    "n2kAnomaly",
    Parameter = parameter,
    ParameterEstimate = parameterestimate,
    AnomalyType = rbind(anomalytype, anomalytype),
    Anomaly = anomaly
  ),
  "Duplicated anomalytypes"
)

# check for matching rows
expect_error(
  new(
    "n2kAnomaly",
    Parameter = parameter,
    AnomalyType = anomalytype,
    Anomaly = anomaly
  ),
"Mismatch on Analysis and Parameter between Anomaly and ParameterEstimate slot"
)
expect_error(
  new(
    "n2kAnomaly",
    Parameter = parameter,
    ParameterEstimate = parameterestimate,
    Anomaly = anomaly
  ),
  "Some Anomaly have no matching Fingerprint in 'AnomalyType'"
)
