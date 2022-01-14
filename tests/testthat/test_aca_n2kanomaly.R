context("n2kAnomaly")
schemeid <- 1L
speciesid <- 2:4
locationgroupid <- 5L
version <- get_analysis_version(sessionInfo())
analysisdate <- runif(
  length(speciesid),
  min = as.POSIXct("2000-01-01", tz = "UTC"),
  max = as.POSIXct("2015-01-01", tz = "UTC")
) %>%
  as.POSIXct(origin = "1970-01-01 00:00.00 UTC", tz = "Europe/Brussels") # nolint: nonportable_path_linter, line_length_linter.
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
  select(-"Status") %>%
  apply(1, sha1)
metadata$StatusFingerprint <- metadata %>%
  select("FileFingerprint", "Status") %>%
  apply(1, sha1)

datafieldid <- sha1(letters)

parameter <- data.frame(
  Description = c("Unit test", "Unit test letters"),
  Parent = NA,
  stringsAsFactors = FALSE
) %>%
  mutate(
    Fingerprint = map2_chr(
      .data$Description,
      .data$Parent,
      ~sha1(c(Description = .x, Parent = .y))
    )
  )
parameter <- expand.grid(
    Description = seq_len(10),
    Parent = parameter$Description,
    stringsAsFactors = FALSE
  ) %>%
  mutate(
    Description = ifelse(
      grepl("letters", .data$Parent),
      LETTERS[.data$Description],
      .data$Description
    ),
    Description = paste("Unit test", .data$Description)
  ) %>%
  inner_join(parameter, by = c("Parent" = "Description")) %>%
  select("Description", Parent = "Fingerprint") %>%
  mutate(
    Fingerprint = map2_chr(
      .data$Description,
      .data$Parent,
      ~sha1(c(Description = .x, Parent = .y))
    )
  ) %>%
  bind_rows(parameter) %>%
  as.data.frame()
parameterestimate <- expand.grid(
  Analysis = metadata$FileFingerprint,
  Parameter = parameter$Fingerprint,
  stringsAsFactors = FALSE
) %>%
  mutate(
    Estimate = rnorm(n()),
    SE = runif(n()),
    LowerConfidenceLimit = .data$Estimate - .data$SE,
    UpperConfidenceLimit = .data$Estimate + .data$SE
  ) %>%
  select(-"SE")

anomalytype <- data.frame(
  Description = c("Unit test", "Unit test 2"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    Fingerprint = map_chr(
      .data$Description,
      ~sha1(c(Description = .x))
    )
  ) %>%
  as.data.frame()
anomaly <- expand.grid(
  AnomalyType = anomalytype$Fingerprint,
  Analysis = metadata$FileFingerprint,
  Parameter = sample(parameter$Fingerprint, min(5, nrow(parameter))),
  DataFieldID = datafieldid,
  Observation = "1",
  stringsAsFactors = FALSE
) %>%
  mutate(Estimate = seq_along(.data$Analysis))
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
    "must not be duplicated"
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
