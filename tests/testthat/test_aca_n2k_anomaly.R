test_that("n2kAnomaly", {
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
    scheme_id = schemeid, species_group_id = speciesid,
    location_group_id = locationgroupid, model_type = "Unit test",
    formula = "y ~ x", first_imported_year = 2000L, last_imported_year = 2010L,
    duration = 11L, last_analysed_year = 2010L, analysis_date = analysisdate,
    seed = 12345L, AnalysisVersion = version@AnalysisVersion$fingerprint,
    status = "converged", stringsAsFactors = FALSE
  )
  metadata$file_fingerprint <- metadata %>%
    select(-"status") %>%
    apply(1, sha1)
  metadata$status_fingerprint <- metadata %>%
    select("file_fingerprint", "status") %>%
    apply(1, sha1)

  datafieldid <- sha1(letters)

  parameter <- data.frame(
    description = c("Unit test", "Unit test letters"), parent = NA,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      fingerprint = map2_chr(
        .data$description, .data$parent, ~sha1(c(description = .x, Parent = .y))
      )
    )
  parameter <- expand.grid(
      description = seq_len(10), parent = parameter$description,
      stringsAsFactors = FALSE
    ) %>%
    mutate(
      description = ifelse(
        grepl("letters", .data$parent), LETTERS[.data$description],
        .data$description
      ) %>%
        sprintf(fmt = "Unit test %s")
    ) %>%
    inner_join(parameter, by = c("parent" = "description")) %>%
    select("description", parent = "fingerprint") %>%
    mutate(
      fingerprint = map2_chr(
        .data$description, .data$parent, ~sha1(c(description = .x, parent = .y))
      )
    ) %>%
    bind_rows(parameter) %>%
    as.data.frame()
  parameterestimate <- expand.grid(
    analysis = metadata$file_fingerprint, parameter = parameter$fingerprint,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      estimate = rnorm(n()), SE = runif(n()),
      lower_confidence_limit = .data$estimate - .data$SE,
      upper_confidence_limit = .data$estimate + .data$SE
    ) %>%
    select(-"SE")

  anomalytype <- data.frame(
    description = c("Unit test", "Unit test 2"), stringsAsFactors = FALSE
  ) %>%
    mutate(
      fingerprint = map_chr(.data$description, ~sha1(c(description = .x)))
    ) %>%
    as.data.frame()
  anomaly <- expand.grid(
    anomaly_type = anomalytype$fingerprint,
    analysis = metadata$file_fingerprint,
    parameter = sample(parameter$fingerprint, min(5, nrow(parameter))),
    datafield_id = datafieldid, observation = "1", stringsAsFactors = FALSE
  ) %>%
    mutate(estimate = seq_along(.data$analysis))
  expect_is(
    new(
      "n2kAnomaly", Parameter = parameter, ParameterEstimate = parameterestimate
    ),
    "n2kAnomaly"
  )
  expect_is(
    new(
      "n2kAnomaly", Parameter = parameter,
      ParameterEstimate = parameterestimate, AnomalyType = anomalytype
    ),
    "n2kAnomaly"
  )
  expect_is(
    new(
      "n2kAnomaly", Parameter = parameter,
      ParameterEstimate = parameterestimate, AnomalyType = anomalytype,
      Anomaly = anomaly
    ),
    "n2kAnomaly"
  )
  # check for duplicates
  expect_error(
    new(
      "n2kAnomaly", Parameter = parameter,
      ParameterEstimate = parameterestimate, AnomalyType = anomalytype,
      Anomaly = cbind(anomaly, anomaly)
    ),
    "duplicated column names in Anomaly"
  )
  expect_error(
    new(
      "n2kAnomaly", Parameter = parameter,
      ParameterEstimate = parameterestimate, AnomalyType = anomalytype,
      Anomaly = rbind(anomaly, anomaly)
    ),
    "Duplicated anomalies"
  )
  expect_error(
    new(
      "n2kAnomaly", Parameter = parameter,
      ParameterEstimate = parameterestimate,
      AnomalyType = rbind(anomalytype, anomalytype), Anomaly = anomaly
    ),
    "Duplicated anomalytypes"
  )

  # check for matching rows
  expect_error(
    new(
      "n2kAnomaly", Parameter = parameter, AnomalyType = anomalytype,
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
    "Some Anomaly have no matching fingerprint in 'AnomalyType'"
  )
})
