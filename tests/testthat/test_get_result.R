context("get_result")
temp.dir <- tempdir()
this.scheme.id <- 1L
this.species.group.id <- 2L
this.location.group.id <- 3L
this.analysis.date <- Sys.time()
this.first.imported.year <- 1990L
this.last.imported.year <- 2015L
this.last.analysed.year <- 2014L
this.duration <- 1L
this.datasource <- 1L
data("cbpp", package = "lme4")
cbpp$ObservationID <- seq_len(nrow(cbpp))
cbpp$DatasourceID <- 2L
describe("get_result on n2kInlaNbinomial", {
  this.model.type <- "inla nbinomial: period + herd"
  this.formula <-
    "incidence ~ offset(log(size)) + period + f(herd, model = 'iid')"
  analysis <- n2k_inla_nbinomial(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = cbpp
  )
  result <- get_result(analysis, datasource.id = this.datasource)
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on unfitted objects", {
    expect_identical(
      nrow(result@Parameter),
      0L
    )
    expect_identical(
      nrow(result@Contrast),
      0L
    )
    expect_identical(
      nrow(result@Anomaly),
      0L
    )
  })
  filename <- paste0(temp.dir, "/", get_file_fingerprint(analysis), ".rda")
  save(analysis, file = filename)
  expect_equal(
    get_result(filename, datasource.id = this.datasource),
    result
  )
  fit_model(filename)
  load(filename)
  result <- get_result(analysis, datasource.id = this.datasource)
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on fitted objects", {
    expect_less_than(
      0,
      nrow(result@Parameter)
    )
    expect_identical(
      nrow(result@Contrast),
      0L
    )
    expect_less_than(
      0,
      nrow(result@Anomaly)
    )
  })
  expect_equal(
    get_result(filename, datasource.id = this.datasource),
    result
  )

  # with linear combination
  lin.comb <- model.matrix(~period, unique(cbpp[, "period", drop = FALSE]))
  this.parent <- "abcd"
  analysis <- n2k_inla_nbinomial(
    scheme.id = this.scheme.id,
    species.group.id = this.species.group.id,
    location.group.id = this.location.group.id,
    model.type = this.model.type,
    formula = this.formula,
    first.imported.year = this.first.imported.year,
    last.imported.year = this.last.imported.year,
    analysis.date = this.analysis.date,
    data = cbpp,
    lin.comb = lin.comb,
    parent = this.parent
  )
  result <- get_result(analysis, datasource.id = this.datasource)
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on unfitted objects", {
    expect_identical(
      nrow(result@Parameter),
      0L
    )
    expect_identical(
      nrow(result@Contrast),
      nrow(lin.comb)
    )
    expect_identical(
      nrow(result@ContrastCoefficient),
      0L
    )
    expect_identical(
      nrow(result@ContrastEstimate),
      0L
    )
    expect_identical(
      nrow(result@Anomaly),
      0L
    )
  })
  filename <- paste0(temp.dir, "/", get_file_fingerprint(analysis), ".rda")
  save(analysis, file = filename)
  expect_equal(
    get_result(filename, datasource.id = this.datasource),
    result
  )
  fit_model(filename)
  load(filename)
  result <- get_result(analysis, datasource.id = this.datasource)
  it("return a n2kResult", {
    expect_is(result, "n2kResult")
  })
  it("returns senbile output on fitted objects", {
    expect_less_than(
      0,
      nrow(result@Parameter)
    )
    expect_identical(
      nrow(result@Contrast),
      nrow(lin.comb)
    )
    expect_less_than(
      0,
      nrow(result@ContrastCoefficient)
    )
    expect_less_than(
      0,
      nrow(result@ContrastEstimate)
    )
    expect_less_than(
      0,
      nrow(result@Anomaly)
    )
  })
  expect_equal(
    get_result(filename, datasource.id = this.datasource),
    result
  )
})

expect_error(
  get_result("junk"),
  "'x' is neither an existing file, neither an existing directory"
)
expect_is(
  get_result(temp.dir, datasource.id = this.datasource, n.cluster = 1),
  "n2kResult"
)
# nolint start
# expect_is(
#   get_result(temp.dir, datasource.id = this.datasource, n.cluster = 2),
#   "n2kResult"
# )
# expect_is(
#   get_result(
#     temp.dir,
#     datasource.id = this.datasource,
#     n.cluster = 2 * parallel::detectCores()
#   ),
#   "n2kResult"
# )
# expect_message(
#   get_result(
#     temp.dir,
#     datasource.id = this.datasource,
#     n.cluster = 2 * parallel::detectCores()
#   ),
#   paste(
#     "Requesting", 2 * parallel::detectCores(), "clusters but only",
#     parallel::detectCores(), "available."
#   )
# )
# nolint end
result.fp <- get_result(
  temp.dir,
  datasource.id = this.datasource,
  keep.fingerprint = TRUE
)
expect_is(result.fp, "n2kResult")
result.id <- get_result(
  temp.dir,
  datasource.id = this.datasource,
  keep.fingerprint = FALSE
)
expect_is(result.id, "n2kResult")

# check the convertion for fingerprint to ID
analysis.level <- sort(unique(c(
  result.fp@AnalysisMetadata$FileFingerprint,
  result.fp@AnalysisRelation$ParentAnalysis
)))
analysis.id <- data_frame(
  Analysis = result.fp@AnalysisMetadata$FileFingerprint,
  Fingerprint = factor(
    result.fp@AnalysisMetadata$FileFingerprint,
    levels = analysis.level
  ),
  AnalysisID = result.id@AnalysisMetadata$FileFingerprint
)
expect_identical(
  analysis.id$Fingerprint,
  analysis.id$AnalysisID
)
analysis.id$Fingerprint <- NULL

require(dplyr, quietly = TRUE)
parameter.id <- result.fp@Parameter %>%
  as.tbl() %>%
  mutate_(ID = ~as.integer(factor(Fingerprint)))
parameter.id <- parameter.id %>%
  left_join(
    parameter.id %>%
      select_(~Fingerprint, ParentID = ~ID),
    by = c("Parent" = "Fingerprint")
  ) %>%
  select_(~-Parent)
aj.parameter <- parameter.id %>%
  anti_join(
    result.id@Parameter,
    by = c("Description", "ID" = "Fingerprint", "ParentID" = "Parent")
  )
expect_identical(nrow(aj.parameter), 0L)

parameter.estimate <- result.fp@ParameterEstimate %>%
  inner_join(analysis.id, by = "Analysis") %>%
  as.tbl() %>%
  select_(~AnalysisID, ~Parameter) %>%
  inner_join(
    parameter.id %>% select_(~Fingerprint, ParameterID = ~ID),
    by = c("Parameter" = "Fingerprint")
  ) %>%
  select_(~-Parameter)
aj.parameterestimate <- parameter.estimate %>%
  anti_join(
    result.id@ParameterEstimate,
    by = c("AnalysisID" = "Analysis", "ParameterID" = "Parameter")
  )
expect_identical(nrow(aj.parameterestimate), 0L)

contrast <- result.fp@Contrast %>%
  as.tbl() %>%
  inner_join(analysis.id, by = "Analysis") %>%
  select_(~-Analysis) %>%
  mutate_(ContrastID = ~as.integer(factor(Fingerprint)))
aj.contrast <- contrast %>%
  anti_join(
    result.id@Contrast,
    by = c(
      "ContrastID" = "Fingerprint",
      "Description",
      "AnalysisID" = "Analysis"
    )
  )
expect_identical(nrow(aj.contrast), 0L)

aj.contrastcoefficient <- result.fp@ContrastCoefficient %>%
  as.tbl() %>%
  inner_join(
    parameter.id %>%
      select_(Parameter = ~Fingerprint, ParameterID = ~ID),
    by = "Parameter"
  ) %>%
  select_(~-Parameter) %>%
  inner_join(
    contrast %>%
      select_(~Fingerprint, ~ContrastID),
    by = c("Contrast" = "Fingerprint")
  ) %>%
  anti_join(
    result.id@ContrastCoefficient,
    by = c(
      "ContrastID" = "Contrast",
      "ParameterID" = "Parameter",
      "Coefficient"
    )
  )
expect_identical(nrow(aj.contrastcoefficient), 0L)

aj.contrastestimate <- result.fp@ContrastEstimate %>%
  inner_join(
    contrast %>%
      select_(~Fingerprint, ~ContrastID),
    by = c("Contrast" = "Fingerprint")
  ) %>%
  select_(~-Contrast) %>%
  anti_join(
    result.id@ContrastEstimate,
    by = c(
      "ContrastID" = "Contrast",
      "Estimate",
      "LowerConfidenceLimit",
      "UpperConfidenceLimit"
    )
  )
expect_identical(nrow(aj.contrastestimate), 0L)

anomalytype <- result.fp@AnomalyType %>%
  mutate_(ID = ~as.integer(factor(Fingerprint)))
aj.anomalytype <- anomalytype %>%
  anti_join(result.id@AnomalyType, by = c("Description", "ID" = "Fingerprint"))
expect_identical(nrow(aj.anomalytype), 0L)

aj.anomaly <- result.fp@Anomaly %>%
  as.tbl() %>%
  inner_join(analysis.id, by = "Analysis") %>%
  select_(~-Analysis) %>%
  inner_join(
    anomalytype %>%
      select_(~Fingerprint, AnomalyTypeID = ~ID),
    by = c("AnomalyType" = "Fingerprint")
  ) %>%
  select_(~-AnomalyType) %>%
  inner_join(
    parameter.id %>%
      select_(~Fingerprint, ParameterID = ~ID),
    by = c("Parameter" = "Fingerprint")
  ) %>%
  select_(~-Parameter) %>%
  mutate_(Datafield = ~factor(Datafield)) %>%
  anti_join(
    result.id@Anomaly,
    by = c(
      "AnomalyTypeID" = "AnomalyType", "AnalysisID" = "Analysis",
      "ParameterID" = "Parameter", "DatasourceID", "Datafield")
  )
expect_identical(nrow(aj.anomaly), 0L)

analysisversion <- data_frame(
  AnalysisVersion = result.fp@AnalysisVersion$Fingerprint,
  AnalysisVersionID = result.id@AnalysisVersion$Fingerprint,
  Fingerprint = factor(result.fp@AnalysisVersion$Fingerprint)
)
expect_identical(analysisversion$AnalysisVersionID, analysisversion$Fingerprint)

rpackage <- result.fp@RPackage %>%
  as.tbl() %>%
  mutate_(RPackageID = ~as.integer(factor(Fingerprint)))
aj.rpackage <- rpackage %>%
  anti_join(
    result.id@RPackage,
    by = c("Description", "Version", "RPackageID" = "Fingerprint")
  )
expect_identical(nrow(aj.rpackage), 0L)

aj.analysisrpackage <- result.fp@AnalysisVersionRPackage %>%
  as.tbl() %>%
  inner_join(analysisversion, by = "AnalysisVersion") %>%
  select_(AnalysisVersion = ~AnalysisVersionID, ~RPackage) %>%
  inner_join(
    rpackage %>%
      select_(RPackage = ~Fingerprint, ~RPackageID),
    by = c("RPackage")
  ) %>%
  anti_join(
    result.id@AnalysisVersionRPackage,
    by = c("AnalysisVersion", "RPackageID" = "RPackage")
  )
expect_identical(nrow(aj.analysisrpackage), 0L)

aj.analysisparent <- result.fp@AnalysisRelation %>%
  as.tbl() %>%
  inner_join(analysis.id, by = "Analysis") %>%
  select_(~-Analysis) %>%
  mutate_(
    ParentAnalysis = ~factor(ParentAnalysis, levels = analysis.level),
    ParentStatusFingerprint = ~factor(ParentStatusFingerprint),
    ParentStatus = ~factor(ParentStatus)
  ) %>%
  anti_join(
    result.id@AnalysisRelation,
    by = c(
      "AnalysisID" = "Analysis",
      "ParentAnalysis",
      "ParentStatusFingerprint",
      "ParentStatus"
    )
  )
expect_identical(nrow(aj.analysisparent), 0L)

# clean temp files
file.remove(
  list.files(
    temp.dir,
    pattern = "^[0-9a-f]{40}\\.rda$",
    full.names = TRUE
  )
)
