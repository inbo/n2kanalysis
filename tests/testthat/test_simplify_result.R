context("simplify results")
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
  apply(1, get_sha1)
metadata$StatusFingerprint <- metadata %>%
  select_(~FileFingerprint, ~Status) %>%
  apply(1, get_sha1)

datasourceid <- 10L

parameter <- data.frame(
  Description = c("Unit test", "Unit test letters"),
  Parent = NA,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate_(
    Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
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
    Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
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
    Fingerprint = ~get_sha1(c(Description = Description))
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

contrast <- expand.grid(
  Description = paste("Unit test", 1:3),
  Analysis = metadata$FileFingerprint,
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate_(
    Fingerprint = ~get_sha1(c(Description = Description, Analysis = Analysis))
  ) %>%
  as.data.frame()
contrast.coefficient <- expand.grid(
  Contrast = contrast$Fingerprint,
  Parameter = sample(parameter$Fingerprint, min(4, nrow(parameter))),
  stringsAsFactors = FALSE
) %>%
  mutate_(Coefficient = ~seq_along(Contrast))
contrast.estimate <- contrast %>%
  select_(Contrast = ~Fingerprint) %>%
  mutate_(
    Estimate = ~seq_along(Contrast),
    LowerConfidenceLimit = ~-Estimate,
    UpperConfidenceLimit = ~2 * Estimate
  )













expect_is(
  result <- new(
    "n2kResult",
    AnalysisMetadata = metadata,
    AnalysisFormula = lapply(metadata$Formula, as.formula),
    AnalysisVersion = version@AnalysisVersion,
    RPackage = version@RPackage,
    AnalysisVersionRPackage = version@AnalysisVersionRPackage,
    Parameter = parameter,
    ParameterEstimate = parameterestimate,
    AnomalyType = anomalytype,
    Anomaly = anomaly,
    Contrast = contrast,
    ContrastCoefficient = contrast.coefficient,
    ContrastEstimate = contrast.estimate
  ),
  "n2kResult"
)
expect_error(
  simplify_result("junk"),
  "result does not inherit from class n2kResult"
)
expect_is(result.simple <- simplify_result(result = result), "n2kResult")
metadata.simple <- result@AnalysisMetadata %>%
  full_join(
    result.simple@AnalysisMetadata,
    by = c(
      "SchemeID", "SpeciesGroupID", "LocationGroupID", "FirstImportedYear",
      "LastImportedYear", "Duration", "LastAnalysedYear", "AnalysisDate",
      "Seed", "StatusFingerprint"
    )
  ) %>%
  select_(
    ~-SchemeID, ~- SpeciesGroupID, ~-LocationGroupID, ~-FirstImportedYear,
    ~-LastImportedYear, ~-Duration, ~-LastAnalysedYear, ~-AnalysisDate, ~-Seed,
    ~-StatusFingerprint
  )
expect_identical(
  factor(metadata.simple$ModelType.x),
  metadata.simple$ModelType.y
)
expect_identical(
  factor(metadata.simple$Formula.x),
  metadata.simple$Formula.y
)
expect_identical(
  factor(metadata.simple$AnalysisVersion.x),
  metadata.simple$AnalysisVersion.y
)
expect_identical(
  factor(metadata.simple$Status.x),
  metadata.simple$Status.y
)
expect_identical(
  factor(metadata.simple$FileFingerprint.x),
  metadata.simple$FileFingerprint.y
)
expect_identical(
  result@AnalysisFormula,
  result.simple@AnalysisFormula
)
expect_identical(
  factor(result@AnalysisVersion$Fingerprint),
  result.simple@AnalysisVersion$Fingerprint
)
rpackage.simple <- result@RPackage %>%
  full_join(result.simple@RPackage, by = c("Description", "Version")) %>%
  select_(~-Description, ~-Version)
expect_true(assertthat::noNA(rpackage.simple))
expect_identical(
  anyDuplicated(rpackage.simple$Fingerprint.x),
  0L
)
expect_identical(
  anyDuplicated(rpackage.simple$Fingerprint.y),
  0L
)

expect_identical(
  result@AnalysisVersionRPackage %>%
    anti_join(rpackage.simple, c("RPackage" = "Fingerprint.x")) %>%
    nrow(),
  0L
)
expect_identical(
  result.simple@AnalysisVersionRPackage %>%
    anti_join(rpackage.simple, c("RPackage" = "Fingerprint.y")) %>%
    nrow(),
  0L
)
expect_identical(
  result@AnalysisVersionRPackage %>%
    inner_join(rpackage.simple, c("RPackage" = "Fingerprint.x")) %>%
    anti_join(
      result.simple@AnalysisVersionRPackage,
      by = c("Fingerprint.y" = "RPackage")
    ) %>% nrow(),
  0L
)
avrp.simple <- result@AnalysisVersionRPackage %>%
    full_join(rpackage.simple, c("RPackage" = "Fingerprint.x")) %>%
    full_join(
      result.simple@AnalysisVersionRPackage,
      by = c("Fingerprint.y" = "RPackage")
    ) %>%
    select_(~starts_with("AnalysisVersion")) %>%
    distinct()
expect_true(assertthat::noNA(avrp.simple))
expect_identical(
  avrp.simple$AnalysisVersion.x,
  as.character(avrp.simple$AnalysisVersion.y)
)

expect_is(result.simple@Parameter$Fingerprint, "integer")
expect_is(result.simple@Parameter$Parent, "integer")
expect_true(assertthat::noNA(result.simple@Parameter$Fingerprint))
expect_identical(anyDuplicated(result.simple@Parameter$Fingerprint), 0L)

parameter.simple <- result@Parameter %>%
  inner_join(result@Parameter, by = c("Parent" = "Fingerprint")) %>%
  select_(Description = ~Description.x, ParentDescription = ~Description.y) %>%
  full_join(
    result.simple@Parameter %>%
      inner_join(result.simple@Parameter, by = c("Parent" = "Fingerprint")) %>%
      select_(Description = ~Description.x, ParentDescription = ~Description.y),
    by = c("Description", "ParentDescription")
  )
expect_true(assertthat::noNA(parameter.simple))
expect_identical(anyDuplicated(parameter.simple), 0L)

parameter.simple <- result@Parameter %>%
  inner_join(result.simple@Parameter, by = "Description") %>%
  select_(~Fingerprint.x, ~Fingerprint.y)
expect_true(assertthat::noNA(parameter.simple))
expect_identical(anyDuplicated(parameter.simple), 0L)

parameterestimate.simple <- result@ParameterEstimate %>%
  full_join(
    result.simple@ParameterEstimate,
    by = c("Estimate", "LowerConfidenceLimit", "UpperConfidenceLimit")
  ) %>%
  select_(~-Estimate, ~-LowerConfidenceLimit, ~-UpperConfidenceLimit)
expect_true(assertthat::noNA(parameterestimate.simple))
expect_identical(
  parameterestimate.simple %>%
    select_(~ends_with(".x")) %>%
    anyDuplicated(),
  0L
)
expect_identical(
  parameterestimate.simple %>%
    select_(~ends_with(".y")) %>%
    anyDuplicated(),
  0L
)
expect_identical(
  parameterestimate.simple$Analysis.x,
  as.character(parameterestimate.simple$Analysis.y)
)
parameterestimate.simple <- parameter.simple %>%
  select_(~Fingerprint.x, ~Fingerprint.y) %>%
  full_join(
    parameterestimate.simple,
    by = c("Fingerprint.x" = "Parameter.x")
  ) %>%
  select_(~Fingerprint.y, ~Parameter.y)
expect_true(assertthat::noNA(parameterestimate.simple))
expect_identical(
  parameterestimate.simple$Fingerprint.y,
  parameterestimate.simple$Parameter.y
)

anomalytype.simple <- result@AnomalyType %>%
  full_join(result.simple@AnomalyType, by = "Description") %>%
  select_(~-Description)
expect_true(assertthat::noNA(anomalytype.simple))
expect_identical(anyDuplicated(anomalytype.simple$Fingerprint.x), 0L)
expect_identical(anyDuplicated(anomalytype.simple$Fingerprint.y), 0L)

anomaly.simple <- result@Anomaly %>%
  mutate_(Datafield = ~factor(Datafield)) %>%
  full_join(
    result.simple@Anomaly,
    by = c("DatasourceID", "Datafield", "Estimate")
  ) %>%
  select_(~-DatasourceID, ~-Datafield, ~-Estimate)
expect_true(assertthat::noNA(anomaly.simple))
expect_identical(
  anomaly.simple %>%
    select(ends_with(".x")) %>%
    anyDuplicated(),
  0L
)
expect_identical(
  anomaly.simple %>%
    select(ends_with(".y")) %>%
    anyDuplicated(),
  0L
)

contrast.simple <- result@Contrast %>%
  full_join(result.simple@Contrast, by = c("Description", "Analysis")) %>%
  select(ends_with(".x"), ends_with(".y"))
expect_true(assertthat::noNA(contrast.simple))
expect_identical(
  contrast.simple %>%
    select(ends_with(".x")) %>%
    anyDuplicated(),
  0L
)
expect_identical(
  contrast.simple %>%
    select(ends_with(".y")) %>%
    anyDuplicated(),
  0L
)

contrastcoefficient.simple <- result@ContrastCoefficient %>%
  full_join(result.simple@ContrastCoefficient, by = "Coefficient") %>%
  select(ends_with(".x"), ends_with(".y"))
expect_true(assertthat::noNA(contrastcoefficient.simple))
expect_identical(
  contrastcoefficient.simple %>%
    select(ends_with(".x")) %>%
    anyDuplicated(),
  0L
)
expect_identical(
  contrastcoefficient.simple %>%
    select(ends_with(".y")) %>%
    anyDuplicated(),
  0L
)

contrastestimate.simple <- result@ContrastEstimate %>%
  full_join(
    result.simple@ContrastEstimate,
    by = c("Estimate", "LowerConfidenceLimit", "UpperConfidenceLimit")
  ) %>%
  select(ends_with(".x"), ends_with(".y"))
expect_true(assertthat::noNA(contrastestimate.simple))
expect_identical(
  contrastestimate.simple %>%
    select(ends_with(".x")) %>%
    anyDuplicated(),
  0L
)
expect_identical(
  contrastestimate.simple %>%
    select(ends_with(".y")) %>%
    anyDuplicated(),
  0L
)
