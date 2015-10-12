context("import result")
expect_error(import_result("junk", "result must be a 'n2kResult' object"))
describe("import result", {
  it("writes the correct information to the database", {
    skip_on_cran()
    require(dplyr)
    require(RODBC)
    channel <- n2khelper::connect_result()
    schemeid <- n2khelper::odbc_get_id(
      table = "Scheme",
      variable = "Description",
      value = "Unit test",
      channel = channel
    )
    speciesid <- n2khelper::odbc_get_multi_id(
      data = data.frame(
        Description = c("Unit test", "Unit test 2", "Unit test 3"),
        SchemeID = schemeid
      ),
      table = "SpeciesGroup",
      id.field = "ID",
      merge.field = c("Description", "SchemeID"),
      create = TRUE,
      select = TRUE,
      channel = channel
    )$ID
    locationgroupid <- n2khelper::odbc_get_multi_id(
      data = data.frame(
        Description = "Unit test",
        SchemeID = schemeid
      ),
      table = "LocationGroup",
      id.field = "ID",
      merge.field = c("Description", "SchemeID"),
      create = TRUE,
      select = TRUE,
      channel = channel
    )$ID
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

    datasourceid <- n2khelper::odbc_get_id(
      table = "Datasource",
      variable = "Description",
      value = "Unit test",
      channel = channel
    )

    parameter <- data.frame(
      Description = "Unit test",
      Parent = NA,
      stringsAsFactors = FALSE
    ) %>%
      mutate_(
        Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
      )
    parameter <-
    data.frame(
      Description = paste("Unit test", 1:10),
      Parent = parameter$Fingerprint
    ) %>%
      rowwise() %>%
      mutate_(
        Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
      ) %>%
      bind_rows(parameter) %>%
      as.data.frame()
    parameterestimate <- expand.grid(
      Analysis = metadata$FileFingerprint,
      Parameter = parameter$Fingerprint[!is.na(parameter$Parent)],
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
      Description = "Unit test",
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
        Fingerprint = ~get_sha1(
          c(Description = Description, Analysis = Analysis)
        )
      ) %>%
      as.data.frame()
    contrast.coefficient <- expand.grid(
      Contrast = contrast$Fingerprint,
      Parameter = sample(parameter$Fingerprint, min(4, nrow(parameter))),
      stringsAsFactors = FALSE
    ) %>%
      mutate_(Coefficient = ~seq_along(Contrast))
    contrast.estimate <- data.frame(
      Contrast = contrast$Fingerprint,
      stringsAsFactors = FALSE
    ) %>%
      mutate_(
        Estimate = ~rnorm(n()),
        SE = ~runif(n()),
        LowerConfidenceLimit = ~Estimate - SE,
        UpperConfidenceLimit = ~Estimate + SE
      ) %>%
      select_(~-SE)
    expect_is(
      result <- new(
        "n2kResult",
        AnalysisMetadata = metadata,
        AnalysisFormula = lapply(metadata$Formula, as.formula),
        AnalysisVersion = version@AnalysisVersion,
        RPackage = version@RPackage,
        AnalysisVersionRPackage = version@AnalysisVersionRPackage,
        Parameter = parameter,
        ParameterEstimate = parameterestimate
      ),
      "n2kResult"
    )
    expect_true(validObject(result))

    import_result(result = result, result.channel = channel)

    sql <- "
    SELECT
      Scheme.ID AS SchemeID,
      SpeciesGroup.ID AS SpeciesGroupID,
      Scheme.Description AS Scheme,
      SpeciesGroup.Description AS SpeciesGroup
    FROM
      Scheme
    FULL JOIN
      SpeciesGroup
    ON
      Scheme.ID = SpeciesGroup.SchemeID
    WHERE
      SpeciesGroup.Description LIKE 'Unit test%' OR
      Scheme.Description = 'Unit test'
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_identical(nrow(exported), length(speciesid))
    expect_false(anyNA(exported))
    expect_true(all(exported$SchemeID == schemeid))
    expect_true(all(exported$SpeciesGroupID %in% speciesid))

    sql <- "
    SELECT
      Scheme.ID AS SchemeID,
      LocationGroup.ID AS LocationGroupID,
      Scheme.Description AS Scheme,
      LocationGroup.Description AS LocationGroup
    FROM
      Scheme
    FULL JOIN
      LocationGroup
    ON
      Scheme.ID = LocationGroup.SchemeID
    WHERE
      LocationGroup.Description LIKE 'Unit test%' OR
      Scheme.Description = 'Unit test'
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_identical(nrow(exported), length(locationgroupid))
    expect_false(anyNA(exported))
    expect_true(all(exported$SchemeID == schemeid))
    expect_true(all(exported$LocationGroupID %in% locationgroupid))

    sql <- "
    SELECT
      SpeciesGroup.SchemeID,
      Analysis.SpeciesGroupID,
      Analysis.LocationGroupID,
      ModelType.Description AS ModelType,
      ModelSet.FirstYear AS FirstImportedYear,
      ModelSet.LastYear AS LastImportedYear,
      ModelSet.Duration,
      Analysis.LastYear AS LastAnalysedYear,
      Analysis.AnalysisDate,
      Analysis.Seed,
      AnalysisVersion.Description AS AnalysisVersion,
      AnalysisStatus.Description AS Status,
      Analysis.Fingerprint AS FileFingerprint,
      Analysis.StatusFingerprint
    FROM
      (
        (
          (
            Analysis
          INNER JOIN
            SpeciesGroup
          ON
            Analysis.SpeciesGroupID = SpeciesGroup.ID
          )
        INNER JOIN
          AnalysisStatus
        ON
          Analysis.StatusID = AnalysisStatus.ID
        )
      INNER JOIN
        AnalysisVersion
      ON
        Analysis.AnalysisVersionID = AnalysisVersion.ID
      )
    INNER JOIN
      (
        ModelSet
      INNER JOIN
        ModelType
      ON
        ModelSet.ModelTypeID = ModelType.ID
      )
    ON
      Analysis.ModelSetID = ModelSet.ID
    WHERE
      SpeciesGroup.Description LIKE 'Unit test%'
    ORDER BY
      Fingerprint
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_equal(
      exported %>%
        mutate_(AnalysisDate = ~format(AnalysisDate, "%F %T %Z")),
      metadata %>%
        mutate_(AnalysisDate = ~format(AnalysisDate, "%F %T %Z")) %>%
        arrange_(~FileFingerprint) %>%
        select_(.dots = colnames(exported))
    )

    sql <- "
    SELECT
      Analysis.Fingerprint AS Analysis,
      Parameter.Description AS Description,
      Estimate,
      LCL AS LowerConfidenceLimit,
      UCL AS UpperConfidenceLimit
    FROM
      (
        ParameterEstimate
      INNER JOIN
        (
          Analysis
        INNER JOIN
          SpeciesGroup
        ON
          Analysis.SpeciesGroupID = SpeciesGroup.ID
        )
      ON
        ParameterEstimate.AnalysisID = Analysis.ID
      )
    INNER JOIN
      Parameter
    ON
      ParameterEstimate.ParameterID = Parameter.ID
    WHERE
      SpeciesGroup.Description LIKE 'Unit test%'
    ORDER BY
      Analysis.Fingerprint, Parameter.Description
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_equal(
      exported,
      parameterestimate %>%
        inner_join(
          parameter %>%
            select_(~-Parent),
          by = c("Parameter" = "Fingerprint")
        ) %>%
        arrange_(~Analysis, ~Description) %>%
        select_(.dots = colnames(exported))
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
    expect_true(validObject(result))

    # Remove unit test data from database

    sql <- "
    DELETE
      pe
    FROM
      ParameterEstimate pe
    INNER JOIN
      (
        Analysis AS a
      INNER JOIN
        SpeciesGroup as s
      ON
        a.SpeciesGroupID = s.ID
      )
    ON
      pe.AnalysisID = a.ID
    WHERE
      s.Description LIKE 'Unit test%'
    "
    sqlQuery(channel = channel, query = sql)

    sql <- "
    DELETE
      c
    FROM
      Contrast AS c
    INNER JOIN
      (
        Analysis AS a
      INNER JOIN
        SpeciesGroup as s
      ON
        a.SpeciesGroupID = s.ID
      )
    ON
      c.AnalysisID = a.ID
    WHERE
      s.Description LIKE 'Unit test%'
    "
    sqlQuery(channel = channel, query = sql)

    sql <- "
    DELETE
      a
    FROM
      Analysis AS a
    INNER JOIN
      SpeciesGroup AS s
    ON
      a.SpeciesGroupID = s.ID
    WHERE
      s.Description LIKE 'Unit test%'
    "
    sqlQuery(channel = channel, query = sql)

    odbcClose(channel)
  })
})
