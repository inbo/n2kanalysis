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
        SchemeID = schemeid,
        stringsAsFactors = FALSE
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
        SchemeID = schemeid,
        stringsAsFactors = FALSE
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
    relation <- metadata %>%
      slice_(1) %>%
      select_(
        ParentAnalysis = ~FileFingerprint,
        ParentStatusFingerprint = ~StatusFingerprint,
        ParentStatus = ~Status
      ) %>%
      bind_cols(
        metadata %>%
          slice_(2) %>%
          select_(Analysis = ~FileFingerprint)
      ) %>%
      as.data.frame()

    datasourceid <- n2khelper::odbc_get_id(
      table = "Datasource",
      variable = "Description",
      value = "Unit test",
      channel = channel
    )
    datafield.type <- n2khelper::odbc_get_multi_id(
      data = data.frame(
        Description = "Unit test",
        stringsAsFactors = FALSE
      ),
      id.field = "ID",
      merge.field = "Description",
      table = "DatafieldType",
      channel = channel,
      create = TRUE,
      select = TRUE
    )
    datafield <- data.frame(
      DatasourceID = datasourceid,
      TableName = "UnitTest",
      PrimaryKey = "UnitTestID",
      TypeID = datafield.type$ID,
      stringsAsFactors = FALSE
    )
    n2khelper::odbc_get_multi_id(
      data = datafield,
      id.field = "ID",
      merge.field = c("DatasourceID", "TypeID"),
      table = "Datafield",
      channel = channel,
      create = TRUE,
      select = FALSE
    )
    parameter <- data.frame(
      Description = "Unit test",
      Parent = NA,
      stringsAsFactors = FALSE
    ) %>%
      mutate_(
        Fingerprint = ~get_sha1(c(Description = Description, Parent = Parent))
      )
    parameter <- data.frame(
      Description = paste("Unit test", 1:10),
      Parent = parameter$Fingerprint,
      stringsAsFactors = FALSE
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
      Parameter = sample(
        x = unique(parameterestimate$Parameter),
        size = min(5, length(unique(unique(parameterestimate$Parameter)))),
        replace = FALSE
      ),
      DatasourceID = datasourceid,
      Datafield = "Unit test",
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
        Estimate = ~seq_len(n()),
        LowerConfidenceLimit = ~-seq_len(n()),
        UpperConfidenceLimit = ~2 * seq_len(n())
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
      WITH
        cteParameter AS (
          SELECT
            ID,
            Description,
            ParentParameterID
          FROM
            Parameter
          WHERE
            Description = 'Unit test'
          UNION ALL
          SELECT
            Parameter.ID,
            Parameter.Description,
            Parameter.ParentParameterID
          FROM
            Parameter
          INNER JOIN
            cteParameter
          ON
            Parameter.ParentParameterID = cteParameter.ID
        )

      SELECT
        ID,
        Description,
        ParentParameterID
      FROM
        cteParameter
      ORDER BY
        Description
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_equal(
      exported %>%
        select_(~Description),
      parameter %>%
        arrange_(~Description) %>%
        select_(~Description)
    )
    parameter.simple <- exported %>%
      inner_join(parameter, by = "Description") %>%
      select_(~ID, ~Fingerprint)
    expect_true(assertthat::noNA(parameter.simple))
    expect_identical(anyDuplicated(parameter.simple), 0L)

    sql <- "
      SELECT
        P1.Description AS Main,
        P2.Description AS Sub
      FROM
        Parameter AS P1
      INNER JOIN
        Parameter AS P2
      ON
        P1.ID = P2.ParentParameterID
      WHERE
        P1.Description = 'Unit test'
      ORDER BY
        P2.Description
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_equal(
      exported,
      parameter %>%
        filter(is.na(Parent)) %>%
        select_(Main = ~Description, ~Fingerprint) %>%
        inner_join(
          parameter %>% select_(Sub = ~Description, ~Parent),
          by = c("Fingerprint" = "Parent")
        ) %>%
        select_(~-Fingerprint) %>%
        arrange_(~Sub)
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
        Anomaly = anomaly
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
      WITH
        cteParameter AS (
          SELECT
            ID,
            Description,
            ParentParameterID
          FROM
            Parameter
          WHERE
            Description = 'Unit test'
          UNION ALL
          SELECT
            Parameter.ID,
            Parameter.Description,
            Parameter.ParentParameterID
          FROM
            Parameter
          INNER JOIN
            cteParameter
          ON
            Parameter.ParentParameterID = cteParameter.ID
        )

      SELECT
        ID,
        Description,
        ParentParameterID
      FROM
        cteParameter
      ORDER BY
        Description
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_equal(
      exported %>%
        select_(~Description),
      parameter %>%
        arrange_(~Description) %>%
        select_(~Description)
    )
    parameter.simple <- exported %>%
      inner_join(parameter, by = "Description") %>%
      select_(~ID, ~Fingerprint)
    expect_true(assertthat::noNA(parameter.simple))
    expect_identical(anyDuplicated(parameter.simple), 0L)

    sql <- "
      SELECT
        P1.Description AS Main,
        P2.Description AS Sub
      FROM
        Parameter AS P1
      INNER JOIN
        Parameter AS P2
      ON
        P1.ID = P2.ParentParameterID
      WHERE
        P1.Description = 'Unit test'
      ORDER BY
        P2.Description
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_equal(
      exported,
      parameter %>%
        filter(is.na(Parent)) %>%
        select_(Main = ~Description, ~Fingerprint) %>%
        inner_join(
          parameter %>% select_(Sub = ~Description, ~Parent),
          by = c("Fingerprint" = "Parent")
        ) %>%
        select_(~-Fingerprint) %>%
        arrange_(~Sub)
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
        AnalysisRelation = relation,
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
      WITH
        cteParameter AS (
          SELECT
            ID,
            Description,
            ParentParameterID
          FROM
            Parameter
          WHERE
            Description = 'Unit test'
          UNION ALL
          SELECT
            Parameter.ID,
            Parameter.Description,
            Parameter.ParentParameterID
          FROM
            Parameter
          INNER JOIN
            cteParameter
          ON
            Parameter.ParentParameterID = cteParameter.ID
        )

      SELECT
        ID,
        Description,
        ParentParameterID
      FROM
        cteParameter
      ORDER BY
        Description
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_equal(
      exported %>%
        select_(~Description),
      parameter %>%
        arrange_(~Description) %>%
        select_(~Description)
    )
    parameter.simple <- exported %>%
      inner_join(parameter, by = "Description") %>%
      select_(~ID, ~Fingerprint)
    expect_true(assertthat::noNA(parameter.simple))
    expect_identical(anyDuplicated(parameter.simple), 0L)

    sql <- "
      SELECT
        P1.Description AS Main,
        P2.Description AS Sub
      FROM
        Parameter AS P1
      INNER JOIN
        Parameter AS P2
      ON
        P1.ID = P2.ParentParameterID
      WHERE
        P1.Description = 'Unit test'
      ORDER BY
        P2.Description
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_equal(
      exported,
      parameter %>%
        filter(is.na(Parent)) %>%
        select_(Main = ~Description, ~Fingerprint) %>%
        inner_join(
          parameter %>% select_(Sub = ~Description, ~Parent),
          by = c("Fingerprint" = "Parent")
        ) %>%
        select_(~-Fingerprint) %>%
        arrange_(~Sub)
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

    sql <- "
    SELECT
      ID,
      Description
    FROM
      AnomalyType
    WHERE
      Description LIKE 'Unit test%'
    ORDER BY
      Description
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_true(
      anomalytype %>%
        full_join(exported, by = "Description") %>%
        noNA()
    )

    sql <- "
    SELECT
      Anomaly.ID,
      Analysis.Fingerprint AS Analysis,
      AnomalyType.Description AS AnomalyType,
      Anomaly.AnalysisID AS AnomalyAnalysis,
      ParameterEstimate.AnalysisID,
      Parameter.Description,
      Datafield.DatasourceID,
      TableName,
      PrimaryKey,
      Datafield.TypeID
    FROM
      (
        (
          (
            (
              AnomalyType
            INNER JOIN
              Anomaly
            ON
              AnomalyType.ID = Anomaly.TypeID
            )
          INNER JOIN
            ParameterEstimate
          ON
            Anomaly.ParameterEstimateID = ParameterEstimate.ID
          )
        INNER JOIN
          Parameter
        ON
          ParameterEstimate.ParameterID = Parameter.ID
        )
      INNER JOIN
        Datafield
      ON
        Anomaly.DatafieldID = Datafield.ID
      )
    INNER JOIN
      Analysis
    ON
      Anomaly.AnalysisID = Analysis.ID
    WHERE
      AnomalyType.Description LIKE 'Unit test%'
    ORDER BY
      Description
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_identical(exported$AnomalyAnalysis, exported$AnalysisID)
    expect_true(
      datafield %>%
        inner_join(datafield.type, by = c("TypeID" = "ID")) %>%
        inner_join(
          anomaly,
          by = c("Description" = "Datafield", "DatasourceID")
        ) %>%
        select_(~-Description) %>%
        inner_join(anomalytype, by = c("AnomalyType" = "Fingerprint")) %>%
        select_(~-AnomalyType, AnomalyType = ~Description) %>%
        inner_join(
          parameter %>%
            select_(~-Parent),
          by = c("Parameter" = "Fingerprint")
        ) %>%
        select_(~-Parameter) %>%
        full_join(
          exported,
          by = c(
            "DatasourceID", "TableName", "PrimaryKey", "Analysis",
            "AnomalyType", "Description", "TypeID"
          )
        ) %>%
        noNA()
    )

    sql <- "
    SELECT
      Contrast.ID,
      Contrast.Description,
      Fingerprint AS Analysis
    FROM
      (
        Contrast
      INNER JOIN
        Analysis
      ON
        Contrast.AnalysisID = Analysis.ID
      )
    INNER JOIN
      SpeciesGroup
    ON
      Analysis.SpeciesGroupID = SpeciesGroup.ID
    WHERE
      SpeciesGroup.Description LIKE 'Unit test%'
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_true(
      contrast %>%
        full_join(exported, by = c("Description", "Analysis")) %>%
        noNA()
    )

    sql <- "
    SELECT
      ContrastCoefficient.ID,
      Contrast.Description AS Contrast,
      Parameter.Description AS Parameter,
      Constant AS Coefficient
    FROM
    (
      (
        (
          Contrast
        INNER JOIN
          Analysis
        ON
          Contrast.AnalysisID = Analysis.ID
        )
      INNER JOIN
        SpeciesGroup
      ON
        Analysis.SpeciesGroupID = SpeciesGroup.ID
      )
    INNER JOIN
      ContrastCoefficient
    ON
      Contrast.ID = ContrastCoefficient.ContrastID
    )
    INNER JOIN
      Parameter
    ON
      ContrastCoefficient.ParameterID = Parameter.ID
    WHERE
      SpeciesGroup.Description LIKE 'Unit test%'
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_true(
      contrast.coefficient %>%
        inner_join(contrast, by = c("Contrast" = "Fingerprint")) %>%
        select_(~-Analysis, ~-Contrast, Contrast = ~Description) %>%
        inner_join(parameter, by = c("Parameter" = "Fingerprint")) %>%
        select_(~-Parent, A = ~Parameter, Parameter = ~Description) %>%
        full_join(exported, by = c("Contrast", "Parameter", "Coefficient")) %>%
        noNA
    )

    sql <- "
    SELECT
      ContrastEstimate.ID,
      Contrast.Description AS Contrast,
      Estimate,
      LCL,
      UCL
    FROM
    (
      (
        Contrast
      INNER JOIN
        Analysis
      ON
        Contrast.AnalysisID = Analysis.ID
      )
    INNER JOIN
      SpeciesGroup
    ON
      Analysis.SpeciesGroupID = SpeciesGroup.ID
    )
    INNER JOIN
      ContrastEstimate
    ON
      Contrast.ID = ContrastEstimate.ContrastID
    WHERE
      SpeciesGroup.Description LIKE 'Unit test%'
    "
    exported <- sqlQuery(
      channel = channel,
      query = sql,
      stringsAsFactors = FALSE
    )
    expect_true(
      contrast.estimate %>%
        inner_join(contrast, by = c("Contrast" = "Fingerprint")) %>%
        select_(~-Contrast, Contrast = ~Description) %>%
        full_join(
          exported,
          by = c(
            "Contrast",
            "Estimate",
            "LowerConfidenceLimit" = "LCL",
            "UpperConfidenceLimit" = "UCL"
          )
        ) %>%
        noNA()
    )

    # Remove unit test data from database
    sql <- "
    DELETE
      an
    FROM
      Anomaly AS an
    INNER JOIN
      (
        ParameterEstimate AS pe
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
      )
    ON
      an.ParameterEstimateID = pe.ID
    WHERE
      s.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      pe
    FROM
      ParameterEstimate AS pe
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
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      cc
    FROM
      ContrastCoefficient AS cc
    INNER JOIN
      (
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
      )
    ON
      cc.ContrastID = c.ID
    WHERE
      s.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      p
    FROM
      Parameter AS p
    WHERE
      p.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      ce
    FROM
      ContrastEstimate AS ce
    INNER JOIN
      (
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
      )
    ON
      ce.ContrastID = c.ID
    WHERE
      s.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

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
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      ar
    FROM
      AnalysisRelation AS ar
    INNER JOIN
      (
        Analysis AS a
      INNER JOIN
        SpeciesGroup AS s
      ON
        a.SpeciesGroupID = s.ID
      )
    ON
      ar.AnalysisID = a.ID OR ar.SourceAnalysisID = a.ID
    WHERE
      s.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

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
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      s
    FROM
      SpeciesGroup AS s
    WHERE
      s.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      l
    FROM
      LocationGroup AS l
    WHERE
      l.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      ms
    FROM
      ModelSet AS ms
    INNER JOIN
      ModelType AS mt
    ON
      ms.ModelTypeID = mt.ID
    WHERE
      mt.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      mt
    FROM
      ModelType AS mt
    WHERE
      mt.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      at
    FROM
      AnomalyType AS at
    WHERE
      at.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      df
    FROM
      Datafield AS df
    INNER JOIN
      DatafieldType as dt
    ON
      df.TypeID = dt.ID
    WHERE
      dt.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    sql <- "
    DELETE
      dt
    FROM
      DatafieldType as dt
    WHERE
      dt.Description LIKE 'Unit test%'
    "
    expect_equal(sqlQuery(channel = channel, query = sql), character(0))

    odbcClose(channel)
  })
})
