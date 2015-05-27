context("n2kModel accesor functions")
data("cbpp", package = "lme4")
cbpp$Count <- cbpp$incidence
object <- n2k_glmer_poisson(
  scheme.id = 1,
  species.group.id = 2,
  location.group.id = 3,
  model.type = "glmer poisson: period + herd",
  covariate = "offset(log(size)) + period + (1|herd)",
  first.imported.year = 1990,
  last.imported.year = 2015,
  analysis.date = as.POSIXct("2000-01-01"),
  data = cbpp
)
model.object <- lme4::glmer(
  incidence ~ offset(log(size)) + period + (1 | herd), 
  data = object@Data,
  family = poisson
)
object.model <- n2k_glmer_poisson(
  data = object, model.fit = model.object, status = "converged"
)

context("status() handles n2kModel objects")
describe("status", {
  it("returns the status", {
    expect_that(
      status(object),
      is_identical_to(object@Status)
    )
    expect_that(
      status(object.model),
      is_identical_to(object.model@Status)
    )
  })
  it("updates the status", {
    status(object) <- "error"
    expect_that(
      status(object),
      is_identical_to("error")
    )
    expect_that(
      status(object) <- "junk",
      throws_error("Status must be one of the following")
    )
  })
})

context("get_data() handles n2kModel objects")
describe("get_data", {
  it("returns the Data slot", {
    expect_that(
      get_data(object),
      is_identical_to(cbpp)
    )
    expect_that(
      get_data(object.model),
      is_identical_to(cbpp)
    )
  })
})

context("get_seed() handles n2kModel objects")
describe("get_seed", {
  it("returns the Seed slot", {
    expect_that(
      get_seed(object),
      is_identical_to(object@Seed)
    )
  })
})

context("get_scheme_id() handles n2kModel objects")
describe("get_scheme_id", {
  it("returns the SchemeID slot", {
    expect_that(
      get_scheme_id(object),
      is_identical_to(object@SchemeID)
    )
  })
})

context("get_species_group_id() handles n2kModel objects")
describe("get_species_group_id", {
  it("returns the SpeciesGroupID slot", {
    expect_that(
      get_species_group_id(object),
      is_identical_to(object@SpeciesGroupID)
    )
  })
})

context("get_location_group_id() handles n2kModel objects")
describe("get_location_group_id", {
  it("returns the LocationGroupID slot", {
    expect_that(
      get_location_group_id(object),
      is_identical_to(object@LocationGroupID)
    )
  })
})

context("get_analysis_date() handles n2kModel objects")
describe("get_analysis_date", {
  it("returns the AnalysisDate slot", {
    expect_that(
      get_analysis_date(object),
      is_identical_to(object@AnalysisDate)
    )
  })
})

context("get_model_type() handles n2kModel objects")
describe("get_model_type", {
  it("returns the ModelType slot", {
    expect_that(
      get_model_type(object),
      is_identical_to(object@ModelType)
    )
  })
})

context("get_covariate() handles n2kModel objects")
describe("get_covariate", {
  it("returns the Covariate slot", {
    expect_that(
      get_covariate(object),
      is_identical_to(object@Covariate)
    )
  })
})

context("get_model_set() handles n2kModel objects")
describe("get_model_set", {
  it("returns the model set data.frame", {
    expect_that(
      get_model_set(object),
      is_identical_to(
        data.frame(
          ModelType = object@ModelType,
          FirstImportedYear = object@FirstImportedYear,
          LastImportedYear = object@LastImportedYear,
          Duration = object@Duration,
          LastAnalysedYear = object@LastAnalysedYear,
          Seed = object@Seed,
          stringsAsFactors = FALSE
        )
      )
    )
  })
})

context("get_session_info() handles n2kModel objects")
describe("get_session_info()", {
  it("returns the SessionInfo slot", {
    expect_that(
      get_session_info(object),
      is_identical_to(object@SessionInfo)
    )
  })
})

context("get_file_fingerprint() handles n2kModel objects")
describe("get_file_fingerprint()", {
  it("returns the FileFingerprint slot", {
    expect_that(
      get_file_fingerprint(object),
      is_identical_to(object@FileFingerprint)
    )
  })
})

context("get_status_fingerprint() handles n2kModel objects")
describe("get_status_fingerprint()", {
  it("returns the StatusFingerprint slot", {
    expect_that(
      get_status_fingerprint(object),
      is_identical_to(object@StatusFingerprint)
    )
  })
})
