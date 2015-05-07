context("n2kModel accesor functions")

context("status() handles n2kModel objects")
describe("status", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    model.type = "glmer poisson: period + herd",
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
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    model.type = "glmer poisson: period + herd",
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
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    model.type = "glmer poisson: period + herd",
    analysis.date = as.POSIXct("2000-01-01"),
    data = cbpp
  )
  it("returns the Seed slot", {
    expect_that(
      get_seed(object),
      is_identical_to(object@Seed)
    )
  })
})

context("get_scheme_id() handles n2kModel objects")
describe("get_scheme_id", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    model.type = "glmer poisson: period + herd",
    analysis.date = as.POSIXct("2000-01-01"),
    data = cbpp
  )
  it("returns the SchemeID slot", {
    expect_that(
      get_scheme_id(object),
      is_identical_to(object@SchemeID)
    )
  })
})

context("get_species_group_id() handles n2kModel objects")
describe("get_species_group_id", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    model.type = "glmer poisson: period + herd",
    analysis.date = as.POSIXct("2000-01-01"),
    data = cbpp
  )
  it("returns the SpeciesGroupID slot", {
    expect_that(
      get_species_group_id(object),
      is_identical_to(object@SpeciesGroupID)
    )
  })
})

context("get_location_group_id() handles n2kModel objects")
describe("get_location_group_id", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    model.type = "glmer poisson: period + herd",
    analysis.date = as.POSIXct("2000-01-01"),
    data = cbpp
  )
  it("returns the LocationGroupID slot", {
    expect_that(
      get_location_group_id(object),
      is_identical_to(object@LocationGroupID)
    )
  })
})

context("get_data_fingerprint() handles n2kModel objects")
describe("get_data_fingerprint", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    model.type = "glmer poisson: period + herd",
    analysis.date = as.POSIXct("2000-01-01"),
    data = cbpp
  )
  it("returns the DataFingerprint slot", {
    expect_that(
      get_data_fingerprint(object),
      is_identical_to(object@DataFingerprint)
    )
  })
})

context("get_analysis_date() handles n2kModel objects")
describe("get_analysis_date", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    model.type = "glmer poisson: period + herd",
    analysis.date = as.POSIXct("2000-01-01"),
    data = cbpp
  )
  it("returns the AnalysisDate slot", {
    expect_that(
      get_analysis_date(object),
      is_identical_to(object@AnalysisDate)
    )
  })
})

context("get_model_type() handles n2kModel objects")
describe("get_model_type", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    scheme.id = 1,
    species.group.id = 2,
    location.group.id = 3,
    model.type = "glmer poisson: period + herd",
    analysis.date = as.POSIXct("2000-01-01"),
    data = cbpp
  )
  it("returns the ModelType slot", {
    expect_that(
      get_model_type(object),
      is_identical_to(object@ModelType)
    )
  })
})
