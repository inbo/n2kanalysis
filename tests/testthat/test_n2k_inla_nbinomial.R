context("prepare a n2kGlmerPoisson object")
require(INLA)
this.scheme.id <- 1L
this.species.group.id <- 2L
this.location.group.id <- 3L
this.analysis.date <- Sys.time()
this.model.type <- "inla nbinomial: period + herd"
this.covariate <- "offset(log(size)) + period + f(herd, model = 'iid')"  
this.first.imported.year <- 1990L
data("cbpp", package = "lme4")
cbpp$Count <- cbpp$incidence
object <- n2k_inla_nbinomial(
  scheme.id = this.scheme.id,
  species.group.id = this.species.group.id,
  location.group.id = this.location.group.id,
  model.type = this.model.type,
  covariate = this.covariate,
  first.imported.year = this.first.imported.year,
  analysis.date = this.analysis.date,
  data = cbpp 
)
model.object <- INLA::inla(
  Count ~ offset(log(size)) + period + f(herd, model = "iid"), 
  data = object@Data,
  family = "nbinomial"
)
model.truth <- INLA::inla(
  Count ~ offset(log(size)) + period + f(herd, model = "iid"), 
  data = cbpp,
  family = "nbinomial"
)
describe("n2k_inla_nbinomial", {
  
  it("adds the data as a data.frame", {
    expect_that(
      object@Data,
      is_identical_to(cbpp)
    )
    expect_that(
      model.object$summary.fixed,
      is_identical_to(model.truth$summary.fixed)
    )
    expect_that(
      model.object$summary.random,
      is_identical_to(model.truth$summary.random)
    )
  })
  it("uses 'new' as default status", {
    expect_that(
      object@Status,
      is_identical_to("new")
    )
  })
  it("requires a correct status", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        scheme.id = this.scheme.id,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        status = "junk"
      ),
      throws_error("Status must be one of the following")
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        scheme.id = this.scheme.id,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        status = NA
      ),
      throws_error("Status must be character")
    )
  })
  it("checks the model type", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        scheme.id = this.scheme.id,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = "junk",
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date
      ),
      throws_error("ModelType should be 'inla nbinomial'")
    )
  })
  it("sets the correct seed", {
    this.seed <- 12345L
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        scheme.id = this.scheme.id,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        seed = this.seed
      )@Seed,
      is_identical_to(this.seed)
    )
  })
  it("converts numeric seed, when possible", {
    this.seed <- 12345
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        scheme.id = this.scheme.id,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        seed = this.seed
      )@Seed,
      is_identical_to(as.integer(this.seed))
    )
    this.seed <- 12345L
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        scheme.id = this.scheme.id,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        seed = this.seed + 1e-11
      )@Seed,
      is_identical_to(this.seed)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        scheme.id = this.scheme.id,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        seed = this.seed + 0.1
      ),
      throws_error("seed is not integer")
    )
  })
  it("sets a random seed when not provided", {
    expect_that(
      object@Seed,
      is_a("integer")
    )
  })
  
  it("sets the correct SchemeID", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      )@SchemeID,
      is_identical_to(this.scheme.id)
    )
  })
  it("converts numeric scheme.id, when possible", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = as.numeric(this.scheme.id)
      )@SchemeID,
      is_identical_to(this.scheme.id)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id + 1e-11
      )@SchemeID,
      is_identical_to(this.scheme.id)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id + 0.1
      ),
      throws_error("scheme.id is not integer")
    )
  })
  
  it("sets the correct SpeciesGroupID", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        covariate = this.covariate,
        scheme.id = this.scheme.id
      )@SpeciesGroupID,
      is_identical_to(this.species.group.id)
    )
  })
  it("converts numeric species.group.id, when possible", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = as.numeric(this.species.group.id),
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      )@SpeciesGroupID,
      is_identical_to(this.species.group.id)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id + 1e-11,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      )@SpeciesGroupID,
      is_identical_to(this.species.group.id)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id + 0.1,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      ),
      throws_error("species.group.id is not integer")
    )
  })
  
  it("sets the correct LocationGroupID", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      )@LocationGroupID,
      is_identical_to(this.location.group.id)
    )
  })
  it("converts numeric location.group.id, when possible", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = as.numeric(this.location.group.id),
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      )@LocationGroupID,
      is_identical_to(this.location.group.id)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id + 1e-11,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      )@LocationGroupID,
      is_identical_to(this.location.group.id)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id + 0.1,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      ),
      throws_error("location.group.id is not integer")
    )
  })
  
  it("sets the correct FirstImportedYear", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      )@FirstImportedYear,
      is_identical_to(this.first.imported.year)
    )
  })
  it("converts numeric location.group.id, when possible", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = as.numeric(this.first.imported.year),
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      )@FirstImportedYear,
      is_identical_to(this.first.imported.year)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year + 1e-11,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      )@FirstImportedYear,
      is_identical_to(this.first.imported.year)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year + 0.1,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      ),
      throws_error("first.imported.year is not integer")
    )
  })
  it("checks that FirstImportedYear is from the past", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = as.integer(format(Sys.time(), "%Y")) + 1,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      ),
      throws_error("Importing data from the future?")
    )
  })
  
  it("checks if analysis date is from the past", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp,
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = Sys.time() + 24 * 60 * 60,
        scheme.id = this.scheme.id
      ),
      throws_error("analysis.date is in the future")
    )
  })  
  it("checks if all variable in covariate are available in the data", {
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp[, c("herd", "period", "size")],
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      ),
      throws_error("Variables missing in df: Count")
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp[, c("herd", "period", "Count")],
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      ),
      throws_error("Variables missing in df: size")
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp[, c("herd", "size", "Count")],
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      ),
      throws_error("Variables missing in df: period")
    )
    expect_that(
      n2k_inla_nbinomial(
        data = cbpp[, c("size", "period", "Count")],
        species.group.id = this.species.group.id,
        location.group.id = this.location.group.id,
        model.type = this.model.type,
        covariate = this.covariate,
        first.imported.year = this.first.imported.year,
        analysis.date = this.analysis.date,
        scheme.id = this.scheme.id
      ),
      throws_error("Variables missing in df: herd")
    )
  })  
})












context("add a model to a n2kInlaNbinomial object")
describe("n2k_inla_nbinomial", {
  object.model <- n2k_inla_nbinomial(
    data = object, model.fit = model.object, status = "converged"
  )
  it("keeps the objects", {
    expect_that(
      object.model@Data,
      is_identical_to(cbpp)
    )
    expect_that(
      object.model@Model$summary.fixed,
      is_identical_to(model.truth$summary.fixed)
    )
    expect_that(
      object.model@Model$summary.random,
      is_identical_to(model.truth$summary.random)
    )
    expect_that(
      object.model@Model,
      is_identical_to(model.object)
    )
    expect_that(
      object.model@Seed,
      is_identical_to(object@Seed)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = object, model.fit = model.object, status = "converged", seed = 1
      )@Seed,
      is_identical_to(object@Seed)
    )
    expect_that(
      object.model@SchemeID,
      is_identical_to(object@SchemeID)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = object, model.fit = model.object, status = "converged", scheme.id = 999
      )@SchemeID,
      is_identical_to(object@SchemeID)
    )
    expect_that(
      object.model@SpeciesGroupID,
      is_identical_to(object@SpeciesGroupID)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = object, model.fit = model.object, status = "converged", species.group.id = 999
      )@SpeciesGroupID,
      is_identical_to(object@SpeciesGroupID)
    )
    expect_that(
      object.model@LocationGroupID,
      is_identical_to(object@LocationGroupID)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = object, model.fit = model.object, status = "converged", location.group.id = 999
      )@LocationGroupID,
      is_identical_to(object@LocationGroupID)
    )
    expect_that(
      object.model@ModelType,
      is_identical_to(object@ModelType)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = object, model.fit = model.object, status = "converged", model.type = 999
      )@ModelType,
      is_identical_to(object@ModelType)
    )
    expect_that(
      object.model@Covariate,
      is_identical_to(object@Covariate)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = object, model.fit = model.object, status = "converged", covariate = 999
      )@Covariate,
      is_identical_to(object@Covariate)
    )
    expect_that(
      object.model@AnalysisDate,
      is_identical_to(object@AnalysisDate)
    )
    expect_that(
      n2k_inla_nbinomial(
        data = object, model.fit = model.object, status = "converged", analysis.data = 999
      )@AnalysisDate,
      is_identical_to(object@AnalysisDate)
    )
  })
  it("stores the new status", {
    expect_that(
      object.model@Status,
      is_identical_to("converged")
    )
    expect_that(
      n2k_inla_nbinomial(
        data = object, model.fit = model.object, status = "junk"
      ),
      throws_error("Status must be one of the following")
    )
  })
  model.poisson <- INLA::inla(
    Count ~ offset(log(size)) + period + f(herd, model = "iid"), 
    data = object@Data,
    family = "poisson"
  )
  it("checks if the model is a nbinomial model", {
    expect_that(
      n2k_inla_nbinomial(
        data = object, model.fit = model.poisson, status = "converged"
      ),
      throws_error("The model must be from the nbinomial family")
    )
  })
})
