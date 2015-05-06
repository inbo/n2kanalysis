context("prepare a n2kGlmerPoisson object")
describe("n2k_glmer_poisson", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    data = cbpp 
  )
  model.object <- lme4::glmer(
    incidence ~ offset(log(size)) + period + (1 | herd), 
    data = object@Data,
    family = poisson
  )
  model.truth <- lme4::glmer(
    incidence ~ offset(log(size)) + period + (1 | herd), 
    data = cbpp, 
    family = poisson
  )
  
  it("adds the data as a data.frame", {
    expect_that(
      object@Data,
      is_identical_to(cbpp)
    )
    expect_that(
      coef(model.object),
      is_identical_to(coef(model.truth))
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
      n2k_glmer_poisson(
        data = cbpp,
        status = "junk"
      ),
      throws_error("Status must be one of the following")
    )
    expect_that(
      n2k_glmer_poisson(
        data = cbpp,
        status = NA
      ),
      throws_error("Status must be character")
    )
  })
  it("uses '' as default weight", {
    expect_that(
      object@Weight,
      is_identical_to("")
    )
  })
  it("checks if the weight variable exists", {
    expect_that(
      n2k_glmer_poisson(
        data = cbpp,
        weight = "junk"
      ),
      throws_error("Variables missing in data: junk")
    )
  })
  it("sets the correct seed", {
    this.seed <- 12345L
    expect_that(
      n2k_glmer_poisson(
        data = cbpp,
        seed = this.seed
      )@Seed,
      is_identical_to(this.seed)
    )
  })
  it("converts numeric seed, when possible", {
    this.seed <- 12345
    expect_that(
      n2k_glmer_poisson(
        data = cbpp,
        seed = this.seed
      )@Seed,
      is_identical_to(as.integer(this.seed))
    )
    this.seed <- 12345L
    expect_that(
      n2k_glmer_poisson(
        data = cbpp,
        seed = this.seed + 1e-11
      )@Seed,
      is_identical_to(this.seed)
    )
    expect_that(
      n2k_glmer_poisson(
        data = cbpp,
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
})

context("add a model to a n2kGlmerPoisson object")
describe("n2k_glmer_poisson", {
  data("cbpp", package = "lme4")
  object <- n2k_glmer_poisson(
    data = cbpp 
  )
  model.object <- lme4::glmer(
    incidence ~ offset(log(size)) + period + (1 | herd), 
    data = object@Data,
    family = poisson
  )
  model.truth <- lme4::glmer(
    incidence ~ offset(log(size)) + period + (1 | herd), 
    data = cbpp, 
    family = poisson
  )
  object.model <- n2k_glmer_poisson(
    data = object, model.fit = model.object, status = "converged"
  )
  it("keeps the objects", {
    expect_that(
      object.model@Data,
      is_identical_to(cbpp)
    )
    expect_that(
      coef(object.model@Model),
      is_identical_to(coef(model.truth))
    )
    expect_that(
      object.model@Model,
      is_identical_to(model.object)
    )
    expect_that(
      object.model@Weight,
      is_identical_to(object@Weight)
    )
    expect_that(
      n2k_glmer_poisson(
        data = object, model.fit = model.object, status = "converged", weight = "junk"
      )@Weight,
      is_identical_to(object@Weight)
    )
    expect_that(
      object.model@Seed,
      is_identical_to(object@Seed)
    )
    expect_that(
      n2k_glmer_poisson(
        data = object, model.fit = model.object, status = "converged", seed = 1
      )@Seed,
      is_identical_to(object@Seed)
    )
  })
  it("stores the new status", {
    expect_that(
      object.model@Status,
      is_identical_to("converged")
    )
    expect_that(
      n2k_glmer_poisson(
        data = object, model.fit = model.object, status = "junk"
      ),
      throws_error("Status must be one of the following")
    )
  })
  model.binomial <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd), 
    data = object@Data,
    family = binomial
  )
  it("checks if the model is a poisson model", {
    expect_that(
      n2k_glmer_poisson(
        data = object, model.fit = model.binomial, status = "converged"
      ),
      throws_error("The model must be from the poisson family")
    )
  })
})
