context("get_sha1")
describe("get_sha1() on models", {
  data(cbpp, package = "lme4")
  cbpp$DatasourceID <- 1
  cbpp$ObservationID <- seq_len(nrow(cbpp))
  
  it("returns the correct sha1", {
    model <- lme4::glmer(
      incidence ~ offset(log(size)) + period + (1 | herd), 
      data = cbpp, 
      family = poisson
    )
    fixed <- coef(summary(model))
    random <- do.call(c, lme4::ranef(model))
    expect_that(
      get_sha1(model),
      is_identical_to(get_sha1(list(fixed, random)))
    )

    model <- lme4::glmer(
      incidence ~ offset(log(size)) + (1 | period) + (1 | herd), 
      data = cbpp, 
      family = poisson
    )
    fixed <- coef(summary(model))
    random <- do.call(c, lme4::ranef(model))
    expect_that(
      get_sha1(model),
      is_identical_to(get_sha1(list(fixed, random)))
    )
  })
})