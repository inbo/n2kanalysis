context("sha1")
describe("sha1() on models", {
  data(cbpp, package = "lme4")
  cbpp$DatasourceID <- sha1(letters)
  cbpp$ObservationID <- seq_len(nrow(cbpp))

  it("returns the correct sha1", {
    model <- lme4::glmer(
      incidence ~ offset(log(size)) + period + (1 | herd),
      data = cbpp,
      family = poisson
    )
    signif.coef <- vapply(
      lme4::ranef(model),
      sha1,
      digits = 6,
      FUN.VALUE = NA_character_
    )
    signif.coef <- c(
      fixed = sha1(lme4::fixef(model), digits = 6),
      signif.coef
    )
    expect_identical(
      sha1(model),
      sha1(signif.coef, digits = 6)
    )

    model <- lme4::glmer(
      incidence ~ offset(log(size)) + (1 | period) + (1 | herd),
      data = cbpp,
      family = poisson
    )
    signif.coef <- vapply(
      lme4::ranef(model),
      sha1,
      digits = 6,
      FUN.VALUE = NA_character_
    )
    signif.coef <- c(
      fixed = sha1(lme4::fixef(model), digits = 6),
      signif.coef
    )
    expect_identical(
      sha1(model),
      sha1(signif.coef, digits = 6)
    )
  })
})
