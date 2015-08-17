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
    signif.coef <- c(
      fixed = list(coef(summary(model))),
      lme4::ranef(model)
    )
    signif.coef <- lapply(
      signif.coef,
      function(z){
        apply(z, 1, function(y){
          sprintf(
            paste0("%.", sha1_digits("coef"), "e"),
            zapsmall(y, digits = sha1_digits("zapsmall"))
          )
        })
      }
    )
    expect_identical(
      get_sha1(model),
      get_sha1(signif.coef)
    )

    model <- lme4::glmer(
      incidence ~ offset(log(size)) + (1 | period) + (1 | herd),
      data = cbpp,
      family = poisson
    )
    signif.coef <- c(
      fixed = list(coef(summary(model))),
      lme4::ranef(model)
    )
    signif.coef <- lapply(
      signif.coef,
      function(z){
        apply(z, 1, function(y){
          sprintf(
            paste0("%.", sha1_digits("coef"), "e"),
            zapsmall(y, digits = sha1_digits("zapsmall"))
          )
        })
      }
    )
    expect_identical(
      get_sha1(model),
      get_sha1(signif.coef)
    )
  })
})
