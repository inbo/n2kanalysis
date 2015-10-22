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
    signif.coef <- lapply(
      lme4::ranef(model),
      function(x){
        sapply(
          x,
          n2khelper::num_32_64,
          digits = n2khelper::sha1_digits("coef"),
          zapsmall = n2khelper::sha1_digits("zapsmall")
        )
      }
    )
    signif.coef <- c(
      fixed = list(
        n2khelper::num_32_64(
          lme4::fixef(model),
          digits = n2khelper::sha1_digits("coef"),
          zapsmall = n2khelper::sha1_digits("zapsmall")
        )
      ),
      signif.coef
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
    signif.coef <- lapply(
      lme4::ranef(model),
      function(x){
        sapply(
          x,
          n2khelper::num_32_64,
          digits = n2khelper::sha1_digits("coef"),
          zapsmall = n2khelper::sha1_digits("zapsmall")
        )
      }
    )
    signif.coef <- c(
      fixed = list(
        n2khelper::num_32_64(
          lme4::fixef(model),
          digits = n2khelper::sha1_digits("coef"),
          zapsmall = n2khelper::sha1_digits("zapsmall")
        )
      ),
      signif.coef
    )
    expect_identical(
      get_sha1(model),
      get_sha1(signif.coef)
    )
  })
})
