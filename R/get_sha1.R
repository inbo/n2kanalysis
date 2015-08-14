#' Calculate SHA hash for glmerMod object
#' @rdname get_sha1_lme4
#' @importFrom methods setMethod
#' @importMethodsFrom n2khelper get_sha1
#' @importClassesFrom lme4 glmerMod
#' @importFrom lme4 ranef
#' @param x the glmerMod object
setMethod(
  f = "get_sha1",
  signature = "glmerMod",
  definition = function(x){
    # the rounding of the coef must be quit aggressive
    # signif.digits <- 5 yields a different SHA1 on 32-bit and 64-bit
    signif.digits <- 4
    signif.coef <- lapply(
      ranef(x),
      function(y){
        signif(y, digits = signif.digits)
      }
    )
    signif.coef <- c(
      signif.coef,
      list(signif(coef(summary(x)), digits = signif.digits))
    )
    get_sha1(signif.coef)
  }
)
