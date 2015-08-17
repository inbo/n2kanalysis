#' Calculate SHA hash for glmerMod object
#' @rdname get_sha1_lme4
#' @importFrom methods setMethod
#' @importMethodsFrom n2khelper get_sha1
#' @importClassesFrom lme4 glmerMod
#' @importFrom lme4 ranef fixef
#' @importFrom n2khelper sha1_digits
#' @param x the glmerMod object
#' @exportMethod get_sha1
setMethod(
  f = "get_sha1",
  signature = "glmerMod",
  definition = function(x){
    # the rounding of the coef must be quit aggressive
    # signif.digits <- 5 yields a different SHA1 on 32-bit and 64-bit
    signif.digits <-
    signif.coef <- lapply(
      ranef(x),
      function(y){
        signif(y, digits = sha1_digits("coef"))
      }
    )
    signif.coef <- c(
      signif.coef,
      list(
        signif(fixef(x), digits = sha1_digits("coef"))
      )
    )
    get_sha1(signif.coef)
  }
)
