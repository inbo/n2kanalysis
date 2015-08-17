#' Calculate SHA hash for glmerMod object
#' @rdname get_sha1_lme4
#' @importFrom methods setMethod
#' @importMethodsFrom n2khelper get_sha1
#' @importClassesFrom lme4 glmerMod
#' @importFrom lme4 ranef
#' @importFrom n2khelper sha1_digits zap_small
#' @param x the glmerMod object
#' @exportMethod get_sha1
setMethod(
  f = "get_sha1",
  signature = "glmerMod",
  definition = function(x){
    # the rounding of the coef must be quit aggressive
    signif.coef <- c(
      fixed = list(coef(summary(x))),
      ranef(x)
    )
    signif.coef <- lapply(
      signif.coef,
      function(z){
        apply(z, 1, function(y){
          sprintf(
            paste0("%.", sha1_digits("coef"), "e"),
            zap_small(y, digits = sha1_digits("zapsmall"))
          )
        })
      }
    )
    get_sha1(signif.coef)
  }
)
