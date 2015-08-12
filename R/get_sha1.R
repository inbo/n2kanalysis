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
    get_sha1(
      list(
        coef(summary(x)),
        do.call(c, ranef(x))
      )
    )
  }
)
