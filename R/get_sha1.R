#' Calculate SHA hash for glmerMod object
#' @rdname get_sha1_lme4
#' @importFrom methods setMethod
#' @importMethodsFrom n2khelper get_sha1
#' @importClassesFrom lme4 glmerMod
#' @importFrom lme4 ranef
#' @importFrom n2khelper sha1_digits num_32_64 get_sha1
#' @param x the glmerMod object
#' @exportMethod get_sha1
setMethod(
  f = "get_sha1",
  signature = "glmerMod",
  definition = function(x){
    signif.coef <- lapply(
      ranef(x),
      function(y){
        sapply(
          y,
          num_32_64,
          digits = sha1_digits("coef"),
          zapsmall = sha1_digits("zapsmall")
        )
      }
    )
    signif.coef <- c(
      fixed = list(
        num_32_64(
          coef(summary(x)),
          digits = sha1_digits("coef"),
          zapsmall = sha1_digits("zapsmall")
        )
      ),
      signif.coef
    )
    get_sha1(signif.coef)
  }
)
