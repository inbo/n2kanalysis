#' Calculate SHA hash for glmerMod object
#' @rdname get_sha1_lme4
#' @importFrom methods setMethod
#' @importMethodsFrom n2khelper get_sha1
#' @importClassesFrom lme4 glmerMod
#' @importFrom lme4 ranef fixef
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
          fixef(x),
          digits = sha1_digits("coef"),
          zapsmall = sha1_digits("zapsmall")
        )
      ),
      signif.coef
    )
    get_sha1(signif.coef)
  }
)

#' Calculate SHA hash for inla object
#' @rdname get_sha1_inla
#' @importFrom methods setMethod
#' @importMethodsFrom n2khelper get_sha1
#' @importFrom n2khelper sha1_digits num_32_64 get_sha1
#' @param x the inla object
#' @exportMethod get_sha1
#' @include import_S3_classes.R
setMethod(
  f = "get_sha1",
  signature = "inla",
  definition = function(x){
    parameter <- c(
      x$summary.random,
      FixedEffects = list(x$summary.fixed),
      Hyper = list(x$summary.hyperpar)
    )
    parameter <- lapply(
      parameter,
      function(y){
        sapply(
          y,
          function(z){
            if (!is.numeric(z)) {
              return(z)
            }
            num_32_64(
              z,
              digits = sha1_digits("coef"),
              zapsmall = sha1_digits("zapsmall")
            )
          }
        )
      }
    )
    get_sha1(parameter)
  }
)
