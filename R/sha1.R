#' @importFrom digest sha1
#' @export sha1
digest::sha1

#' @importFrom digest sha1
#' @importFrom lme4 fixef ranef
#' @export
#' @method sha1 glmerMod
sha1.glmerMod <- function(x, digits = 6L, zapsmall = 7L) {
  signif.coef <- vapply(
    ranef(x),
    sha1,
    digits = digits,
    zapsmall = zapsmall,
    FUN.VALUE = NA_character_
  )
  signif.coef <- c(
    fixed = sha1(fixef(x), digits = digits, zapsmall = zapsmall),
    signif.coef
  )
  attr(signif.coef, "digest::sha1") <- list(
    class = class(x),
    digits = as.integer(digits),
    zapsmall = as.integer(zapsmall)
  )
  sha1(signif.coef, digits = digits, zapsmall = zapsmall)
}

#' @importFrom digest sha1
#' @export
#' @method sha1 inla
#' @include import_S3_classes.R
sha1.inla <- function(x, digits = 14L, zapsmall = 7L) {
  parameter <- list(
    RandomEffects = x$summary.random,
    FixedEffects = list(x$summary.fixed),
    Hyper = list(x$summary.hyperpar)
  )
  attr(parameter, "digest::sha1") <- list(
    class = class(x),
    digits = as.integer(digits),
    zapsmall = as.integer(zapsmall)
  )
  sha1(parameter, digits = digits, zapsmall = zapsmall)
}
