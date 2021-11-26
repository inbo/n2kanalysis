#' @importFrom digest sha1
#' @export sha1
digest::sha1

#' @importFrom digest sha1
#' @export
#' @method sha1 inla
#' @include import_S3_classes.R
sha1.inla <- function(x, digits = 14L, zapsmall = 7L, ...) { #nolint
  parameter <- list(
    RandomEffects = x$summary.random,
    FixedEffects = list(x$summary.fixed),
    Hyper = list(x$summary.hyperpar)
  )
  attr(parameter, "digest::sha1") <- list(
    class = class(x),
    digits = as.integer(digits),
    zapsmall = as.integer(zapsmall),
    ... = ...
  )
  sha1(parameter, digits = digits, zapsmall = zapsmall, ... = ...)
}

#' @importFrom digest sha1
#' @importClassesFrom multimput rawImputed
#' @export
#' @method sha1 rawImputed
sha1.rawImputed <- function(x, digits = 14L, zapsmall = 7L, ...) {# nolint
  parameter <- list(
    Data = x@Data,
    Response = x@Response,
    Imputation = x@Imputation
  )
  attr(parameter, "digest::sha1") <- list(
    class = class(x),
    digits = as.integer(digits),
    zapsmall = as.integer(zapsmall),
    ... = ...
  )
  sha1(parameter, digits = digits, zapsmall = zapsmall, ...)
}

#' @importFrom digest sha1
#' @importClassesFrom multimput aggregatedImputed
#' @export
#' @method sha1 aggregatedImputed
sha1.aggregatedImputed <- function(x, digits = 14L, zapsmall = 7L, ...) {# nolint
  parameter <- list(
    Covariate = x@Covariate,
    Imputation = x@Imputation
  )
  attr(parameter, "digest::sha1") <- list(
    class = class(x),
    digits = as.integer(digits),
    zapsmall = as.integer(zapsmall),
    ...
  )
  sha1(parameter, digits = digits, zapsmall = zapsmall, ...)
}
