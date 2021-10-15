#' The n2kManifest class
#' @name n2kManifest-class
#' @rdname n2kManifest-class
#' @exportClass n2kManifest
#' @aliases n2kManifest-class
#' @importFrom methods setClass
#' @importFrom digest sha1
#' @docType class
setClass(
  "n2kManifest",
  representation = representation(
    Manifest = "data.frame",
    Fingerprint = "character"
  ),
  prototype = prototype(
    Manifest = data.frame(
      Fingerprint = character(0),
      Parent = character(0),
      stringsAsFactors = FALSE
    ),
    Fingerprint = sha1(
      data.frame(
        Fingerprint = character(0),
        Parent = character(0),
        stringsAsFactors = FALSE
      )
    )
  )
)

#' @importFrom methods setValidity
#' @importFrom n2khelper check_dataframe_variable
#' @importFrom assertthat assert_that noNA
#' @importFrom dplyr filter anti_join left_join select
#' @importFrom rlang .data
setValidity(
  "n2kManifest",
  function(object) {
    assert_that(
      is.string(object@Fingerprint),
      msg = "Fingerprint is not a string (a length one character vector)."
    )
    check_dataframe_variable(
      df = object@Manifest,
      variable = list(Fingerprint = "character", Parent = "character"),
      force_na = TRUE,
      name = "Manifest"
    )
    assert_that(
      noNA(object@Manifest$Fingerprint),
      msg = "Fingerprint contains missing values"
    )

    if (all(!is.na(object@Manifest$Parent))) {
      stop("All rows have parents")
    }

    self_link <- object@Manifest %>%
      filter(.data$Fingerprint == Parent) %>%
      nrow()
    if (self_link > 0) {
      stop("Self references between Parent and Fingerprint")
    }

    if (!all(is.na(object@Manifest$Parent))) {
      missing_link <- object@Manifest %>%
        filter(!is.na(.data$Parent)) %>%
        anti_join(object@Manifest, by = c("Parent" = "Fingerprint")) %>%
        nrow()
      if (missing_link  > 0) {
        stop("Some Parent in 'Manifest' slot have no matching Fingerprint")
      }
    }

    if (any(!is.na(object@Manifest$Parent))) {
      link <- object@Manifest %>%
        left_join(object@Manifest, by = c("Parent" = "Fingerprint")) %>%
        select(.data$Fingerprint, Parent = .data$Parent.y)
      i <- 1
      while (any(!is.na(link$Parent))) {
        if (i > 10) {
          stop("Too many parent - child levels.")
        }
        i <- i + 1
        link <- link %>%
          left_join(object@Manifest, by = c("Parent" = "Fingerprint")) %>%
          select(.data$Fingerprint, Parent = .data$Parent.y)
      }
    }

    if (sha1(object@Manifest) != object@Fingerprint) {
      stop("wrong fingerprint")
    }
    return(TRUE)
  }
)
