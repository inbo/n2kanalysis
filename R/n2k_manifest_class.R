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
    Manifest = "data.frame", Fingerprint = "character"
  ),
  prototype = prototype(
    Manifest = data.frame(
      fingerprint = character(0), parent = character(0),
      stringsAsFactors = FALSE
    ),
    Fingerprint = sha1(
      data.frame(
        fingerprint = character(0), parent = character(0),
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
      df = object@Manifest, force_na = TRUE, name = "Manifest",
      variable = list(fingerprint = "character", parent = "character")
    )
    assert_that(
      noNA(object@Manifest$fingerprint),
      msg = "fingerprint contains missing values"
    )

    assert_that(
      any(is.na(object@Manifest$parent)), msg = "All rows have parents"
    )

    self_link <- object@Manifest %>%
      filter(.data$fingerprint == .data$parent) %>%
      nrow()
    assert_that(
      self_link == 0, msg = "Self references between parent and fingerprint"
    )

    if (!all(is.na(object@Manifest$parent))) {
      missing_link <- object@Manifest %>%
        filter(!is.na(.data$parent)) %>%
        anti_join(object@Manifest, by = c("parent" = "fingerprint")) %>%
        nrow()
      assert_that(
        missing_link == 0,
        msg = "Some parent in 'Manifest' slot have no matching fingerprint"
      )
    }

    if (any(!is.na(object@Manifest$parent))) {
      object@Manifest %>%
        left_join(object@Manifest, by = c("parent" = "fingerprint")) %>%
        select(.data$fingerprint, parent = .data$parent.y) -> link
      i <- 1
      while (any(!is.na(link$parent))) {
        if (i > 10) {
          stop("Too many parent - child levels.")
        }
        i <- i + 1
        link %>%
          left_join(object@Manifest, by = c("parent" = "fingerprint")) %>%
          select(.data$fingerprint, parent = .data$parent.y) -> link
      }
    }

    assert_that(
      sha1(object@Manifest) == object@Fingerprint, msg = "wrong fingerprint"
    )
    return(TRUE)
  }
)
