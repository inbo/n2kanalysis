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
#' @importFrom dplyr filter_ anti_join left_join select_
setValidity(
  "n2kManifest",
  function(object){
    check_dataframe_variable(
      df = object@Manifest,
      variable = c("Fingerprint", "Parent"),
      name = "Manifest"
    )
    assert_that(
      noNA(object@Manifest$Fingerprint),
      msg = "Fingerprint contains missing values"
    )

    if (class(object@Manifest$Fingerprint) != class(object@Manifest$Parent)) {
      if (!all(is.na(object@Manifest$Parent))) {
        stop("Fingerprint and Parent must be of the same class")
      }
    }

    if (class(object@Manifest$Fingerprint) == "factor") {
      if (
        !identical(
          levels(object@Manifest$Fingerprint),
          levels(object@Manifest$Parent)
        )
      ) {
        stop("Fingerprint and Parent must have the same levels")
      }
    }

    if (all(!is.na(object@Manifest$Parent))) {
      stop("All rows have parents")
    }

    self_link <- object@Manifest %>%
      filter_(~Fingerprint == Parent) %>%
      nrow()
    if (self_link > 0) {
      stop("Self references between Parent and Fingerprint")
    }

    missing_link <- object@Manifest %>%
      filter_(~!is.na(Parent)) %>%
      anti_join(object@Manifest, by = c("Parent" = "Fingerprint")) %>%
      nrow()
    if (missing_link  > 0) {
      stop("Some Parent in 'Manifest' slot have no matching Fingerprint")
    }

    if (any(!is.na(object@Manifest$Parent))) {
      link <- object@Manifest %>%
        left_join(object@Manifest, by = c("Parent" = "Fingerprint")) %>%
        select_(~Fingerprint, Parent = ~Parent.y)
      i <- 1
      while (any(!is.na(link$Parent))) {
        if (i > 10) {
          stop("Too many parent - child levels.")
        }
        i <- i + 1
        link <- link %>%
          left_join(object@Manifest, by = c("Parent" = "Fingerprint")) %>%
          select_(~Fingerprint, Parent = ~Parent.y)
      }
    }

    if (length(object@Fingerprint) != 1) {
      stop("Fingerprint must be a single character")
    }
    if (sha1(object@Manifest) != object@Fingerprint) {
      stop("wrong fingerprint")
    }
    return(TRUE)
  }
)
