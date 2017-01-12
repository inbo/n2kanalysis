#' A function to generate a simple dataset for unit testing
#' @param datasource.id the string id of the datasource
#' @param missing the required proportion of missing data
#' @importFrom assertthat assert_that is.string is.number
#' @importFrom dplyr %>% mutate_ n row_number
#' @importFrom stats model.matrix rnbinom rnorm runif
test_data <- function(datasource.id = sha1(letters), missing = 0){
  assert_that(is.string(datasource.id))
  assert_that(is.number(missing))
  assert_that(missing >= 0)
  assert_that(missing <= 1)

  set.seed(999)
  n.e <- 10
  sd.random <- 0.1
  theta <- 4 #nolint

  dataset <- expand.grid(
    A = factor(c("a1", "a2", "a3")),
    B = factor(c("b1", "b2")),
    C = 1:3,
    D = -3:-1,
    E = seq_len(n.e),
    F = seq_len(3)
  )
  mm.fixed <- model.matrix(
    ~ A * (B + C) + C * D,
    data = dataset
  )
  fixed <- runif(ncol(mm.fixed))

  mm.random <- model.matrix(
    ~ 0 + factor(E) : A,
    data = dataset
  )
  random <- dataset$A %>%
    levels() %>%
    length() %>%
    "*"(n.e) %>% #nolint
    rnorm(sd = sd.random) %>%
    matrix(nrow = n.e) %>%
    apply(2, cumsum) %>%
    as.vector()
  eta <- mm.fixed %*% fixed + mm.random %*% random #nolint
  dataset <- dataset %>%
    mutate_(
      Count = ~ifelse(
        rbinom(n(), size = 1, prob = missing) == 1,
        NA,
        rnbinom(n(), mu = exp(eta), size = theta)
      ),
      DatasourceID = ~datasource.id,
      ObservationID = ~row_number(Count)
    )
  return(dataset)
}
