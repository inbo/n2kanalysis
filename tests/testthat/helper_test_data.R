#' A function to generate a simple dataset for unit testing
#' @param datasource.id the string id of the datasource
#' @param missing the required proportion of missing data
test_data <- function(datafield.id = sha1(letters), missing = 0){
  assertthat::assert_that(assertthat::is.string(datafield.id))
  assertthat::assert_that(assertthat::is.number(missing))
  assertthat::assert_that(missing >= 0)
  assertthat::assert_that(missing <= 1)

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
  mm.fixed <- model.matrix(~ A * (B + C) + C * D, data = dataset)
  fixed <- runif(ncol(mm.fixed))

  mm.random <- model.matrix(~ 0 + factor(E) : A, data = dataset)
  random <- rnorm(length(levels(dataset$A)) * n.e, sd = sd.random)
  random <- apply(matrix(random, nrow = n.e), 2, cumsum)
  random <- as.vector(random)
  eta <- mm.fixed %*% fixed + mm.random %*% random #nolint
  dataset$Count <- rnbinom(nrow(dataset), mu = exp(eta), size = theta)
  dataset$Count[rbinom(nrow(dataset), size = 1, prob = missing) == 1] <- NA
  dataset$DataFieldID <- datafield.id
  dataset$ObservationID <- seq_along(dataset$Count)
  return(dataset)
}
