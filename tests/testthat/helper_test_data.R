#' A function to generate a simple dataset for unit testing
#' @param datasource_id the string id of the data source
#' @param missing the required proportion of missing data
test_data <- function(datafield_id = sha1(letters), missing = 0) {
  assertthat::assert_that(assertthat::is.string(datafield_id))
  assertthat::assert_that(assertthat::is.number(missing))
  assertthat::assert_that(missing >= 0)
  assertthat::assert_that(missing <= 1)

  set.seed(999)
  n_e <- 10
  sd_random <- 0.1
  theta <- 4

  dataset <- expand.grid(
    A = factor(c("a1", "a2", "a3")),
    B = factor(c("b1", "b2")),
    C = 1:3,
    D = -3:-1,
    E = seq_len(n_e),
    G = seq_len(3)
  )
  mm_fixed <- model.matrix(~ A * (B + C) + C * D, data = dataset)
  fixed <- runif(ncol(mm_fixed))

  mm_random <- model.matrix(~ 0 + factor(E) : A, data = dataset)
  random <- rnorm(length(levels(dataset$A)) * n_e, sd = sd_random)
  random <- apply(matrix(random, nrow = n_e), 2, cumsum)
  random <- as.vector(random)
  eta <- mm_fixed %*% fixed + mm_random %*% random
  dataset$Count <- rnbinom(nrow(dataset), mu = exp(eta), size = theta)
  dataset$Count[rbinom(nrow(dataset), size = 1, prob = missing) == 1] <- NA
  dataset$datafield_id <- datafield_id
  dataset$observation_id <- seq_along(dataset$Count)
  return(dataset)
}
