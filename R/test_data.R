# A function to generate a simple dataset for unit testing
#' @importFrom dplyr %>%
#' @importFrom stats model.matrix rnbinom rnorm runif
test_data <- function(){
  set.seed(999)
  n.e <- 10
  sd.random <- 0.1
  theta <- 4

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
  eta <- mm.fixed %*% fixed + mm.random %*% random
  dataset$Count <- rnbinom(nrow(dataset), mu = exp(eta), size = theta)
  dataset$DatasourceID <- 1L
  dataset$ObservationID <- seq_along(dataset$Count)
  return(dataset)
}
