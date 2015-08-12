library(INLA)
library(optimx)
library(n2kanalysis)
fit_every_model(
  path = paste("~", "analysis", "abv", sep = "/"),
  status = c("new", "waiting")
)
