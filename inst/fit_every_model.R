library(INLA)
library(optimx)
library(n2kanalysis)
fit_every_model(
  path = "~/analysis", 
  status = c("new", "error", "converged", "false convergence")
)
