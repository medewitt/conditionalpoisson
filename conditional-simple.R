# Single Pass

library(tidyverse)
library(cmdstanr)

x1 <- rnorm(50, 0, 1)
x2 <- rnorm(50,0,1)

y <- rpois(50, exp(1*x1 + 0.2* x2))
hist(y)
X <- cbind(x1,x2)

mod_cp_simple <- cmdstan_model("conditional-simple.stan")

dat_simple <- list(
  N = length(x1),
  K = length(x1),
  J = ncol(X),
  x = X,
  y = y
)

fit_simple <- mod_cp_simple$sample(dat_simple)

z <- fit_simple$summary()

bayesplot::mcmc_areas(fit_simple$draws(variables = "beta"))
