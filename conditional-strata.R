# Single Pass

library(tidyverse)
library(cmdstanr)

x1 <- rnorm(40, 0, 1)
x2 <- rnorm(40,0,1)
strata <- rep(1:4,10)

bias <- c(.1,-.1,.1,.2)

y <- rpois(50, exp(1*x1 + 0.2* x2 + bias[strata]))

# 4 different strata
# 10 replicates per strata

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
