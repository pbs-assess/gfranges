##############################
# Maximum likelihood simulation practice
##############################

#set.seed(1)
library("bbmle")




##############################
# NORMAL
##############################

# simulate data for simple linear model

N <- 100
x <- runif(N, -2, 2)
a <- 1
b <- 1.5
sigma <- 1
y_true <- a + b * x

y <- rnorm(N, mean = y_true, sd = sigma )  

# plot simulated data 
plot(x, y)
lines(sort(x), y_true[order(x)])

# check values are returned with lm
simple_lm <- lm(y~x)
coef(simple_lm)

# function to generate negative log-likelihood for a parameter value
nll_norm <- function(a, b, log_sigma) {
  eta <- a + b * x
   # sum of negative log likelihoods:
  -sum(dnorm(y, mean = eta, sd = exp(log_sigma), log = TRUE))
}

# find maximum likelihood using bbmle::mle2 with intitial values normally distributed around 1
lm_mle2 <- mle2(nll_norm, start = list(a = rnorm(1), b = rnorm(1), log_sigma = 0))
confint(lm_mle2)





##############################
# POISSON
##############################

# Simulate data for poisson model

N <- 100
x <- runif(N, -2, 2)
a <- 1
b <- 1.5
mu <- exp(a + b * x)

y <- rpois(N, lambda = mu)  

# plot simulated data 
plot(x, y)
lines(sort(x), mu[order(x)])

# check values are returned with glm
pois_glm <- glm(y~x, family = poisson(link=log))
coef(pois_glm)

# function to generate negative log-likelihood for a parameter value
nll_pois <- function(a, b) {
  mu <- exp(a + b * x)
  # sum of negative log likelihoods:
  -sum(dpois(y, lambda = mu, log = TRUE))
}

# find maximum likelihood using bbmle::mle2 with intitial values normally distributed around 1
pois_mle2 <- mle2(nll_pois, start = list(a = rnorm(1), b = rnorm(1)))
confint(pois_mle2)






##############################
# LOGISTIC
##############################

# Simulate data for logistic model

N <- 100
x <- runif(N, -2, 2)
a <- 1
b <- 1.5
eta <- a + b * x
mu <- exp(eta)/(1 + exp(eta))

y <- rbinom(N, 1, prob = mu)

# plot simulated data 
plot(x, y)
lines(sort(x), mu[order(x)])

# check values are returned with glm
logistic_glm <- glm(y~x, family = binomial(link=logit))
coef(logistic_glm)

# function to generate negative log-likelihood for a parameter value
nll_logistic <- function(a, b) {
  eta <- a + b * x
  mu <- exp(eta)/(1 + exp(eta))
  # sum of negative log likelihoods:
  -sum(dbinom(y, size = 1, prob = mu, log = TRUE))
}

# find maximum likelihood using bbmle::mle2 with intitial values normally distributed around 1
logistic_mle2 <- mle2(nll_logistic, start = list(a = rnorm(1), b = rnorm(1)))
confint(logistic_mle2)






##############################
# GAMMA
##############################

# Simulate data for gamma model

N <- 100
x <- runif(N, -2, 2)
a <- 1
b <- 1.5
eta <- a + b * x
mu <- exp(eta)
shape <- 10
y <- rgamma(N, rate = shape / mu, shape = shape)

# plot simulated data 
plot(x, y)
lines(sort(x), mu[order(x)])

# check values are returned with glm
gamma_glm <- glm(y~x, family = Gamma(link=log))
coef(gamma_glm)

# function to generate negative log-likelihood for a parameter value
nll_gamma <- function(a, b, log_shape) {
  shape <- exp(log_shape)
  eta <- a + b * x
  mu <- exp(eta)
  # rate = shape / mu
  rate <- shape / mu
  # sum of negative log likelihoods:
  -sum(dgamma(y, rate = rate, shape = shape, log = TRUE))
}

# find maximum likelihood using bbmle::mle2 with intitial values normally distributed around 1
# 'shape' parameter must be positive so using values from log normal distribution
gamma_mle2 <- mle2(nll_gamma, start = list(a = rnorm(1), b = rnorm(1), log_shape = 0))
gamma_mle2
confint(gamma_mle2)






##############################
# TWEEDIE
##############################

# Simulate data for compound poisson-gamma Tweedie model
# continuous gamma with exact zeros with 1 < power < 2
# Var(y) = phi ∗ mu ^ power

library(tweedie)
library(statmod)

N <- 100
x <- runif(N, -2, 2)
a <- 3
b <- 1

power <- 1.5 # must be between 1 and 2 for compound poisson-gamma
phi <- 1 # dispersion must be positive
eta <- a + b * x 
mu <- exp(eta) 
y <- rtweedie(N, power = power, mu = mu, phi = phi) 

data <- data.frame(y,x)

# plot simulated data 
plot(x, y)
lines(sort(x), mu[order(x)])

hist(y, breaks=30)

# check values are returned with glm

tweedie_glm <- glm(y ~ x, family = tweedie(var.power = 1.5, link.power = 0), data=data)
summary(tweedie_glm)
coef(tweedie_glm)


# function to generate negative log-likelihood for a parameter value
nll_tweedie <- function(par) {
  eta <- par[1] + par[2] * x
  mu <- exp(eta)
  p <- plogis(par[3]) + 1
  phi <- exp(par[4])
  # sum of negative log likelihoods:
  loglik <- log(dtweedie(y, power = p, mu = mu, phi = phi))
  nll <- -sum(loglik) #[is.finite(loglik)]
  print(nll)
  nll
}

# find maximum likelihood using nlminb
tweedie_ml <- nlminb(start = c(a = 0, b = 0, ipx = 0, log_phi = log(1)), nll_tweedie)
tweedie_ml$par 




# function to generate negative log-likelihood for a parameter value
nll_tweedie <- function(a, b, log_phi) {
  eta <- a + b * (x)
  mu <- exp(eta)
  #p <- plogis(ipx) + 1
  phi <- exp(log_phi)
  # sum of negative log likelihoods:
  loglik <- log(dtweedie.inversion(y, power = 1.5, mu = mu, phi = phi))
  nll <- -sum(loglik[is.finite(loglik)])
  print(nll)
  nll
}

# find maximum likelihood using bbmle::mle2 with intitial values normally distributed around 1
tweedie_mle2 <- mle2(nll_tweedie, start = list(a = 3, b = 1, log_phi = 0)) 
attributes(tweedie_mle2)
