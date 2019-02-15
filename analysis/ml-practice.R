

library("bbmle")
#set.seed(1)

##############################
# NORMAL
##############################

# simulate data for simple linear model

N <- 100
x <- runif(N, -2, 2)
a <- 1
b <- 1.5
sigma <- 1
eta <- a + b * x

y <- rnorm(N, mean = eta, sd = sigma )  

# plot simulated data 
plot(x, y)
lines(sort(x), eta[order(x)])

# check values are returned with lm
simple_lm <- lm(y~x)
coef(simple_lm)

# function to generate negative log-likelihood for a parameter value
nll_norm <- function(a, b) {
  eta <- a + b * x
   # sum of negative log likelihoods:
  -sum(dnorm(y, mean = eta, sd = sigma, log = TRUE))
}

# find maximum likelihood using bbmle::mle2 with intitial values normally distributed around 1
lm_mle2 <- mle2(nll_norm, start = list(a = rnorm(1), b = rnorm(1)))
confint(lm_mle2)

##############################
# POISSON
##############################

# Simulate data for poisson model

N <- 100
x <- runif(N, -2, 2)
a <- 1
b <- 1.5
lambda <- y_true
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
nll_gamma <- function(a, b, shape) {
  eta <- a + b * x
  # rate = shape / mu
  rate <- shape / exp(eta)
  # sum of negative log likelihoods:
  -sum(dgamma(y, rate = rate, shape = shape, log = TRUE))
}

# find maximum likelihood using bbmle::mle2 with intitial values normally distributed around 1
# 'shape' parameter must be positive so using values from log normal distribution
gamma_mle2 <- mle2(nll_gamma, start = list(a = rnorm(1), b = rnorm(1), shape = (rlnorm(1)))) 
confint(gamma_mle2)



