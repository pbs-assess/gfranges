

library("bbmle")
#set.seed(1)


# simulate data for simple linear model

N <- 100
x <- runif(N, -1, 1)
a <- 5
b <- 2
sigma <- 1.5
y_true <- a + b * x

y <- rnorm(N, mean = y_true, sd = sigma )  

# plot simulated data 
plot(x, y)
lines(sort(x), y_true[order(x)])

# check values are returned with lm
simple_lm <- lm(y~x)
coef(simple_lm)

# function to generate negative log-likelihood for a parameter value
nll_norm <- function(a, b) {
  y_true <- a + b * x
   # sum of negative log likelihoods:
  -sum(dnorm(y, mean = y_true, sd = sigma,
              log = TRUE))
}

# find maximum likelihood using bbmle::mle2 with intitial values normally distributed around 1
mle2(nll_norm, start = list(a = rnorm(1), b = rnorm(1)))


# Simulate data for poisson model

N <- 100
x <- runif(N, -1, 1)
a <- 1
b <- 0.5
lambda <- y_true
y_true <- exp(a + b * x)

y <- rpois(N, lambda = y_true)  

# plot simulated data 
plot(x, y)
lines(sort(x), y_true[order(x)])

# check values are returned with glm
pois_glm <- glm(y~x, family = poisson)
coef(pois_glm)

# function to generate negative log-likelihood for a parameter value
nll_pois <- function(a, b) {
  y_true <- exp(a + b * x)
  # sum of negative log likelihoods:
  -sum(dpois(y, lambda = y_true, 
             log = TRUE))
}

# find maximum likelihood using bbmle::mle2 with intitial values normally distributed around 1
mle2(nll_pois, start = list(a = rnorm(1), b = rnorm(1)))
