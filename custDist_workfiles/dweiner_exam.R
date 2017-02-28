# Examine the correctness of dweiner in JAGS

# First, we need to load Rube from C
# Then we need to set up the file for customized distributions
# Then we need to run examples from the JAGS website

library(RWiener)
library(R2jags)
load.module("wiener")

# 1. Load rube from c:
# a. use wickman's devtools
library(devtools)
# install('C:/rube/rube')
# For some reason doesn't load the files...

# b. use load all fine in the source file
load_all("C:/rube/rube/R")

# 2. Set up customized distributions
# "test.custDist.txt"
# the file can be directly called in rube

# 3.
# Example 0 - fake data
mastery.data <- list(x=4) # very little data!!
mastery.learning.model <- "model {
  r <- 3
  x ~ dwiener(a,b,c,d)
  p <- sqrt(2*u + 0.25) - 0.5
  u ~ dunif(0,1)
}"
mastery.init <- function() {
  return(list(u=runif(1)))
}
rube(mastery.learning.model, mastery.data, mastery.init, 
     custom.dist = "test.custDist.txt")


# Example 1

# Quoted from JAGS Example
# Read original data from the experiment
dat <- read.table("example/1/R/data/exp1_01_m23r_2012-10-31-110958.txt", 
                  skip=8, header=T)
dat <- dat[21:218,] # Throw out trainig trials
dat$RT[dat$Key == "a"] = -dat$RT[dat$Key == "a"]
dat <- data.frame(Id=dat$Id,Cond=as.numeric(dat$Stim), RT=dat$RT)

# Put data in a list for simple use in the jags.model() command
dat <- list(N=198, y=dat$RT, C=3, cond=dat$Cond)

# End of Quote

ex.1.model = " model {
  alpha ~ dunif(0.0001,3) # Estimate

  tau ~ dunif(0,1)

  beta <- 0.5

  for (c in 1:C) {
    delta[c] ~ dnorm(0, 1)
  }

  for (i in 1:N) {
    y[i] ~ dwiener(alpha,tau,beta,delta[cond[i]])
  }

}"

ex.1.init = function() {
  list(alpha = 1, tau = 0.001, delta = c(3, 0.5, -2))
}


example.1.rube = rube(ex.1.model, dat, ex.1.init, custom.dist = "test.custDist.txt", n.iter = 500)
summary(example.1.rube)