# Inference from Iterative Simulation Using Multiple Sequences
# Author(s): Andrew Gelman and Donald B. Rubin
# Source: Statistical Science, Vol. 7, No. 4 (Nov., 1992), pp. 457-472
# Stable URL: http://www.jstor.org/stable/2246093
#
## Matches gelman.diag() from package "coda", but not WinBUGS() "summary" component.
## Better than gelman.diag() because multivariate stat is not bothered to be calculated
Rhat1 <- function(mat) {
  m <- ncol(mat)
  n <- nrow(mat)
  b <- apply(mat,2,mean)
  B <- sum((b-mean(mat))^2)*n/(m-1)
  w <- apply(mat,2,var)
  W <- mean(w)
  s2hat <- (n-1)/n*W + B/n
  Vhat <- s2hat + B/m/n 
  covWB <- n /m * (cov(w,b^2)-2*mean(b)*cov(w,b))
  varV <- (n-1)^2 / n^2 * var(w)/m +
          (m+1)^2 / m^2 / n^2 * 2*B^2/(m-1) +
          2 * (m-1)*(n-1)/m/n^2 * covWB
  df <- 2 * Vhat^2 / varV
  R <- sqrt((df+3) * Vhat / (df+1) / W)
  return(R)
}

Rhat <- function(arr) {
  dm <- dim(arr)
  if (length(dm)==2) return(Rhat1(arr))
  if (dm[2]==1) return(NULL)
  if (dm[3]==1) return(Rhat1(arr[,,1]))
  return(apply(arr,3,Rhat1))
}


# Calculate Gelman Rhat on sims.array (for n.chains = 2 to 4)
#Rhat <- function(arr) {
#  m <- dim(arr)[2]
#  if (m==1) return(NA)
#  if (m>4) stop("m>4 not implemented")
#  if (m==2) {
#    tmp <- mcmc.list(mcmc(arr[,1,]), mcmc(arr[,2,]))
#  } else if (m==3) {
#    tmp <- mcmc.list(mcmc(arr[,1,]), mcmc(arr[,2,]), mcmc(arr[,3,]))
#  } else if (m==3) {
#    tmp <- mcmc.list(mcmc(arr[,1,]), mcmc(arr[,2,]), mcmc(arr[,3,]), mcmc(arr[,4,]))
#  }
#  return(as.numeric(gelman.diag(tmp, autoburnin=FALSE)$psrf[,1]))
#}

## Rhat(rbst13$sims.array[,,1:5])
