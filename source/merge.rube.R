##################################################################
# Merge the chains of two rube objects into a single object with #
# n.chains+n.chains chains.                                      #
##################################################################
merge.rube <- function(x, y, ..., minimal=FALSE) {
  if (!is(x,"rube")) stop(deparse(x), " must be a rube object")
  if (!is(y,"rube")) stop(deparse(y), " must be a rube object")
  if (!is(x,"bugs") && !is(x,"rjags")) stop(deparse(x), " did not successfully complete an MCMC run")
  if (!is(y,"bugs") && !is(y,"rjags")) stop(deparse(y), " did not successfully complete an MCMC run")
  if (length(x$model)!=length(y$model) || any(x$model!=y$model))
    warning("models differ!")

  if (!(is.null(x$engine) || is.null(y$engine)) && (x$engine != y$engine))
    stop("rube() results were generated with different MCMC engines")

  # drop initial values to make chains the same length
  dimX <- dim(x$sims.array)
  dimY <- dim(y$sims.array)
  ncx <- dimX[2]
  ncy <- dimY[2]
  nx <- dimX[1]
  ny <- dimY[1]
  if (nx>ny) {
    start <- nx - ny + 1
    x$sims.array <- x$sims.array[start:nx,,,drop=FALSE]
    nx <- ny
  } else if (ny>nx) {
    start <- ny - nx + 1
    y$sims.array <- y$sims.array[start:ny,,,drop=FALSE]
    ny <- nx
  }

  # discard non-common parameters
  namesx <- dimnames(x$sims.array)[[3]]
  np <- length(namesx)
  namesy <- dimnames(y$sims.array)[[3]]
  if (length(namesx)!=length(namesy) || any(namesx!=namesy)) {
    common <- intersect(namesx,namesy)
    x$sims.array <- x$sims.array[,,match(common,namesx),drop=FALSE]
    y$sims.array <- y$sims.array[,,match(common,namesy),drop=FALSE]
    np <- length(common)
    if (np - ifelse(any(common=="deviance"),1,0) < 1)
      stop("no common parameters")
  }

  # Add chains from y to x
  z <- x$sims.array
  x$sims.array <- array(NA, dim=c(nx, ncx+ncy, np),
                        dimnames=dimnames(z))
  for (i in 1:np)
    x$sims.array[,,i] <- cbind(z[,,i],y$sims.array[,,i])
  
  # clean up (incl. remove stuff that is wrong)
  x$n.chains <- ncx + ncy
  x$n.keep <- nx
  x$n.sims <- nx * x$n.chains
  if (x$n.thin!=y$n.thin) x$n.thin <- NA
  x$n.iter <- x$n.burn + (x$n.thin * x$n.keep)

  x$sims.list <- NULL
  x$sims.matrix <- NULL
  x$summary <- NULL
  x$mean <- NULL
  x$sd <- NULL
  x$median <- NULL
  x$isDIC <- FALSE
  x$DICbyR <- NULL
  x$pD <- NULL
  x$DIC <- NULL
  x$MCMCerr <- NULL

  # Recompute MCMC chain statistics
  newStats <- makeStats(x$sims.array)
  x$summary <- newStats$summary 
  if (!minimal) {
    x$mean <- newStats$mean 
    x$median <- newStats$median 
    x$sd <- newStats$sd 
    x$MCMCerr <- newStats$MCMCerr
  }

  # Recompute sims.matrix and sims.list as permuted sims.array
  if (!minimal) {
    temp <- makeMatList(x$sims.array)
    x$sims.matrix <- temp$sims.matrix
    x$sims.list <- temp$sims.list
  }

  if (minimal) {
    x$sims.list <- NULL
    x$sims.matrix <- NULL
    x$indexes.short <- NULL
    x$mean <- NULL
    x$sd <- NULL
    x$median <- NULL
  }


  return(x)
}
