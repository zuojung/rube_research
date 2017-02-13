1# Shrink a rube object by dropping variables and/or thinning the chains and/or
# dropping initial iterations (i.e., longer burn-in).
# INPUT:
#   'n.thin' is the new thinning value (1 is no further thinning)
#   'drop' is the number of initial kept iterations to drop (before any new thinning)
#   'chainsDropped' is a vector of chain numbers to drop
#   'ignore' is a vector of parameter names (without []s) to drop from the OUTPUT rube object
#   'keep' is a vector of parameter names (without []s) to kee in the OUTPUT rube object
#   'minimal' drops sims.list, sims.matrix, indexes.short, mean, sd, and median
# OUTPUT:
#   the new smaller rube object with the "summary" and related components updated
#   to describe the new data subset.
#
shrink <- function(rubeObject, n.thin=1, drop=0, chainsDropped=NULL, 
                   ignore=NULL, keep=NULL, minimal=FALSE) {
  isRube <- is(rubeObject, "rube")
  isFname <- is(rubeObject, "character")
  if (any(!isRube && !isFname))
    stop("'rubeObject' must be a rube object or dput() filename only")
  if (isFname) {
    isFile <- match(rubeObject, list.files())
    if (is.na(isFile))
      stop("can't find file: ", rubeObject)
  } else {
    isBugs <- is(rubeObject, "bugs") || is(rubeObject,"rjags")
    if (!isBugs) 
      stop("failed MCMC or MCMC not attempted for for the rubeObject")
  }

  # Get object
  if (!isRube) {
    fname <- rubeObject
    rubeObject <- try(dget(rubeObject), silent=TRUE)
    if (is(rubeObject, "try-error")) 
      stop("file ", fname, " was not created with dput()")
    if (!is(rubeObject, "rube")) 
      stop("object ", fname, " is not a rube object")
    if (!is(rubeObject, "bugs") || !is(rubeObject, "rjags")) 
      stop("failed MCMC or MCMC not attempted for for the rubeObject")
  }

  # Convenience variables
  nc <- rubeObject$n.chains
  n <- rubeObject$n.keep

  # Drop undesired chains
  if (!is.null(chainsDropped)) {
    if (!is.numeric(chainsDropped)) stop("'chainsDropped' must be numeric")
    chainsDropped <- round(chainsDropped)
    if (length(chainsDropped)!=length(unique(chainsDropped)))
      stop("'chainsDropped' must be unique integers")
    if (any(chainsDropped<1 | chainsDropped>nc))
      stop("'chainsDropped' must be in 1:rubeObject$n.chains")
    if (length(chainsDropped)==nc)
      stop("cannot drop all chains")
    chainsKept <- setdiff(1:nc, chainsDropped)
    rubeObject$n.chains<- length(chainsKept)
    nc <- rubeObject$n.chains
    rubeObject$sims.array <- rubeObject$sims.array[,chainsKept,,drop=FALSE]
    # $sims.matrix and $sims.list are intended to be permuted and are dealt with below
  }

  # prepare to remove un-desired parameters
  if (is.null(ignore) && is.null(keep)) keep <- rubeObject$root.short
  if (!is.null(ignore)) {
    temp <- match(ignore, rubeObject$root.short)
    if (any(is.na(temp))) {
      stop(paste(ignore[is.na(temp)],collapse=", "), " is not in ",
                 deparse(substitute(rubeObject)))
    }
    keep <- setdiff(keep, ignore)
  }
  if (length(keep)==0) stop("no variables kept")

  # 'bugs' object structure (partial):
  #   $sims.array: array of MCMC simulated values (nIterKept x nChains x nParam)
  #   $sims.matrix: matrix of MCMC simulated values (nIterKept x nParam)
  #     colnames() includes "[#+]" where needed
  #     order of parameters matches $root.short but not long.short
  #   Note:  all(dimnames(rubeObject$sims.array)[[3]]==
  #              colnames(rubeObject$sims.matrix))  is TRUE
  #   $sims.list: list of MCMC simulated values (length=length($root.short))
  #     named the same as $root.short.
  #     Each element is either a (nIterKept x nElements) matrix or a
  #     vector of nIterKept MCMC simulation values.
  #   $root.short: vector of basenames of saved parameters (no [])
  #   $long.short: list of vectors matching $root.short, holding numbers
  #     matching 1:ncol($sims.matrix) in appropriate sized vectors matching
  #     the lengths of the elements of $root.short, but not matching actual
  #     parameter positions in any of the $sims.xxx bugs() elements (???!!!)
  #     positions of all elements of the corresponding parameters
  #   $dimension.short: vector of length matching $root.short with '0' for
  #     vector parameters, and '1' for scalar parameters
  #   $indexes.short: list of vectors matching $root.short, holding NULL when
  #     the correponding parameter is scalar, and a list of single element 
  #     vectors containing parameter position numbers (typically 1:length(parameter))
  #     where the parameter is a vector.

  # remove undesired parameters, if any
  if (length(keep)<length(rubeObject$root.short)) {
    temp <- match(keep, rubeObject$root.short)
    if (any(is.na(temp))) {
      stop(paste(keep[is.na(temp)],collapse=", "), " is not in ",
                 deparse(substitute(rubeObject)))
    }
  
    ignore <- setdiff(rubeObject$root.short, keep)
    if (length(ignore)>0) {
      igLoc <- match(ignore, rubeObject$root.short)
      lengths <- sapply(rubeObject$sims.list, function(x) ifelse(is.matrix(x),ncol(x),1))
      igLocN <- rep(c(FALSE,TRUE)[1+rubeObject$root.short%in%keep], lengths)
      rubeObject$sims.array <- rubeObject$sims.array[,,igLocN]
      # $sims.matrix and $sims.list are indended to be permuted and are redefined below
      rubeObject$root.short <- rubeObject$root.short[-igLoc]
      rubeObject$long.short <- NULL
      rubeObject$dimension.short <- rubeObject$dimension.short[-igLoc]
      if (minimal) {
        rubeObject$indexes.short <- NULL
      } else {
        rubeObject$indexes.short <- rubeObject$indexes.short[-igLoc]
      }
    }
  }

  # Deal with 'drop' and 'n.thin'
  # Notes: n.burnin counts all iterations in one chain regardless of n.thin
  #        n.iter works like n.burnin and counts burnin and post-burnin
  #        n.keep counts all post-burnin iterations after thinning for one chain
  #        n.sims counts all post-burnin iterations after thinning for all chains
  # rubeObject$n.iter is unchanged: it is the total iterations run (per chain)
  if (drop!=0 || n.thin!=1) {
    if (drop!=round(drop) || n.thin!=round(n.thin))
      stop("'drop' and 'n.thin' must be integers")
    if (drop<0 || drop>rubeObject$n.keep-5)
      stop("'drop' is too large; you must keep at least 5 iterations")
    iterKept <- seq(drop+1, rubeObject$n.keep, n.thin)
    rubeObject$n.keep <- length(iterKept)
    if (rubeObject$n.keep<5)
      stop("'n.thin' and 'drop' together leave less than 5 iterations")
    rubeObject$n.burnin <- rubeObject$n.burnin + drop*rubeObject$n.thin
    rubeObject$n.sims <- rubeObject$n.keep * rubeObject$n.chains
    rubeObject$n.thin <- rubeObject$n.thin * n.thin
    rubeObject$matrix <- NULL
    rubeObject$list <- NULL
    rubeObject$summary <- NULL
    rubeObject$mean <- NULL
    rubeObject$sd <- NULL
    rubeObject$median <- NULL
    rubeObject$MCMCerr <- NULL
    rubeObject$sims.array <- rubeObject$sims.array[iterKept,,,drop=FALSE]
  }

  # Recompute MCMC chain statistics
  #rubeObject$summary <- R2WinBUGS:::monitor(rubeObject$sims.array, keep.all=TRUE)
  newStats <- makeStats(rubeObject$sims.array)
  rubeObject$summary <- newStats$summary 
  if (!minimal) {
    rubeObject$mean <- newStats$mean 
    rubeObject$median <- newStats$median 
    rubeObject$sd <- newStats$sd 
    rubeObject$MCMCerr <- newStats$MCMCerr
  }

  # Recompute sims.matrix and sims.list as permuted sims.array
  if (!minimal) {
    temp <- makeMatList(rubeObject$sims.array)
    rubeObject$sims.matrix <- temp$sims.matrix
    rubeObject$sims.list <- temp$sims.list
  }

  if (minimal) {
    rubeObject$sims.list <- NULL
    rubeObject$sims.matrix <- NULL
    rubeObject$indexes.short <- NULL
    rubeObject$mean <- NULL
    rubeObject$sd <- NULL
    rubeObject$median <- NULL
  }

  return(rubeObject)
}

