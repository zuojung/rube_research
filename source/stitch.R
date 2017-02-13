# Stitch two consecutive rube runs into one
# Note: 'rubeResultX' may be the filename of a rube object stored using dput().
stitch <- function(rubeResult1, rubeResult2, minimal=FALSE) {
  if (!is(rubeResult1,"rube") || (is.character(rubeResult1) && length(rubeResult1)==1)) 
    stop("'rubeResult1' must be a filename or rube result")
  if (!is(rubeResult2,"rube") || (is.character(rubeResult2) && length(rubeResult2)==1)) 
    stop("'rubeResult2' must be a filename or rube result")
  if (is.character(rubeResult1)) {
    if (is.na(match(rubeResult1, list.files()))) 
      stop("can't find file: ", rubeResult1)
  } else {
    if (!is(rubeResult1, "bugs") && !is(rubeResult1, "rjags"))
      stop("failed bugs/jags run for ", deparse(substitute(rubeResult1)))
  }
  if (is.character(rubeResult2)) {
    if (is.na(match(rubeResult2, list.files()))) 
      stop("can't find file: ", rubeResult2)
  } else {
    if (!is(rubeResult2, "bugs") && !is(rubeResult2, "rjags"))
      stop("failed bugs/jags run for ", deparse(substitute(rubeResult2)))
  }

  # handle object #1
  nam <- deparse(substitute(rubeResult1))
  if (is.character(rubeResult1)) {
    rubeResult1 <- try(dget(rubeResult1), silent=TRUE)
    if (is(rubeResult1, "try-error")) 
      stop("File ", nam, " was not created with dput()")
    if (!is(rubeResult1, "rube")) 
      stop("Object ", nam, " does not contain a rube object")
    if (!is(rubeResult1, "bugs") && !is(rubeResult1, "rjags")) 
      stop("the WinBUGS/jags run failed for rube object ", nam)
  }
  nc <- rubeResult1$n.chains
  n <- rubeResult1$n.keep
  ni <- rubeResult1$n.iter
  nt <- rubeResult1$n.thin
  nb <- rubeResult1$n.burnin
  ns <- rubeResult1$n.sims
  parms <- dimnames(rubeResult1$sims.array)[[3]]
  parmsBase <- parms
  leftBracket <- str_locate(parmsBase,  "\\[")[,1]
  hasBracket <- !is.na(leftBracket)
  parmsBase[hasBracket] <- substring(parmsBase[hasBracket],1,leftBracket[hasBracket]-1)
  np <- length(parms)
  rubeResult1$sims.list <- NULL
  rubeResult1$sims.matrix <- NULL
  rubeResult1$n.keep <- NULL
  rubeResult1$n.iter <- NA
  rubeResult1$n.sims <- NA
  rubeResult1$summary <- NULL
  rubeResult1$mean <- NULL
  rubeResult1$sd <- NULL
  rubeResult1$median <- NULL
  rubeResult1$root.short <- NULL
  rubeResult1$long.short <- NULL
  rubeResult1$dimension.short <- NULL
  rubeResult1$indexes.short <- NULL
  rubeResult1$last.values <- NULL
  rubeResult1$isDIC <- FALSE
  rubeResult1$DICbyR <- FALSE
  rubeResult1$pD <- NULL
  rubeResult1$DIC <- NULL
  rubeResult1$MCMCerr <- NULL
  ## Keep unchanged
  ## rubeResult1$bugs.seed
  ## rubeResult1$check
  ## rubeResult1$n.thin
  ## rubeResult1$n.burnin
  ## rubeResult1$startTime
  ## rubeResult1$runTime

  # stitch second object to the end of the previous one
  gc() # ?? overcome dget() bug??
  nam2 <- deparse(substitute(rubeResult2))
  if (is.character(rubeResult2)) {
    rubeResult2 <- try(dget(rubeResult2), silent=TRUE)
    if (is(rubeResult2, "try-error")) 
      stop("File ", nam2, " was not created with dput()")
    if (!is(rubeResult2, "rube")) 
      stop("Object ", nam2, " does not contain not a rube object")
    if (!is(rubeResult2, "bugs")) 
      stop("the WinBUGS run failed for rube run in ", nam2)
  }
  if (length(rubeResult1$model)!=length(rubeResult2$model)) stop("models differ")
  if (any(rubeResult1$model!=rubeResult2$model)) stop("models differ")
  if (!(is.null(rubeResult1$engine) || is.null(rubeResult2$engine)) &&
      (rubeResult1$engine != rubeResult2$engine))
    stop("rube() results were generated with different MCMC engines")
  n2 <- rubeResult2$n.keep
  ni2 <- rubeResult2$n.iter
  nt2 <- rubeResult2$n.thin
  nb2 <- rubeResult2$n.burnin
  ns2 <- rubeResult2$n.sims
  nc2 <- rubeResult2$n.chains
  if (nb2!=0) {
    rubeResult1$n.burnin = NA
    warning("stitch() expects n.burnin=0 for second run")
  }
  if (nc!=nc2) stop("number of chains differs: ", nc, " vs. ", nc2)
  if (nt!=nt2) {
    rubeResult1$n.thin = NA
    warning("thinning differs: ", nt, " vs. ", nt2)
  }
  rubeResult1$n.keep <- n + n2
  rubeResult1$n.iter <- ni + ni2
  rubeResult1$n.sims <- ns + ns2
  rubeResult1$startTime <- c(rubeResult1$startTime, rubeResult2$startTime)
  rubeResult1$runTime <- list(rubeResult1$runTime, rubeResult2$runTime)

  parms2 <- dimnames(rubeResult2$sims.array)[[3]]
  parms2Base <- parms2
  leftBracket <- str_locate(parms2Base,  "\\[")[,1]
  hasBracket <- !is.na(leftBracket)
  parms2Base[hasBracket] <- substring(parms2Base[hasBracket],1,leftBracket[hasBracket]-1)
  isect <- intersect(parms, parms2)
  if (length(isect)==0) stop("these two runs have no parameters in common")
  if (length(isect)<length(parms)) {
    parms <- isect
    np <- length(parms)
    rubeResult1$sims.array <- rubeResult1$sims.array[,,parms]
    rubeResult2$sims.array <- rubeResult2$sims.array[,,parms]
  }

  dm <- dim(rubeResult1$sims.array)
  tmp <- rubeResult1$sims.array
  rubeResult1$sims.array <- array(NA, c(n+n2, nc, np))
  dimnames(rubeResult1$sims.array) <- list(NULL, NULL, dimnames(tmp)[[3]])
  for (i in 1:np) {
   for (ch in 1:nc) {
      rubeResult1$sims.array[,ch,i] <- c(tmp[,ch,i], rubeResult2$sims.array[,ch,i])
    }
  }
  
  # Recompute MCMC chain statistics
  newStats <- makeStats(rubeResult1$sims.array)
  rubeResult1$summary <- newStats$summary 
  if (!minimal) {
    rubeResult1$mean <- newStats$mean 
    rubeResult1$median <- newStats$median 
    rubeResult1$sd <- newStats$sd 
    rubeResult1$MCMCerr <- newStats$MCMCerr
  }

  # Recompute sims.matrix and sims.list as permuted sims.array
  if (!minimal) {
    temp <- makeMatList(rubeResult1$sims.array)
    rubeResult1$sims.matrix <- temp$sims.matrix
    rubeResult1$sims.list <- temp$sims.list
  }

  return(rubeResult1)
}
