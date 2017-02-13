#############################################################################
# Take the sims.array object and return a list containing the corresponding #
# summary, mean, median, sd, and MCMCerr results.                           #
#############################################################################
#
makeStats <- function(simsArr) {
  if (!is.array(simsArr) || length(dim(simsArr))!=3 ||
      is.null(dimnames(simsArr)) || is.null(dimnames(simsArr)[[3]]))
    stop("'simsArr' must be a bugs sims.array object")
  DN3 <- DN3bare <- dimnames(simsArr)[[3]]
  if (length(grep(",.+,", DN3))>0)
    stop("sorry -- arrays greater than 1 dimensional are not yet programmed")

  # Get chain info
  objDim <- dim(simsArr)
  n.chains <- objDim[2]
  n.iter <- objDim[1]

  # Get variable info
  n.varLong <- objDim[3]
  temp <- regexpr("[[]", DN3)
  if (sum(temp>0)==n.varLong) {
    varNamesAll <- varNamesScalar <- NULL
  } else {
    varNamesAll <- varNamesScalar <- DN3[temp==-1]
  }
  if (length(temp)==0) {
    varNamesVector <- NULL
  } else {
    varNamesVector <- substring(DN3[temp>0], 1, temp[temp>0]-1)
    DN3bare[temp>0] <- varNamesVector
    varNamesVector <- unique(varNamesVector)
  }
  varNamesAll <- unique(DN3bare)

  # Convert array to sims.list
  pieces <- paste("simsArr[,", 1:n.chains, ",]", sep="")
  whole <- paste("mcmc.list(",
                 paste("mcmc(", pieces, ")", sep="", collapse=", "),
                 ")")
  codaForm <- eval(parse(text=whole))

  # Compute Rhat
  # One big call to gelman.diag should work, but if any param. fails, all fails
  if (n.chains==1) {
    Rhat <- NULL
  } else {
    Rhat <- rep(NA, n.varLong)
    for (i in 1:n.varLong) {
      pieces <- paste("simsArr[,", 1:n.chains, ",", i, "]", sep="")
      whole <- paste("mcmc.list(",
                   paste("mcmc(", pieces, ")", sep="", collapse=", "),
                   ")")
      temp <- try(gelman.diag(eval(parse(text=whole)),
                              autoburnin=FALSE)$psrf[,"Point est."],
                  silent=TRUE)
      if (!is(temp, "try-error")) Rhat[i] <- temp
    }
  }

  # modification to summary.list.mcmc that fixes error/warning problems
  statnames <- c("mean", "sd", "MCMCerr")
  varstats <- matrix(nrow = n.varLong, ncol = length(statnames), 
                     dimnames = list(varnames(codaForm), statnames))
  xtsvar <- matrix(nrow = n.chains, ncol = n.varLong)
  if (is.matrix(codaForm[[1]])) {
    for (i in 1:n.chains) {
      for (j in 1:n.varLong) {
        temp <- try(suppressWarnings(spectrum0(codaForm[[i]][, j])), silent=TRUE)
        if (is(temp, "try-error")) {
          xtsvar[i,j] <- NA
        } else {
          xtsvar[i,j] <- temp$spec
        }
      }
    }
    xlong <- do.call("rbind", codaForm)
  } else {
    for (i in 1:n.chains) {
      temp <- try(suppressWarnings(spectrum0(codaForm[[i]])), silent=TRUE)
      if (is(temp, "try-error")) {
        xtsvar[i,] <- NA
      } else {
        xtsvar[i,] <- temp$spec
      }
    }
    xlong <- as.matrix(codaForm)
  }
  xmean <- apply(xlong, 2, mean)
  xmedian <- apply(xlong, 2, mean)
  xvar <- apply(xlong, 2, var)
  xtsvar <- apply(xtsvar, 2, mean)
  varquant <- t(apply(xlong, 2, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975)))
  varstats[, 1] <- xmean
  varstats[, 2] <- sqrt(xvar)
  varstats[, 3] <- sqrt(xtsvar/(n.iter * n.chains))
  newQuant <- drop(varquant)
  newMeanSD <- drop(varstats[,c("mean","sd")])
  newEffSize <- effectiveSize(codaForm)
  if (is.matrix(newMeanSD)) {
    newSummary <- cbind(newMeanSD, newQuant)
  } else {
    newSummary <- t(as.matrix(c(newMeanSD, newQuant)))
    rownames(newSummary) <- DN3bare
  }
  if (!is.null(Rhat))
    newSummary <- cbind(newSummary, Rhat=Rhat)
  newSummary <- cbind(newSummary, n.eff=floor(newEffSize)) # Does not match bugs()

  # Compute mean, median, sd in $summary format
  meanNew <- medianNew <- sdNew <- vector("list", length(varNamesAll))
  names(meanNew) <- names(medianNew) <- names(sdNew) <- varNamesAll
  for (i in seq(along=varNamesAll)) {
    newVar <- varNamesAll[i]
    for (heading in c("mean","median","sd")) {
      colName <- ifelse(heading=="median", "50%", heading)
      if (newVar %in% varNamesScalar) {
        text <- paste(heading,"New[['",newVar,"']] = newSummary[DN3==newVar,colName]", sep="")
        eval(parse(text=text))
      } else {
        temp <- substring(DN3,1,nchar(newVar)+1) ==
                paste(newVar,"[",sep="")
        text=paste(heading,
                   "New[['",newVar,"']] = array(as.numeric(newSummary[DN3bare==newVar,colName]), sum(temp))",
                   sep="")
        eval(parse(text=text))
      }
    }
  }
  return(list(summary=newSummary, mean=meanNew, median=medianNew, 
              sd=sdNew, MCMCerr=varstats[,"MCMCerr"]))
}
