#####################################################################################
# Show trace, ar plot, and histogram for one or all parameters of a set of bugs()   #
# results.                                                                          #
# Note that a rube() result is a valid superset of a bugs() results.                #
# Interactive exploration of the posterior components is supported automatically.   #
# Input MUST come from bugs()/jags() objects (including successful rube() objects). #
# Use params= to select just some components.                                       #
# By default the chain used in the case of multiple chains is chain 1.              #
# By default the 95% PI is shown on the histogram/density.  Set to PIprob to NULL   #
#   to suppress.                                                                    #
# Rhat is shown in green/orange/red if it is <1.2, 1.2 to 2, or >2 respectively     #
#   unless rHatGrayZone is changed.                                                 #
# When possible, the distribution name is shown.                                    #
# For large parameters (>nSample) only nSample PI values and acf's are shown, but   #
#   clicking again on the parameter button shows a new random sample.               #
# For saving plots, try noMenu=TRUE with params= set to one parameter.              #
#####################################################################################
#
compare <- function(..., params=NULL, chains=rep(1, length(MCMCRslts)),
               nSample=20, true=NULL, PIprob=0.95,
               rHatGrayZone=c(1.2,2.0), noMenu=FALSE,
               idString=NULL, cex.main=1) {
  # convert "..." to a list and check it
  MCMCRslts <- list(...)
  if (length(MCMCRslts)<2 || !all( sapply(MCMCRslts, is, class2="bugs") ||
                                   sapply(MCMCRslts, is, class2="rjags")))
    stop(gettextf("input must be at least two bugs/rjags (rube) objects"))

  # get names for ... objects
  modNames <- names(MCMCRslts)
  M = length(MCMCRslts)
  if (is.null(modNames)) modNames <- rep("", M)
  temp <- modNames==""
  if (any(temp)) {
    temp2 <- deparse(substitute(list(...)))
    temp3 <- gsub("[[:space:]]", "", strsplit(substring(temp2, 6, nchar(temp2)-1), ",")[[1]])
    modNames[temp] <- temp3[temp]
  }

  # Check input
  if (!is.null(PIprob) && (!is.numeric(PIprob) || PIprob<=0 || PIprob>=1))
    stop(gettextf("'PIprob' must be NULL or in (0,1)"))
  if (!is.null(params) && (!class(params)%in%c("character","numeric","integer") ||
                           length(params)==0)) {
    stop(gettextf("'params' must be NULL or a character or numeric vector"))
  }
  if (!is.numeric(rHatGrayZone) || length(rHatGrayZone)!=2)
    stop(gettextf("'rHatGrayZone' must be a vector of two numbers"))
  rHatGrayZone = c(0, rHatGrayZone)


  # Set chosen chain
  tmp <- length(chains)
  if (tmp!=M && tmp!=1) 
    stop(gettextf("'chains' is of length %d but there are %d elements in 'MCMCRslts'",
                  tmp, M))
  if (tmp<M) chains <- rep(chains, M)
  for (i in 1:M) {
    if (dim(MCMCRslts[[i]]$sims.array)[2]<chains[i])
      stop("bugsRslt number ", i, " has fewer than ", chains[i], " chains")
    MCMCRslts[[i]]$sims.array <- MCMCRslts[[i]]$sims.array[,chains[i],]
  }

  # Get labels
  if (is.null(idString)) idString <- paste(paste(modNames, "=", palette()[1:M], sep=""), collapse=", ")

  # Get Rhat and distribution info
  Rhat <- lapply(MCMCRslts, function(x) {
    if (is.na(match("Rhat",colnames(x$summary)))) {
      return(NULL)
    } else {
      return(as.numeric(x$summary[,"Rhat"]))
    }
  })
  
  # get sims info
  iterNums <- lapply(MCMCRslts, function(x) seq(1, by=x$n.thin, length=x$n.keep))
  iterNs <- sapply(iterNums,length)
  iterMax <- max(sapply(iterNums,max))

  # convert c("a","b[1]","b[2]") to c("a","b","b")
  baseNames <- function(x) sub("\\[[[:digit:]]+[,]*[[:digit:]]*\\]", "", x)

  # Create a list of parameter names common to all results and possibly selected by the user
  pars <- dimnames(MCMCRslts[[1]]$sims.array)[[2]]
  for (i in 2:M) {
    pars <- intersect(pars, dimnames(MCMCRslts[[i]]$sims.array)[[2]])
  }
  if (!is.null(params)) {
    tmp <- setdiff(params, unique(baseNames(pars)))
    if (length(tmp)!=0)
      warning(gettextf("'params' has components not in the 'bugs' objects"))
    pars <- pars[baseNames(pars)%in%params]
  }
  if (length(pars)==0) stop(gettextf("no parameters left to plot"))
  compRslt <- lapply(pars, function(x) {
    rtn <- NULL
    for (i in 1:M) {
      rtn <- c(rtn, list(MCMCRslts[[i]]$sims.array[,x]))
    }
    return(rtn)
  })
  names(compRslt) <- pars
  for (i in 1:M) {
    if (!is.null(Rhat[[i]])) # added 12/11/12
      Rhat[[i]] <- Rhat[[i]][match(pars,dimnames(MCMCRslts[[i]]$sims.array)[[2]])]
  }
  check <- lapply(MCMCRslts, function(x) x$check)
  if (any(sapply(check,is.null))) check <- NULL

  rm(MCMCRslts) # potentially free lots of space

  ## Information about distributions of each parameter
  parsLB <- str_locate(pars,"\\[")[,1]
  Sel <- is.na(parsLB)
  parsBase <- c(pars[Sel], unique(substring(pars[!Sel],1,parsLB[!Sel]-1)))
  distNames <- NULL
  distStrings <- NULL
  if (!is.null(check)) {
    distInfo <- lapply(check, function(x, Pars) {
      tmp <- x$stochs[,1]
      distNames <- names(tmp)
      distStrings <- as.character(tmp)
      Sel <- match(Pars,distNames)
      Sel <- Sel[!is.na(Sel)]
      return(data.frame(I(distNames),I(distStrings))[Sel,])
    }, Pars=parsBase)
    # don't use dist. labels if different models have different dists.
    if (nrow(distInfo[[1]])==0) {
      distStrings <- NULL
    } else {
      OK <- apply(matrix(sapply(distInfo, function(x)x$distStrings), ncol=M),
                  1, function(x)all(x==x[1]))
      distInfo <- lapply(distInfo, function(x, ok) x[ok,], ok=OK)
      if (nrow(distInfo[[1]])>0) {
        distNames <- distInfo[[1]]$distNames
        distStrings <- distInfo[[1]]$distStrings
      }
    }
    rm(distInfo)
  }


  # Get name info
  parNamesAll <- pars
  parNamesRep <- baseNames(parNamesAll)
  parNames <- parNamesRep[!duplicated(parNamesRep)]
  parDescr <- parNames # descriptive, e.g, x[1:5,2:1000]
  if (length(parNames) < length(parNamesAll)) {
    vecArr <- unique(parNamesRep[duplicated(parNamesRep)])
    colon <- function(x) {
      if (x[1]==x[2]) return(paste(x[1]))
      return(paste(x[1], ":", x[2], sep=""))
    }
    for (v in vecArr) {
      tmp <- parNamesAll[parNamesRep==v]
      tmp <- substring(tmp, nchar(v)+2, nchar(tmp)-1)
      Dim <- length(strsplit(tmp[1], ",")[[1]])
      if (Dim==1) {
        rng <- try(range(suppressWarnings(as.numeric(unlist(tmp)))), silent=TRUE)
        if (!is(rng, "try-error"))
          parDescr[match(v,parDescr)] <- paste(v, "[", colon(rng), "]", sep="")
      } else {
        tmp <- strsplit(tmp, ",")
        tmp <- suppressWarnings(as.numeric(unlist(tmp)))
        if (!any(is.na(tmp))) {
          tmp <- apply(matrix(tmp,Dim),1,range)
          tmp <- apply(tmp,2, colon)
          tmp <- paste(tmp, collapse=", ")
          parDescr[match(v,parDescr)] <- 
             paste(v, "[", tmp, "]", sep="")          
        }
      }   
    }
  }
  # Annotate parameters in titles with distributions if known
  parNamesAllDist <- parNamesAll
  parNamesDist <- parNames
  if (!is.null(distStrings)) {
    tmp <- match(parNamesRep, distNames)
    OK <- !is.na(tmp)
    if (any(OK)) 
      parNamesAllDist[OK] <- paste(parNamesAllDist[OK], distStrings[tmp[OK]], sep=" ~ ")
    tmp <- match(parNames, distNames)
    OK <- !is.na(tmp)
    if (any(OK)) 
      parNamesDist[OK] <- paste(parNamesDist[OK], distStrings[tmp[OK]], sep=" ~ ")
  }
  nPar <- length(parNames)

  # Get first-parameter-to-display info
  pNum <- 1
  thisPar <- parNames[[pNum]]
  thisElements <- which(thisPar==parNamesRep)
  thisDim <- length(thisElements)
  if (nPar==1 && thisDim==1) noMenu <- TRUE
  if (thisDim==1) {
    element <- 1
    index <- thisElements[element]
  } else {
    element <- 0
  }
  
  # Setup plot
  oldpar <- par(c("mar","oma","mfrow"))
  on.exit(par(oldpar))
  options(locatorBell=FALSE)
  par(oma=c(0.3,0.3,3,0.3))
  marStd <- c(4.1,4.1,2.1,1.1)
  marMenu <- c(1,1,0,1)
  menuFrac <- 0.2

  # Set button text
  navButtons <- c("-10","-1","Previous","Quit","Next","+1","+10")
  if (nPar>18) {
      third <- ceiling(nPar/3)
      twothirds <- ceiling(2*nPar/3)
      buttonText <- list(parDescr[1:third],
                         parDescr[(third+1):(twothirds)],
                         parDescr[(twothirds+1):nPar],
                         navButtons)
      navRow <- 4
      border <- 0.1
  } else if (nPar>5) {
      half <- ceiling(nPar/2)
      buttonText <- list(parDescr[1:half],
                         parDescr[(half+1):nPar],
                         navButtons)
      navRow <- 3
      border <- 0.2
  } else {
    buttonText <- list(parDescr, navButtons)
    navRow <- 2
    border <- 0.3
  }
  noGreyButtons <- lapply(buttonText, function(x) rep(FALSE, length(x)))

  # Try to fix slightly jumping menu boxes
  spacingHack <- function(x) {
    x <- round(x,2)
    x[1] <- 1 - sum(x[-1])
    return(x)
  }

  # Function to plot set of parameters
  plotOverview <- function() {
    nPlot <- ifelse(is.null(Rhat[[1]]),2,3)
    if (nPlot==3) {
      wdths <- c(0.44, 0.28, 0.28)
    } else {
      wdths <- c(0.64, 0.36)
    }
    if (noMenu) {
      layout(matrix(1:nPlot, nrow=1), widths=wdths)
    } else {
      layout(rbind(1:nPlot, rep(nPlot+1,nPlot)), 
             heights=spacingHack(c(1-menuFrac, menuFrac)),
             widths=wdths)
    }
    par(mar=marStd)

    nShow <- min(thisDim,nSample)
    Sel <- sort(sample(thisDim, nShow))
    y <- y[Sel]
    # Posterior intervals
    tmp <- lapply(y, function(x) {
             sapply(x, quantile, prob=c((1-PIprob)/2, (1+PIprob)/2))})
    tmpm <- sapply(y, function(x) {sapply(x, mean)})
    plot(NA, type="n", xlim=c(min(unlist(tmp)), max(unlist(tmp))),
         ylim=c(0.6, nShow+0.4), axes=F,
         xlab="", ylab="", main=paste(100*PIprob,"% Post. Int.",sep=""))
    title(xlab=thisPar, line=2)
    box(); axis(1)
    axis(2, at=1:nShow, labels=substring(parNamesAll[parNamesRep==thisPar][rev(Sel)],
                                         nchar(thisPar)+1), las=1)
    delta <- min(0.15, nShow/2/M)
    Mid <- (M+1)/2
    for (i in 1:nShow) {
      for (j in 1:M) {
        segments(tmp[[i]][1,j], nShow-i+1+(j-Mid)*delta, 
                 tmp[[i]][2,j], nShow-i+1+(j-Mid)*delta, col=j)
      }
    }
    for (i in 1:nShow) {
      for (j in 1:M) {
        segments(tmpm[j,i], nShow-i+1+(j-Mid)*delta+delta/4, 
                 tmpm[j,i], nShow-i+1+(j-Mid)*delta-delta/4, col=j)
      }
    }
    mtext(paste(parNamesDist[pNum],"  (",idString,")",sep=""), outer=T, line=+0.4, cex=1.35*cex.main)

    # acf
    Sig <- qnorm(0.975)/sqrt(min(iterNs))
    tmp <- sapply(y, function(z) {
                sapply(z, function(x) {
                  tmp <- acf(x, plot=FALSE)$acf[-1]
                  tmp2 <- which(tmp<Sig)
                  if (!any(tmp2)) return(length(tmp))
                  return(max(0.2, min(tmp2)-1))})
           })
    maxLag <- max(tmp)
    plot(NA, type="n", xlim=c(0, 10*log10(min(iterNs))),
         ylim=c(0.6, nShow+0.4), axes=F,
         xlab="", ylab="", main="Signif. Autocor. Lags")
    title(xlab="Lag", line=2)
    box()
    axis(1)
    axis(2, at=1:nShow, labels=substring(parNamesAll[parNamesRep==thisPar][rev(Sel)],
                                         nchar(thisPar)+1), las=1)
    for (i in 1:nShow) {
      for (j in 1:M) {
        segments(0, nShow-i+1+(j-Mid)*delta, 
                 tmp[j,i], nShow-i+1+(j-Mid)*delta, col=j)
      }
    }

    # rHats
    if (!is.null(Rhat[[1]])) {
      rHatMax <- 5
      rhat <- sapply(Rhat, function(x) if (is.null(x)) rep(NA,sum(parNamesRep==thisPar)) else x[parNamesRep==thisPar])
      maxHat <- max(rhat, na.rm=TRUE)
      plot(NA, type="n", xlim=c(0, rHatMax),
           ylim=c(0.6, thisDim+0.4), axes=F,
           xlab="", ylab="", main="rHat")
      title(xlab="Rhat", line=2)
      box()
      axis(1)
      if (thisDim<30) {
        axis(2, at=1:thisDim, labels=substring(parNamesAll[parNamesRep==thisPar],
                                             nchar(thisPar)+1), las=1)
      } 
      for (i in 1:thisDim) {
        for (j in 1:M) {
          if (!is.null(Rhat[[j]]))
           segments(0, thisDim-i+1+(j-Mid)*delta, rhat[i,j], thisDim-i+1+(j-Mid)*delta, 
                    col=j)
        }
      }
      rect(rHatGrayZone[1],0,rHatGrayZone[2],0.4,density=-1,col="green3",border=NA)
      rect(rHatGrayZone[2],0,rHatGrayZone[3],0.4,density=-1,col="orange",border=NA)
      rect(rHatGrayZone[3],0,rHatMax,0.4,density=-1,col="red",border=NA)
    }

    # Setup menu area
    if (!noMenu) {
      par(mar=marMenu)
      plot(c(0,1), c(0,1), type="n", xlab="", ylab="", axes=F)
    }
    invisible(NULL)
  }

  # Function to plot individual parameter
  plot3 <- function() {
    nPlot <- 3
    if (noMenu) {
      layout(matrix(1:nPlot, ncol=1))
    } else {
      layout(matrix(1:(nPlot+1), ncol=1), 
             heights=spacingHack(c(rep((1-menuFrac)/nPlot,nPlot),menuFrac)))
    }
    par(mar=marStd)

    # trace plot(s):
    plot(iterNums[[1]], y[[1]], type="l", xlab="", ylab=thisPar, ylim=range(y), xlim=c(1,iterMax))
    title(xlab="iteration number", line=2)
    for (i in 2:M) lines(iterNums[[i]], y[[i]], col=i)
    mtext(paste(parNamesAllDist[index],"  (",idString,")",sep=""), outer=T, line=+0.4, cex=1.35*cex.main)
    rhat <- sapply(Rhat, function(x) if (is.null(x)) NA else x[index])
    rhat <- rhat[!is.na(rhat)]
    if (length(rhat)>0) {
      legend("topright", paste(paste("Rhat=",format(rhat,digits=3),sep=""), collapse="  "),
             bty="n", text.col=c("green3","orange","red")[findInterval(rhat,rHatGrayZone)])
    }

    # acf(s):
    tmp <- lapply(y, function(x) {
              if (var(x)==0) {
                lag.max <- floor(10 * (log10(length(x))))
                return(list(lag=0:lag.max, acf=c(1,rep(0,lag.max))))
              }
              return(acf(x, plot=FALSE))
           })
    rng <- sapply(tmp, function(x)range(x$acf))
    plot(NA, type="n", xlim=c(0,length(tmp[[1]]$lag)-1), ylim=range(0,rng),
         xlab="", ylab="ACF", main="")
    title(xlab="Lag", line=2)
    abline(h=0)
    abline(h=c(-1,1)*qnorm(0.975)/sqrt(min(iterNs)), col="blue", lty="dashed")
    for (i in 1:M) {
      lag <- as.numeric(tmp[[i]]$lag)
      ac <- as.numeric(tmp[[i]]$acf)
      d <- 0.15*(i-1)
      segments(lag+d, rep(0,length(lag)), lag+d, ac, col=i)
    }

    # hist/density plots:
    tmp <- lapply(y, density)
    tmpx <- sapply(tmp, function(o) range(o$x))
    tmpy <- sapply(tmp, function(o) range(o$y))
    plot(NA, type="n", xlim=range(tmpx), ylim=range(tmpy),
         xlab="", ylab="density", main="")
    title(xlab=parNamesAll[index], line=2)
    for (i in 1:M) lines(tmp[[i]], col=i) 
    if (!is.null(PIprob)) {
      tmp <- sapply(y, quantile, 
                    probs=c((1-PIprob)/2, 1-(1-PIprob)/2))
      tmpm <- sapply(y, mean)
      for (i in 1:M) 
        abline(v=c(tmp[,i],tmpm[i]), col=i, lwd=c(1,1,2))
    }
    if (!is.null(true)&&length(true)>=pNum) abline(v=true[pNum],col=1, lwd=3, lty=3)

    # Setup menu area
    if (!noMenu) {
      par(mar=marMenu)
      plot(c(0,1), c(0,1), type="n", xlab="", ylab="", axes=F)
    }
    invisible(NULL)
  }


  # Show posterior plot(s) until user quits
  allDone <- FALSE
  lastPar <- thisPar
  lastDim <- 0
  while (!allDone) {
    if (element==0) { 
      y <- compRslt[which(parNamesRep==thisPar)]
      nShow <- min(thisDim,nSample)
      if (thisDim<nSample) {
        thisSubset <- 1:nShow
      } else {
        if (thisPar==lastPar || thisDim!=lastDim)
          thisSubset <- sort(sample(thisDim, nShow))
      }
      plotOverview()
    } else {
      y <- compRslt[[index]]
      plot3()
    }

    # Display input panel (menu), wait for click, and then dispatch
    if (noMenu) break
    greyButtons <- noGreyButtons
    if (element<=1) greyButtons[[navRow]][1:2] <- TRUE # -10/-1
    if (pNum==1) greyButtons[[navRow]][3] <- TRUE # Previous
    if (pNum==nPar) greyButtons[[navRow]][5] <- TRUE # Next
    if (thisDim==1 || (thisDim>1 && element==thisDim)) greyButtons[[navRow]][6:7] <- TRUE # +1/+10
    click <- buttonInput(buttonText, greyButtons=greyButtons, draw=TRUE, borderGuard=border)
    if (is.null(click)) click <- "Quit"
    id <- match(click, unlist(buttonText))
    if (click == "Quit") { # remove buttons
      u=par("usr")
      rect(u[1], u[3], u[2], u[4], col="white", border="white", xpd=NA, pty="m")
      break
    }
    lastPar <- thisPar
    lastDim <- thisDim
    if (click=="Previous") {
      pNum <- pNum - 1
      thisPar <- parNames[[pNum]]
      thisElements <- which(thisPar==parNamesRep)
      thisDim <- length(thisElements)
      element <- ifelse(thisDim==1, 1, 0)
    } else if (click=="Next") {
       pNum <- pNum + 1
       thisPar <- parNames[[pNum]]
       thisElements <- which(thisPar==parNamesRep)
       thisDim <- length(thisElements)
       element <- ifelse(thisDim==1, 1, 0)
    } else if (click=="-1") {
      element <- element - 1
    } else if (click=="+1") {
      element <- element + 1
    } else if (click=="-10") {
      element <- max(1, element - 10)
    } else if (click=="+10") {
      element <- min(thisDim, element + 10)
    } else {
      pNum <- id
      thisPar <- parNames[[pNum]]
      thisElements <- which(thisPar==parNamesRep)
      thisDim <- length(thisElements)
      element <- ifelse(thisDim==1, 1, 0)
    }
    if (element>0) index <- thisElements[element]
  }  # end while viewing plots

  #if (!is.null(oldpar)) par(oldpar)
  
  invisible(NULL)
}

