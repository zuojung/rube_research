####################################################################################
# Show trace, ar plot, and histogram for one or all parameters in a bugs() or      #
# jags() result.                                                                   #
# Note that a rube() result is a valid superset of a bugs() or jags() result.      #
# Single or multiple chains are allowed.                                           #
# Interactive exploration of the posterior components is supported automatically.  #
# Input MUST come from the $sims.array component of a bugs() object because        #
#   $sims.matrix and $sims.list are randomly permuted!!!   Use either the rube()   #
#   object or its $sims.array component.                                           #
# Use params= to select just some components.                                      #
# If n.chains is 1, panel 3 shows a histogram, otherwise density plots.            #
# The first "drop" iterations are dropped.                                         #
# By default the 95% PI is shown on the histogram/density.  Set to PIprob to NULL  #
#   to suppress.                                                                   #
# Rhat is shown in green/orange/red if it is <1.2, 1.2 to 2, or >2 respectively    #
#   unless rHatGrayZone is changed.                                                #
# When possible, the distribution name is shown in the title.                      #
# For parameters with more than >nSample element, only nSample PI values and acf's #
# are shown, but clicking again on the parameter button shows a new random sample. #
# For saving plots, try noMenu=TRUE with params= set to one parameter.             #
# 'add' is a names list of computed values to add to the list of traces viewed.    #
# The computation is an expression or string using existing parameter names and is #
# currently restricted to non-vector parameters.                                   #
####################################################################################
#
p3 <- function(rubeResult, params=NULL, only=NULL, drop=0, add=NULL,
               nSample=20, true=NULL, viewGroups=NULL,
               LCzero=TRUE, PIprob=0.95, rHatGrayZone=c(1.2,2.0), noMenu=FALSE) {
  # Check input
  if (is(rubeResult,"rube") && !(is(rubeResult,"bugs") || is(rubeResult,"rjags")))
    stop("WinBUGS/jags failed or was not attempted; try summary() on your rube() output")
  if (!is.null(PIprob) && (!is.numeric(PIprob) || PIprob<=0 || PIprob>=1))
    stop("PIprob must be NULL or in (0,1)")
  if (!is.null(params) && (!class(params)%in%c("character","numeric","integer") ||
                           length(params)==0)) {
    stop("params must be NULL or a character or numeric vector")
  }
  if (!is.numeric(rHatGrayZone) || length(rHatGrayZone)!=2)
    stop("rHatGrayZone must be a vector of two numbers")
  rHatGrayZone = c(0, rHatGrayZone)

  # Get chain info
  objDim <- dim(rubeResult$sims.array)
  n.chains <- objDim[2]
  n.iter <- objDim[1]

  # Handle 'add' expressions
  if (!is.null(add))
    rubeResult$sims.array <- augment(rubeResult, add)$sims.array

  # Get extra info before all but MCMC results are dropped
  LCPreSuf <- rubeResult$LCPreSuf  # LC() prefixes and suffixes
  varList <- rubeResult$varList    # LC formula variable lists

  # Get Gelman r-hat values and distribution names if available
  Rhat <- distStrings <- NULL
  if (is(rubeResult,"bugs") || is(rubeResult,"rjags"))  {
    if (any("Rhat"==colnames(rubeResult$summary))) {
      Rhat <- as.numeric(rubeResult$summary[,"Rhat"])
    }
    if (!is.null(rubeResult$check)) {
      tmp <- rubeResult$check$stochs[,1]
      distNames <- names(tmp)
      distStrings <- as.character(tmp)
    }
    rubeResult <- rubeResult$sims.array
  } else {
    if (!is.array(rubeResult) || length(dim(rubeResult))!=3)
      stop("rubeResult must be a bugs() or jags() object or its $sims.array")
  }

  # Handle 'drop'
  if (drop>0 && drop<n.iter-1) {
    rubeResult <- rubeResult[(drop+1):n.iter,,]
    objDim <- dim(rubeResult)
    n.iter <- objDim[1]
  }  

  # Get parameter name info (multiple forms needed)
  parNamesAll <- dimnames(rubeResult)[[3]]
  parNamesAllLCname <- rep(NA, length(parNamesAll)) # alternate form stores LCnames

  # Process / create viewGroups
  if (!is.list(viewGroups) && is.character(viewGroups)) 
    viewGroups <- list(viewGroups)
  if (!is.null(viewGroups) && !is.list(viewGroups)) stop("viewGroups must be a list")
  if (!is.null(viewGroups)) {
    loc <- str_locate(parNamesAll,"\\[")[,1]
    Sel <- which(!is.na(loc))
    if (length(Sel)>0) {
      arr <- unique(substring(parNamesAll[Sel],1,loc-1))
      for (i in 1:length(viewGroups)) {
        if (any(viewGroups[[i]]%in%arr))
          stop("viewGroups may not contains vectors/arrays")
      }
    }
  }
  if (!is.null(LCPreSuf)) {
    for (i in 1:nrow(LCPreSuf)) {
      id <- rownames(LCPreSuf)[i]
      form <- varList[[id]]
      if (!is.null(form)) {
        formVars <- parseFormula(form)
        if (is.numeric(formVars[[1]])) formVars <- formVars[-1]
        formVars <- sapply(formVars, paste, collapse="")
        formVars <- paste(LCPreSuf[i,1], formVars, LCPreSuf[i,2], sep="")
        if (!is.null(viewGroups)) {
          dups <- lapply(viewGroups, function(x,this) intersect(x,this), this=formVars)
          firstdups <- which(sapply(dups,length)>0)[1]
          if (!is.na(firstdups)) {
            stop("parameter(s) from in viewGroups also in varList: ", 
                 paste(dups[[firstdups]], collapse=", ")) 
          } 
        }
        attr(formVars,"formName") <- id
        attr(formVars,"prefix") <- LCPreSuf[i,1]
        attr(formVars,"suffix") <- LCPreSuf[i,2]
        viewGroups <- c(viewGroups, list(formVars)) 
        if (LCPreSuf[i,2]=="") {
          names(viewGroups)[length(viewGroups)] <- LCPreSuf[i,1]
        } else {
          names(viewGroups)[length(viewGroups)] <- paste(LCPreSuf[i,1], ".", LCPreSuf[i,2], sep="")
        }
      }
    }
  }
  if (!is.null(viewGroups)) {
    if (is.null(names(viewGroups)))
      names(viewGroups) <- rep(NA,length(viewGroups))
    Sel <- which(is.na(names(viewGroups)))
    if (any(Sel)) 
      names(viewGroups)[Sel] <- paste("viewGroup",1:length(viewGroups),sep="")[Sel]
    # drop viewGroup elements that were not in parameters.to.save
    tmp <- lapply(viewGroups, match, table=parNamesAll)
    tmp2 <- sapply(tmp, function(x) all(is.na(x)))
    if (any(tmp2)) {
      viewGroups <- viewGroups[!tmp2]
      tmp <- tmp[!tmp2]
    }
    if (length(viewGroups)==0) {
      viewGroups <- NULL
    } else {
      tmp2 <- sapply(tmp, function(x) any(is.na(x)))
      for (i in (1:length(tmp2))[tmp2]) {
        tmpA <- sapply(c("formName","prefix","suffix"), function(x)attr(viewGroups[[i]],x))
        viewGroups[[i]] <- viewGroups[[i]][which(!is.na(tmp[[i]]))]
        attr(viewGroups[[i]], "formName") <- tmpA[1]
        attr(viewGroups[[i]], "prefix") <- tmpA[2]
        attr(viewGroups[[i]], "suffix") <- tmpA[3]
      }
    }
    for (i in seq(along=viewGroups)) {
      Sel <- which(parNamesAll%in%viewGroups[[i]])
      if (is.null(attr(viewGroups[[i]],"formName"))) {
        parNamesAll[Sel] <- paste(names(viewGroups)[i], "[",
                                  parNamesAll[Sel], "]", sep="")
        parNamesAllLCname[Sel] <- ""
      } else {
        parNamesAll[Sel] <- paste(names(viewGroups)[i], "[",
                                  substring(parNamesAll[Sel], nchar(attr(viewGroups[[i]],"prefix"))+1,
                                            nchar(parNamesAll[Sel]) -
                                            nchar(attr(viewGroups[[i]],"suffix"))), "]", sep="")
        parNamesAllLCname[Sel] <- attr(viewGroups[[i]], "formName")
      }
    }
  }

  parNamesRep <- sub("\\[.*\\]", "", parNamesAll)
  parNames <- parNamesRep[!duplicated(parNamesRep)]
  #parNamesLCname <- parNamesAllLCname[!duplicated(parNamesRep)]

  # Remove unwanted parameters
  if (is.null(params)) {
    params <- parNames
  } else {
    if (is.numeric(params)) {
      params <- params[params>0 & params<=length(parNames)]
      params <- unique(floor(params))
      if (length(params)==0) stop("bad numeric params vector")
      params <- parNames[params]
    }
    err <- is.na(match(params, parNames))
    if (any(err)) stop(paste(params[err],sep=", "), " not found in rubeResult")
    rubeResult <- rubeResult[,,!is.na(match(parNamesRep,params)), drop=FALSE]
    if (!is.null(Rhat)) Rhat <- Rhat[!is.na(match(parNamesRep,params))]
    parNamesAll <- dimnames(rubeResult)[[3]]
    parNamesAllLCname <- rep(NA, length(parNamesAll))  # Fix to get ids
    #parNamesRep <- sub("\\[.*\\]", "", parNamesAll)
    #parNames <- parNamesRep[!duplicated(parNamesRep)]
  }
  parNamesRep <- sub("\\[.*\\]", "", parNamesAll)
  parNames <- parNamesRep[!duplicated(parNamesRep)]
  parNamesLCname <- parNamesAllLCname[!duplicated(parNamesRep)]

  # parDescr include range of dimensions with names, e.g, x[1:5,2:1000]
  parDescr <- parNames
  if (length(parNames) < length(parNamesAll)) {
    vecArr <- unique(parNamesRep[duplicated(parNamesRep)])
    colon <- function(x) {
      if (x[1]==x[2]) return(paste(x[1]))
      return(paste(x[1], ":", x[2], sep=""))
    }
    for (v in vecArr) {
      tmp <- parNamesAll[parNamesRep==v]
      tmp <- substring(tmp, nchar(v)+2, nchar(tmp)-1)
      if (!substring(tmp[1],1,1)%in%as.character(0:9)) {
        Sel <- match(v,parDescr)
        if (!is.na(parNamesLCname[Sel][1])) {
          parDescr[Sel] <- paste(v, "[", parNamesLCname[Sel][1], "]", sep="")
        } else {
          parDescr[Sel] <- paste(v, "[", colon(c(1,length(tmp))), "]", sep="")
        }
      } else {
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
            parDescr[match(v,parDescr)] <- paste(v, "[", tmp, "]", sep="")          
          }
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
  if (length(parNames)==1 && !is.null(only)) {
    if (any(only>length(thisElements))) stop("'only' exceeds length of ", thisPar)
    if (length(only)==1) {
      thisElements <- thisElements[only]
    } 
  }
  thisDim <- length(thisElements)
  if (nPar==1 && thisDim==1) noMenu <- TRUE
  if (thisDim==1) {
    element <- 1
    index <- thisElements[element]
  } else {
    element <- 0  # element==0 indicates "summary plot mode"
  }
  # Variables to allow synch of subsets across parameters of identical dimension
  lastPar <- ""
  lastDim <- thisDim
  nShow <- min(thisDim,nSample)
  if (is.null(only)) {
    thisSubset <- sort(sample(thisDim, nShow))
  } else {
    thisSubset <- only 
    nShow <- length(only)
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

  #### Function to plot set of parameters
  plotOverview <- function() {
    nPlot <- ifelse(n.chains==1 || is.null(Rhat),2,3)
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

    # Posterior intervals
    Split <- rep(1:n.chains, each=n.iter)
    tmp <- apply(y, 2, function(x) {
             sapply(split(x, Split), quantile, prob=c((1-PIprob)/2, (1+PIprob)/2))
           })
    tmpm <- apply(y, 2, function(x) { sapply(split(x, Split), mean) })
    if (is.null(dim(tmpm))) tmpm <- matrix(tmpm, nrow=1)
    plot(NA, type="n", xlim=c(min(tmp), max(tmp)), ylim=c(0.6, nShow+0.4), axes=F,
         xlab="", ylab="", main=paste(100*PIprob,"% Post. Int.",sep=""))
    title(xlab=thisPar, line=2)
    box(); axis(1)
    labs <- substring(parNamesAll[parNamesRep==thisPar][rev(thisSubset)],nchar(thisPar)+1)
    fullLabs <- rev(substring(parNamesAll[parNamesRep==thisPar],nchar(thisPar)+1))
    if (!(substring(labs,2,2)%in%as.character(0:9))[1]) {
      labs <- substring(labs, 2, nchar(labs)-1)
    }
    axis(2, at=1:nShow, labels=labs, las=1)
    delta <- min(0.15, nShow/2/n.chains)
    Mid <- (n.chains+1)/2
    for (i in 1:nShow) {
      for (j in 1:n.chains) {
        segments(tmp[2*j-1,i], nShow-i+1+(j-Mid)*delta, 
                 tmp[2*j,i], nShow-i+1+(j-Mid)*delta, col=j)
      }
    }
    for (i in 1:nShow) {
      for (j in 1:n.chains) {
        segments(tmpm[j,i], nShow-i+1+(j-Mid)*delta-delta/4, 
                 tmpm[j,i], nShow-i+1+(j-Mid)*delta+delta/4, col=j)
      }
    }
    mtext(parNamesDist[pNum], outer=T, line=+0.4, cex=1.35)
    if (LCzero && !is.na(parNamesLCname[pNum])) abline(v=0,lty=3,col="pink")

    # acf
    Sig <- qnorm(0.975)/sqrt(n.iter)
    tmp <- apply(y, 2, function(x) {
             sapply(split(x, Split), function(x) {
                tmp <- acf(x, plot=FALSE)$acf[-1]
                tmp2 <- which(tmp<Sig)
                if (!any(tmp2)) return(length(tmp))
                return(max(0.2, min(tmp2)-1))})
           })
    if (is.null(dim(tmp))) tmp <- matrix(tmp, 1)
    maxLag <- max(tmp)
    plot(NA, type="n", xlim=c(0, 10*log10(n.iter)),
         ylim=c(0.6, nShow+0.4), axes=F,
         xlab="", ylab="", main="Signif. Autocor. Lags")
    title(xlab="Lag", line=2)
    box()
    axis(1)
    axis(2, at=1:nShow, labels=labs, las=1)
    for (i in 1:nShow) {
      for (j in 1:n.chains) {
        segments(0, nShow-i+1+(j-Mid)*delta, 
                 tmp[j,i], nShow-i+1+(j-Mid)*delta, col=j)
      }
    }

    # rHats
    if (!is.null(Rhat)) {  #n.chains>1) {
      rhat <- Rhat[parNamesRep==thisPar]
      maxHat <- max(rhat)
      plot(NA, type="n", xlim=c(0, 5),
           ylim=c(0.6, thisDim+0.4), axes=F,
           xlab="", ylab="", main="rHat")
      title(xlab="Rhat", line=2)
      box()
      axis(1)
      if (thisDim<30) {
        axis(2, at=1:thisDim, labels=fullLabs, las=1)
      } 
      for (i in 1:thisDim) {
          segments(0, thisDim-i+1, rhat[i], thisDim-i+1, 
            col=c("green3","orange","red")[findInterval(rhat[i],rHatGrayZone)])
      }
    }

    # Setup menu area
    if (!noMenu) {
      par(mar=marMenu)
      plot(c(0,1), c(0,1), type="n", xlab="", ylab="", axes=F)
    }
    invisible(NULL)
  }

  #### Function to plot individual parameter
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
    plot(y[,1,], type="l", xlab="", ylab=thisPar, ylim=range(y))
    title(xlab="iteration number", line=2)
    if (n.chains==1) {  # reference line & RHS dot to show horizontal trend
      lmr <- lm(y~I(1:length(y)))
      abline(lmr, col="pink")
      usrx <- par("usr")[1:2]
      points(usrx[2], coef(lmr)[1]+usrx[1]*coef(lmr)[2], pch=16, col="pink")
    } else { # other chains
      for (i in 2:n.chains) lines(y[,i,], col=i)
      if (!is.null(Rhat)) {
        rhat <- Rhat[index]
        usr = par("usr")
        cxy = par("cxy")
        text(usr[2]-0.5*cxy[1], usr[4]+0.25*cxy[2], adj=c(1,0), xpd=NA,
             paste("Rhat=",format(rhat,digits=3),sep=""),
             col=c("green3","orange","red")[findInterval(rhat,rHatGrayZone)])
      }
    }
    mtext(parNamesAllDist[index], outer=T, line=+0.4, cex=1.35)

    # acf(s):
    tmp <- apply(y, 2, function(x) {
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
    abline(h=c(-1,1)*qnorm(0.975)/sqrt(n.iter), col="blue", lty="dashed")
    for (i in 1:n.chains) {
      lag <- as.numeric(tmp[[i]]$lag)
      ac <- as.numeric(tmp[[i]]$acf)
      d <- 0.15*(i-1)
      segments(lag+d, rep(0,length(lag)), lag+d, ac, col=i)
    }

    # hist/density plots:
    if (n.chains==1) {
      hist(as.numeric(y), breaks=min(30,length(y)/30), xlab="", main="")
      title(xlab=parNamesAll[index], line=2)
    } else {
      tmp <- apply(y, 2, density)
      tmpx <- sapply(tmp, function(o) range(o$x))
      tmpy <- sapply(tmp, function(o) range(o$y))
      plot(NA, type="n", xlim=range(tmpx), ylim=range(tmpy),
           xlab="", ylab="density", main="")
      title(xlab=parNamesAll[index], line=2)
      for (i in 1:n.chains) lines(tmp[[i]], col=i) 
    }
    if (!is.null(PIprob)) {
      tmp <- apply(y, 2, quantile, 
                    probs=c((1-PIprob)/2, 1-(1-PIprob)/2))
      tmpm <- apply(y, 2, mean)
      for (i in 1:n.chains) 
        abline(v=c(tmp[,i],tmpm[i]), col=ifelse(n.chains==1,3,i), lwd=c(1,1,2))
    }
    if (!is.null(true)&&length(true)>=pNum) abline(v=true[pNum],col=1, lwd=3, lty=3)

    # Setup menu area
    if (!noMenu) {
      par(mar=marMenu)
      plot(c(0,1), c(0,1), type="n", xlab="", ylab="", axes=F)
    }
    invisible(NULL)
  }

  #############################################
  ## Show posterior plot(s) until user quits ##
  #############################################
  allDone <- FALSE
  while (!allDone) {
    if (element==0) {
      y <- rubeResult[,,which(parNamesRep==thisPar), drop=FALSE]
      nShow <- min(thisDim,nSample)
      if (!is.null(only)) nShow <- length(only)
      if (thisDim<nSample) {
        thisSubset <- 1:nShow
      } else {
        if (thisPar==lastPar || thisDim!=lastDim)
          thisSubset <- sort(sample(thisDim, nShow))
      }
      y <- apply(y[,,thisSubset,drop=FALSE],3,function(x)as.numeric(x))
      plotOverview()
    } else {
      y <- rubeResult[,,index, drop=FALSE]
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

  ##if (!is.null(oldpar)) par(oldpar)
  
  invisible(NULL)
}

