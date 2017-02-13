# Interactively Explore Prior Distributions
#
priorExplore <- function() {
  Dists <- list(discrete=c("Bernoulli","binomial","Poisson","neg-binomial"),
                continuous=c("normal","gamma","uniform","beta","tauAsGamma"))
  ParmN <- list(discrete=c(1,2,1,2), continuous=c(2,2,2,2,3))
  ParmNames <- list(discrete=list("prob", c("size","prob"),
                                  "mean", c("prob","failLimit")),
                    continuous=list(c("mean","sd"), c("location","scale"),
                                    c("lower","upper"),c("location","scale"),
                                    c("log10a","log10b","plotUpper%")))
  DistSupport <- list(discrete=cbind(c(0,0,0,0), c(1,NA,Inf,Inf)),
                      continuous=cbind(c(-Inf,0,NA,0,0), c(Inf,Inf,NA,1,Inf)))
  ParmLims <- list(discrete=list(bern=rbind(c(0,1)),
                                 binom=rbind(c(1,15), c(0,1)),
                                 pois=rbind(c(0.1,50)),
                                 nb=rbind(c(0,1),c(1,25))),
                   continuous=list(norm=rbind(c(-30,30),c(0.2,100)),
                                   gamma=rbind(c(0.01,20), c(0.01,5)),
                                   unif=rbind(c(-20,100), c(-20,100)),
                                   beta=rbind(c(0.1,30), c(0.1,30)),
                                   tauAsGamma=rbind(c(-3,1),c(-3,1), c(0.02,0.99))))
  ParmRound <- list(discrete=list(bern=2,
                                 binom=c(0,2),
                                 pois=2,
                                 nb=c(2,0)),
                    continuous=list(norm=c(2,2),
                                    gamma=c(2,2),
                                    unif=c(2,2),
                                    beta=c(2,2),
                                    tauAsGamma=c(1,1,2)))
  ParmSymbols <- list(discrete=list("p", c("n","p"),"mu",c("p","r")),
                      continuous=list(c("mu","sigma"),c("a","b"),
                                      c("a","b"),c("a","b"),c("log10a","log10b","%")))
  ParmDefaults <- list(discrete=list(0.6, c(4,0.5), 3, c(0.3,2)),
                       continous=list(c(0,10), c(2,0.4), c(-2,4), c(3,7), c(0,0,0.99)))
  WBDists <- list(discrete=c("dbern","dbin","dpois","dnegbin"),
                  continuous=c("dnorm","dgamma","dunif","dbeta","dgamma"))
  RDists <- list(discrete=c("dbinom","dbinom","dpois","dnbinom"),
                  continuous=c("dnorm","dgamma","dunif","dbeta","dgamma"))
  Types <- c("Discrete","Continuous")

  typeN <- 1
  type <- Types[typeN]
  priorN <- 1
  prior <- Dists[[typeN]][priorN]
  parms <- ParmDefaults[[typeN]][[priorN]]
  parmsNames <- ParmNames[[typeN]][[priorN]]
  parmsN <- length(parms)
  parmsSymbols <- ParmSymbols[[typeN]][[priorN]]
  wbDist <- WBDists[[typeN]][priorN]

  qShow <- c(0.01, 0.025, 0.25, 0.50, 0.75, 0.975, 0.99)
  qShowN <- length(qShow)

  NAC <- as.character(NA)
  if (is.null(dev.list())) 
    plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
  oldPar = par(no.readonly=TRUE)
  on.exit(par(oldPar))
  layout(cbind(1:2), heights=c(0.67, 0.33))
  firstDraw <- TRUE
  resetButtonVals <- TRUE
  resetSlideVals <- TRUE
  while (TRUE) {
    # Show distribution
    par(mar=c(4.1,4.1,3,1))
    if (prior=="normal") {
      main = paste(WBDists[[typeN]][priorN], "(", 
               round(parms[1],2), ", tau=", 
               round(1/parms[2]^2,5), ")", sep="")
    } else if (prior=="tauAsGamma") {
      main = paste(WBDists[[typeN]][priorN], "(", 
               round(10^parms[1],5), ", ", 
               round(10^parms[2],5), ")", sep="")
    } else {
      main = paste(WBDists[[typeN]][priorN], "(", 
               paste(round(parms,2), collapse=", "), ")", sep="")
    }
    parMean <- parSD <- NA

    # Some parameterizations don't match R
    adjParms <- parms
    if (prior=="neg-binomial") {
      adjParms <- rev(parms)
    } else if (prior=="Bernoulli") {
      adjParms <- c(1, parms[1])
    } else if (prior=="tauAsGamma") {
      adjParms <- 10^parms[1:2]
    }
 
    # Get x limits to plot, plus mean and sd
    low = DistSupport[[typeN]][priorN,1]
    hi = DistSupport[[typeN]][priorN,2]
    if (type=="Discrete") {
      if (prior=="Bernoulli") {
        parMean <- parms[1]
        parSD <- sqrt(parms[1]*(1-parms[1]))
      } else if (prior=="binomial") {
        hi = adjParms[1]
        parMean <- parms[1]*parms[2]
        parSD <- sqrt(parms[1]*parms[2]*(1-parms[2]))
      } else if (prior=="Poisson") {
        parMean <- parms[1]
        parSD <- sqrt(parms[1])
      } else if (prior=="neg-binomial") {
        parMean <- parms[2]*(1-parms[1])/parms[1]
        parSD <- sqrt(parms[2]*(1-parms[1])/parms[1]^2)
      }
    } else { # Continuous 
      if (prior=="normal") {
        parMean <- parms[1]
        parSD <- parms[2]
      } else if (prior=="gamma") {
        parMean <- parms[1]/parms[2]
        parSD <- sqrt(parms[1]/parms[2]^2)
      } else if (prior=="uniform") {
        low <- adjParms[1]; hi <- adjParms[2]
        if (hi<=low) {
          low <- hi <- NA
        } else {
        parMean <- sum(parms)/2
        parSD <- sqrt(diff(parms)^2/12)
        }
      } else if (prior=="beta") {
        parMean <- parms[1]/sum(parms)
        parSD <- sqrt(parms[1]*parms[2]/sum(parms)^2/(sum(parms)+1))
      } else if (prior=="tauAsGamma") {
        sims <- sqrt(1/rgamma(10000, adjParms[1], adjParms[2]))
        parMean <- mean(sims)
        parSD <- sd(sims)
        qtiles <- quantile(sims, qShow)
        low <- qtiles[1]
        hi <- quantile(sims, parms[3])
        sims <- sims[sims>low & sims<hi]
      }
    }
    if (prior=="Bernoulli" || (prior=="binomial" && parms[1]<5)) {
      qtiles <- NA
    } else if (prior!="tauAsGamma") {
      expr <- paste("q", substring(RDists[[typeN]][priorN],2),
                    "(c(", paste(qShow,collapse=","), "),", 
                    paste(adjParms,collapse=","), ")", sep="")
      qtiles = eval(parse(text=expr)) 
    }
    if (is.infinite(hi)) {
      hi = qtiles[qShowN] 
    }
    if (is.infinite(low)) {
      low = qtiles[1] 
    }
    if (!is.na(low) & !is.na(hi)) {
      if (type=="Continuous") {
        Seq <- seq(low, hi, len=200)
      } else {
        Seq <- low:hi
      }
      if (prior=="tauAsGamma") {
        dens <- density(sims, from=low, to=hi)
        Seq <- dens$x
        dens <- dens$y
      } else {
        dens <- eval(parse(text=paste(RDists[[typeN]][priorN],"(Seq,",
                                  paste(adjParms,collapse=","),")")))
      }
      err <- is.infinite(dens)
      if (any(err)) {
        dens <- dens[!err]
        Seq <- Seq[!err]
      }
      seqN <- length(Seq)
      if (seqN<2) {cat("<2 density values\n");browser()}

      # Annotate part of distribution not shown
      left <- NA
      tmpLim <- DistSupport[[typeN]][priorN,]
      if (!is.na(tmpLim[1]) && Seq[1]>tmpLim[1]) {
        expr <- paste("p", substring(RDists[[typeN]][priorN],2),
                    "(Seq[1], ", paste(adjParms,collapse=","), ")", sep="")
        left <- round(eval(parse(text=expr)),3)
      }
      right <- NA
      if (!is.na(tmpLim[2]) && Seq[seqN]<tmpLim[2]) {
        expr <- paste("p", substring(RDists[[typeN]][priorN],2),
                    "(Seq[seqN], ", paste(adjParms,collapse=","), ")", sep="")
        right <- round(1-eval(parse(text=expr)),3)
      }
      xlab <- prior
      if (prior=="tauAsGamma") xlab <- "sd for tau as Gamma"
      if (!is.na(parMean) && !is.na(parSD))
        xlab <- paste(xlab, ":  mean=", round(parMean,2), " sd=", 
                      round(parSD,2), sep="")
      if (type=="Continuous") {
        plot(Seq, dens, type="l", ylim=c(0,max(dens)), col=2,
               xlab="", ylab="", main=main)
        title(xlab=xlab, line=1.8)
        title(ylab="Density", line=2)
        if (prior=="uniform")
          segments(adjParms, rep(0,2), adjParms, rep(dens[1],2), col=2)
        segments(Seq[1], 0, Seq[seqN], 0, lty="dotted", col="lightgray")
        if (!is.na(left)) segments(Seq[1], 0, Seq[1], dens[1], col=2)
        if (!is.na(right)) segments(Seq[seqN], 0, Seq[seqN], dens[seqN], col=2)
      } else {
        plot(NA, type="n", xlim=c(low, hi), ylim=c(0,max(dens)), 
          xlab="", ylab="", main=main)
        title(xlab=xlab, line=1.8)
        title(ylab="Probability", line=2)
        segments(Seq, rep(0, length(Seq)), Seq, dens, lwd=4, col=2)
      }
      usr = par("usr")
      if (!is.na(qtiles[1])) {
        abline(v=qtiles, col=3)
        text(qtiles, usr[4], qShow, adj=c(0.5,-0.2), col=3, cex=0.8, xpd=NA)
      }
      if (!is.na(left))
        text(usr[1], dens[1], left, adj=c(0,-0.2), col=2, cex=0.8, xpd=NA)
      if (!is.na(right))
        text(usr[2], dens[seqN], right, adj=c(1,-0.2), col=2, cex=0.8, xpd=NA)
    } else {
      plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE, main=main)
    }

    # Setup for new buttons/sliders
    if (resetButtonVals) {
      buttons <- c(Dists[[typeN]][-priorN], Types[3-typeN], "Quit")
      resetButtonVals <- FALSE
    }
    if (resetSlideVals) {
      slideVals <- list(parms)
      slideParms <- NULL
      PL <- ParmLims[[typeN]][[priorN]]
      WL <- ParmRound[[typeN]][[priorN]]
      for (i in 1:parmsN) {
        slideParms[[i]] <- list(range=PL[i,], round=WL[i], labX=0.2)
      }
      names(slideParms) <- paste(parmsSymbols," (",parmsNames,")",sep="")
      resetSlideVals <- FALSE
    }
    
    # Get button/slider input
    par(mar=rep(1,4))
    click <- buttonInput(list(buttons,NAC), list(slideParms), slideVals, 
                         heights=c(0.34, 0.66), new=TRUE,#firstDraw,
                         input=TRUE)
    firstDraw <- FALSE
    if (is.null(click) || click=="Quit") break

    # process button/slider input
    ss=strsplit(click,"/")[[1]]
    if (click == Types[3-typeN]) {
      typeN <- 3 - typeN
      type <- Types[typeN]
      priorN <- 1
      prior <- Dists[[typeN]][priorN]
      parms <- ParmDefaults[[typeN]][[priorN]]
      parmsNames <- ParmNames[[typeN]][[priorN]]
      parmsN <- length(parms)
      parmsSymbols <- ParmSymbols[[typeN]][[priorN]]
      wbDist <- WBDists[[typeN]][priorN]
      #firstDraw <- TRUE
      resetButtonVals <- TRUE
      resetSlideVals <- TRUE
    } else if (click %in% Dists[[typeN]]) {
      priorN <- match(click, Dists[[typeN]])
      prior <- Dists[[typeN]][priorN]
      parms <- ParmDefaults[[typeN]][[priorN]]
      parmsNames <- ParmNames[[typeN]][[priorN]]
      parmsN <- length(parms)
      parmsSymbols <- ParmSymbols[[typeN]][[priorN]]
      wbDist <- WBDists[[typeN]][priorN]
      #firstDraw <- TRUE
      resetButtonVals <- TRUE
      resetSlideVals <- TRUE
    } else if (length(ss)==3) {
      spnum <- as.numeric(ss[2])
      newval <- as.numeric(ss[3])
      parms[spnum] <- newval
      resetSlideVals <- TRUE
    }
  } # end while looping on user input
 
  invisible(NULL)
  
}


