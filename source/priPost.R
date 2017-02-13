##########################################################
# Prior / posterior plotter function                     #
# Allows prior to be normal with parameters mean and sd, #
# or uniform with parameters min and max or gamma with   #
# parameters a, b such that mean=a/v (as in bugs), or    #
# hyperparameters of a beta or gamma ("post" must be a   #
# two column matrix or a list of alpha and beta vectors, #
# and pripost is then c(a, b, a, b)).                    #
# "nsim" sets number of simulations for hyperpriors.     #
##########################################################
priPost <- function(post, 
             dist=c("Normal","Uniform","Gamma","Beta",
                    "sdFromGammaPrecision",
                    "HyperBeta", "HyperGamma"),
             pripar, main=NULL, xlab=NULL, xlim=NULL, nsim=2000, digits=1, 
             true=NULL, hgtrim=0.99) {
  #if (!suppressWarnings(require("stringr", quietly=TRUE)))
  #  stop("Please install the stringr package, then try again.")
  vers = R.Version()
  if (vers$major<2 || (vers$major==2 && as.numeric(vers$minor)<10))
    stop("Please upgrade to at least R version 2.10.")

  if (is.null(xlab)) {
    xlab = deparse(substitute(post))
    loc = str_locate_all(xlab,fixed("$"))[[1]][,1]
    LL = length(loc)
    if (LL==0) {
    } else {
      loc = loc[length(loc)]
      if (!is.na(loc)) xlab = substring(xlab,loc+1)
    }
  }
  dist = match.arg(dist)
  if (substring(dist,1,5)=="Hyper") {
    if (!is.matrix(post) && !is.list(post))
      stop("For hyperpriors,post must be a list or matrix")
    if (is.list(post)) {
      if (length(post)!=2 || !all(sapply(post,class)=="numeric"))
        stop("list version of post must be two numeric vectors")
      if (length(post[[1]])!=length(post[[2]]))
        stop("list version of post must be two equal sized numeric vectors")
      post = cbind(post[[1]], post[[2]])
    } else {
      if (ncol(post)!=2) stop("Need 2 columns in post")
    }
  } else {
    if (is.list(post) && is.numeric(unlist(post))) post=unlist(post)
    if (!is(post,"numeric")) {
      if (is.list(post)) cat("Perhaps you used [] instead of [[]].\n")
      stop("post must be a numeric vector")
    }
  }

  # Set prior x limits based on distribution
  if (dist=="Normal") {
    priLim=c(-3,3)*pripar[2]+pripar[1]
    dFrom = -Inf; dTo = Inf;
  } else if (dist=="Uniform") {
    priLim=pripar
    dFrom = pripar[1]; dTo = pripar[2];
  } else if (dist=="Gamma") { 
    priLim=qgamma(c(0.01,0.99), pripar[1], pripar[2])
    dFrom = 0; dTo = Inf;
  } else if (dist=="Beta") { 
    priLim=c(0,1)
    dFrom = 0; dTo = 1;
  } else if (dist=="HyperBeta") {
    priLim=c(0,0)
    dFrom = 0; dTo = 1;
  } else if (dist=="HyperGamma") {
    priLim=c(0,1) # should be overridden by posterior
    dFrom = 0; dTo = Inf;
  } else if (dist=="sdFromGammaPrecision") {
    priLim = rev(1/sqrt(qgamma(c(0.01,0.99), pripar[1], pripar[2])))
  } else stop("missing dist limit code for ", dist)

  # Posterior density
  if (dist=="HyperBeta") {
    post = apply(post, 1, function(x) rbeta(1,x[1],x[2]))
    #pd = density(post, from=0, to=1)
  } else if (dist=="HyperGamma") {
    post = apply(post, 1, function(x) rgamma(1,x[1],x[2]))
    #pd = density(post, from=0)
  } else if (dist=="sdFromGammaPrecision") {
    #pd = density(1/post^2, from=0)
    post = 1/post^2
  } 
  if (!is.finite(dFrom)) {
    if (!is.finite(dTo)) {
      pd = density(post)
    } else {
      pd = density(post, to=dTo)
    }
  } else if (!is.finite(dTo)) {
      pd = density(post, from=dFrom)
  } else {
      pd = density(post, from=dFrom, to=dTo)
  }
  if (is.null(xlim)) {
    xlim=c(min(pd$x,priLim[1]), max(pd$x,priLim[2]))
  }

  # Prior density
  if (substring(dist,1,5)!="Hyper") {
    if (is.null(xlim)) {
      Seq=seq(priLim[1],priLim[2],len=200)
    } else {
      Seq=seq(max(xlim[1],priLim[1]),min(xlim[2],priLim[2]),len=200)
    }
  }
  if (dist=="Normal") {
    prior = dnorm(Seq,pripar[1],pripar[2])
    priorText = paste("prior N(", pripar[1],",",pripar[2],")", sep="")
  } else if (dist=="Uniform") {
    prior = dunif(Seq,pripar[1],pripar[2])
    priorText = paste("prior Unif(", pripar[1],",",pripar[2],")", sep="")
  } else if (dist=="Gamma") { 
    prior = dgamma(Seq,pripar[1],pripar[2])
    priorText = paste("prior Gam(", pripar[1],",",pripar[2],")", sep="")
  } else if (dist=="Beta") { 
    prior = dbeta(Seq,pripar[1],pripar[2])
    priorText = paste("prior Beta(", pripar[1],",",pripar[2],")", sep="")
  } else if (dist=="HyperBeta") {
    priorSim = rbeta(nsim, rgamma(nsim, pripar[1], pripar[2]),
                           rgamma(nsim, pripar[3], pripar[4]))
    prior = density(priorSim, from=0, to=1)
    Seq = prior$x
    prior = prior$y
    priorText = paste("prior Gam(", pripar[1], ",", pripar[2],") + Gam(",
                      pripar[3], ",", pripar[4], ")", sep="")
  } else if (dist=="HyperGamma") {
    priorSim = rgamma(nsim, rgamma(nsim, pripar[1], pripar[2]),
                           rgamma(nsim, pripar[3], pripar[4]))
    prior = density(priorSim[priorSim<quantile(priorSim,hgtrim)], from=0)
    Seq = prior$x
    prior = prior$y
    priorText = paste("prior Gam(", pripar[1], ",", pripar[2],") + Gam(",
                      pripar[3], ",", pripar[4], ")", sep="")
  } else if (dist=="sdFromGammaPrecision") {
    priorSim = 1/sqrt(rgamma(nsim,pripar[1],pripar[2]))
    prior = density(priorSim, from=0)
    Seq = prior$x
    prior = prior$y
    priorText = paste("prior precision is Gam(", pripar[1],",",pripar[2],")", sep="")
  } else stop("missing prior density code for ", dist)

  if (is.null(main)) main = "Prior / Posterior Plot"
  plot(pd, xlim=xlim, main=main, type="l", xlab=xlab,
       ylim=c(0,max(pd$y,min(max(prior),3*max(pd$y)))))
  lines(Seq, prior, col=3)
  if (!is.null(true)) abline(v=true, col=2)

  legend("topright", c(priorText,
        paste("posterior 95%=[",round(quantile(post,0.025),digits),
                            ",",round(quantile(post,0.975),digits),
                            "]",sep="")), lty=1, col=c(3,1))
  invisible(NULL)
}

