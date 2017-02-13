summary.rube <- function(object, ..., limit=c(10,2), digits=3) {
  if (!is(object,"bugs") && !is(object,"rjags")) {
    object <- summary.bugsCheck(object$check)
  } else {
    smry <- object$summary
    if (!is.null(object$MCMCerr)) {
      nc <- ncol(smry)
      smry <- cbind(smry[,1:2], MCMCerr=NA, smry[,3:nc])
      MCMCerr = object[["MCMCerr"]]
      tmp <- match(names(MCMCerr), rownames(smry))
      tmpNA <- is.na(tmp)
      smry[tmp[!is.na(tmp)], "MCMCerr"] <- MCMCerr[!is.na(tmp)]
    }
    if (any(rownames(smry)=="deviance")) smry <- smry[rownames(smry)!="deviance",,drop=FALSE]
    indLoc <- str_locate(rownames(smry), "\\[[[:digit:],]+\\]")
    hasInd <- !is.na(indLoc[,1])
    if (any(hasInd)) {
      indString <- substring(rownames(smry), indLoc[,1]+1, indLoc[,2]-1)
      indices <- rep(NA, length(indString))
      indices[hasInd] <- lapply(strsplit(indString[hasInd],","),as.numeric)
      mx <- max(sapply(indices,length))
      Len <- length(limit)
      if (Len<mx) limit <- c(limit, rep(limit[Len],mx-Len))
      keep <- sapply(indices, function(x) {
                                if (any(is.na(x))) return(TRUE)
                                return(all(x<=limit[1:length(x)]))
                              })
      smry <- smry[keep,,drop=FALSE]
    }
    object <- list(summary=smry, n.chains=object$n.chains, n.keep=object$n.keep,
                   n.thin=object$n.thin, n.burnin=object$n.burnin,
                   isDIC=object$isDIC, DIC=object$DIC, digits=digits,
                   startTime=object$startTime, runTime=object$runTime, engine=object$engine)
    class(object) <- "summary.rube"
  }

  return(object)
}
