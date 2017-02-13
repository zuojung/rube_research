print.rube <- function(x, ..., limit=c(10,2), digits=3) {
  if (is(x,"bugs") || is(x,"rjags")) {
    cat("Rube Results:\n")
    if (is.null(x[["startTime"]])) {
      if (!is.null(x$engine)) cat(gettextf(paste("Run by", x$engine,"\n")))
    } else {
      cat("Run", 
          ifelse(is.null(x$engine), "", paste(" by", x$engine)),
          " at ", substring(as.character(x$startTime),1,16), " and taking ",
          round(unlist(x$runTime),2), " ", attr(x$runTime, "units"), "\n", sep="")
    }

    smry <- x$summary
    if (!is.null(x$MCMCerr)) {
      nc <- ncol(smry)
      smry <- cbind(smry[,1:2], MCMCerr=NA, smry[,3:nc])
      MCMCerr = x[["MCMCerr"]]
      tmp <- match(names(MCMCerr), rownames(smry))
      tmpNA <- is.na(tmp)
      smry[tmp[!is.na(tmp)], "MCMCerr"] <- MCMCerr[!is.na(tmp)]
    }
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
    print(smry, digits=digits)
    if (x$isDIC) cat("\nDIC =", x$DIC, "\n")
  } else {
    print.bugsCheck(x$check)
  }

  invisible(x)
}
