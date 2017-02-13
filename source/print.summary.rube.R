print.summary.rube <- function(x, ..., digits=NULL) {
  if (is.null(digits)) digits <- x$digits
  if (is.null(digits)) digits <- 3

  cat(gettextf("Rube Summary:\n"))
  if (is.null(x[["startTime"]])) {
    if (!is.null(x$engine)) cat(gettextf(paste("Run by", x$engine,"\n")))
  } else {
    if (is.list(x$runTime)) {
      timeTimes <- sapply(x$runTime,
                         function(tm) gettextf("%3.2f %s", round(unlist(tm),2), attr(tm, "units")))
      timeTimes <- paste(timeTimes, collapse=" & ")
    } else {
      timeTimes <- gettextf("%3.2f %s", round(unlist(x$runTime),2), attr(x$runTime, "units"))
    }
    cat(gettextf(paste("Run",
                       ifelse(is.null(x$engine), "", paste(" by", x$engine)),
                       " at ", paste(substring(as.character(x$startTime),1,16), collapse=" & "),
                       " and taking ", timeTimes, "\n", sep="")))
  }

  cat(gettextf("Based on %d kept iterations (n.thin=%d) after burn-in of %d iterations\n",
                 x$n.keep, x$n.thin, x$n.burnin))

  if (x$n.chains==1) {
    cat(gettextf("for one chain.\n"))
  } else {
    cat(gettextf("for %d chains.\n",
                 x$n.chains))
  }

  print(x$summary, digits=digits)
  if (x$isDIC) cat("\nDIC =", x$DIC, "\n")

  invisible(x)
}
