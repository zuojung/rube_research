print.summary.bugsCheck <- function(x, ..., digits=max(3, getOption("digits")-3)) {
  cat("Rube Summary:\n")
  if (!is.null(x[["startTime"]])) {
    cat("Run at", substring(as.character(x$startTime),1,16),
        "and taking", round(unlist(x$runTime),2), attr(x$runTime, "units"), "\n")
  }

  if (!is.null(x$forVars)) wrap(x$forVars,"For variables:")

  if (is.null(x$assigns)) {
    cat("\nNo assignments.\n")
  } else {
    cat("\nAssignments (logicals):\n")
    if (is.null(dim(x$assigns))) {
      wrap(x$assigns)
    } else {
      print(x$assigns)
    }
  }

  #if (is.null(x$inits)) {
  #  cat("\nNo initializations.\n")
  #} else {
  #  cat("\nInitializations:\n")
  #  if (is.null(dim(x$inits))) {
  #    wrap(x$inits)
  #  } else {
  #    print(x$inits)
  #  }
  #}

  if (!is.null(x$uninitialized)) {
    cat("\nUninitialized:\n")
    wrap(x$uninitialized)
  }

  if (is.null(x$constants) ||
      (!is.null(dim(x$constants)) && nrow(x$constant)==0)) {
    cat("\nNo constants.\n")
  } else {
    cat("\nConstants:\n")
    if (is.null(dim(x$constants))) {
      wrap(x$constants)
    } else {
      print(x$constants)
    }
  }   

  if (is.null(x$data)) {
    cat("\nNo data.\n")
  } else {
    cat("\nData:\n")
    if (is.null(dim(x$data))) {
      wrap(x$data)
    } else if (nrow(x$data)==0) {
      cat("None\n")
    } else {
      print(x$data)
    }
  }

  cat("\nStochastics:\n")
  if (is.null(x$stochs) || nrow(x$stochs)==0) {
    cat("None\n")
  } else {
    print(x$stochs)
  }

  if (!is.null(x$concerns)) {
    cat("\nConcerns:\n")
    cat(paste(x$concerns, collapse="\n"), "\n")
  }

  if (!is.null(x$problems)) {
    cat("\nProblems:\n")
    prob <- x$problems
    for (i in 1:length(prob)) {
      cat(prob[i], "\n")
      tmp <- str_locate(prob[i],"line [[:digit:]]*")[1,]
      if (!is.na(tmp[1]))
        cat(x$model[as.numeric(substring(prob[i],tmp[1]+5,tmp[2]))], "\n")
    }
  } else {
    cat("\nNo problems detected!\n")
  }

  invisible(x)
}
