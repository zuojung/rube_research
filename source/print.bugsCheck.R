print.bugsCheck <- function(x, ...) {
  cat("Rube Results:\n")
  if (!is.null(x[["startTime"]])) {
    cat("Run at", substring(as.character(x$startTime),1,16),
        "and taking", round(unlist(x$runTime),2), attr(x$runTime, "units"), "\n")
  }
 
  if (!is.null(x$constants) &&
      (is.null(dim(x$constants)) || nrow(x$constant)>0)) {
    cat("\nConstants:\n")
    if (is.null(dim(x$constants))) {
      wrap(x$constants)
    } else {
      print(x$constants)
    }
  }   

  if (!is.null(x$data)) {
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

  if (!is.null(x$problems)) {
    cat("\nProblems:\n")
    cat(paste(x$problems, collapse="\n"), "\n")
  }
  invisible(x)
}
