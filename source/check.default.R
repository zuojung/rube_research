check.default <- function(x, ...) {
  stop("method 'check()' is not defined for class '", class(x), "'")
}

