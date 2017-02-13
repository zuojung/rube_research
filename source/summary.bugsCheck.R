summary.bugsCheck <- function(object, ..., digits=max(3, getOption("digits")-3)) {
  class(object) <- "summary.bugsCheck"
  return(object)
}
