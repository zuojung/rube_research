# Print a string vector, optionally with commas, wrapped to a
# line length.  Default is to (print=TRUE) write to the screen.
wrap <- function(text, prefix="", suffix="", print=TRUE, length=72, sep=",") {
  if (!is.character(prefix) || !is.character(suffix) || !is.character(text)) stop("bad input")
  if (length(prefix)!=1) stop("bad prefix")
  if (length(suffix)!=1) stop("bad suffix")
  n <- length(text)
  if (n==0) return(NULL)
  if (n>1) text[1:(n-1)] <- paste(text[1:(n-1)], sep, sep="")
  if (nchar(prefix)>0) {
    text <- c(prefix, text)
    n <- n + 1
  }
  if (nchar(suffix)>0) {
    text <- c(text, suffix)
    n <- n + 1
  }
  start <- 1
  lens <- sapply(text, nchar) + 1
  newtext = NULL
  while (TRUE) {
    clens <- cumsum(lens)
    end <- rev(which(clens<=length))[1]
    if (is.na(end)) end <- 1
    onetext <- paste(c(text[1:end], "\n"), collapse=" ")
    if (print) {
      cat(onetext)
    }
    newtext <- c(newtext, onetext)
    if (n==end) break
    text <- text[(end+1):n]
    lens <- lens[(end+1):n]
    n <- n - end
  }
  invisible(newtext)
}
