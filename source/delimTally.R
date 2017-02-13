# Make a table with counts of all delimiters of a certain class
# in a vector of text strings.
# First row counts "left" (open) and second row counts "right" (close).
# Each column is for one text string with a non-zero count for either
# left or right delimiters.  The line numbers are in the column names.
# If assumeSingles==TRUE, time is saved, but the result is correct
# only if the number of delimeters per line is always zero or one.
# E.g., delimTally(c("for (i in 1:4) {x <- (a+b)*3}", "y <- 4", "z <- exp(a)"), "(")
#          1  3
#    left  2  1
#    right 2  1
#
delimTally <- function(text, delim=c("{","(","["), assumeSingles=FALSE) {
  delim1 = match.arg(delim)
  delim2 = c("}",")","]")[match(delim1,c("{","(","["))]
  if (assumeSingles) {
    d1 = grep(delim1, text, fixed=TRUE)
    d2 = grep(delim2, text, fixed=TRUE)
    delimPos = sort(c(d1,d2))
    delimCount = matrix(0, 2, length(delimPos))
    delimCount[1, match(d1,delimPos)] = 1
    delimCount[2, match(d2,delimPos)] = 1
  } else {
    delimPos = grep(paste("(\\",delim1,"|\\",delim2,")",sep=""), text)
    delimCount = sapply(strsplit(text[delimPos],""),
                      function(x) c(sum(x==delim1), sum(x==delim2)))
  }
  if (length(delimPos)>0) 
    dimnames(delimCount) = list(c("left","right"), delimPos)
  return(delimCount)
} # end delimTally()

