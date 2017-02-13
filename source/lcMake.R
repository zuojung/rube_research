# This utility function is useful for generating the
# parameters.to.save argument of rube() when the model
# includes use of the LC() pseudofunction.
#
# Arguments prefix and suffix are whatever was used
# in the LC() pseudofunction, and RHS is a right-hand-side
# formula, normally an element of the varList argument to
# rube().
#
# The function returns a string vector of the corresponding
# model parameters.
#
# Currently this incorrectly drops interactions!!!!!!!!!
lcMake <- function(prefix, RHS, suffix) {
  tmp <- str_locate(RHS, "-1")[1,1]
  if (!is.na(tmp)) {
    RHS <- gsub("-1", " ", RHS)
    zero <- NULL
  } else {
    zero <- "0"
  }
  RHS <- gsub("[\\*+()^:]", " ", RHS)
  RHS <- unique(c(zero, strsplit(RHS," ")[[1]]))
  return(paste(prefix,RHS,suffix,sep=""))
}
