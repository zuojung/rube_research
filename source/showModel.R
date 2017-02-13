###############################################################################
# Convenience function to show a model saved as a string with newlines in it. #
# Substitutions and case selection takes place (see subsText() and            #
# selectText()) unless subs=NA.  Variable lists are substituted into LC()     #
# pseudocode.  Indentation text can be changed, e.g., to a tab character.     #
# The default line numbering can be suppressed.                               #
###############################################################################
showModel <- function(model, subs=NULL, cases=NULL, 
                      varList=NULL, indent="  ", lineNumbers=TRUE) {
  model <- composeModel(model, subs=subs, cases=cases, varList=varList, 
                       outFile=NULL, warnUseless=FALSE, allowNoVarList=TRUE)
  M <- length(model)
  level <- rep(0, M)
  hasLeft <- grep("[{]",model)
  for (i in seq(along=hasLeft)) {
    level[(hasLeft[i]+1):M] <- 1 + level[(hasLeft[i]+1):M]
  }
  hasRight <- grep("[}]",model[-M])
  for (i in seq(along=hasRight)) {
    level[(hasRight[i]):M] <- -1 + level[(hasRight[i]):M]
  }
  level[M] <- 0
  prefix <- sapply(level, function(x) paste(rep(indent,max(0,x)),collapse=""))
  if (lineNumbers) {
    L10length <- floor(log10(length(prefix)))
    LN <- paste(formatC(seq(along=prefix), digits=L10length), ":", sep="")
    prefix <- paste(LN, prefix)
  }
  cat(paste(paste(prefix,model,sep=""),collapse="\n"))
  cat("\n")
  invisible(NULL)
}

