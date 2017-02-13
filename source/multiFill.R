#############################################################
# Fill substitution text (subs) into the text string vector #
# replacing the text specified by the corresponding rows of #
# locAllMat, e.g., created by str_locate_all in stringr.    #
# There cannot be NAs in locAllMat.                         #
#############################################################
multiFill <- function(text, locAllMat, subs) {
  if (length(text)!=1 || !is.character(text)) stop("Bad text in multiFill()")
  if (!is.matrix(locAllMat)) stop("locAllMat in multiFill() must be a matrix")
  if (any(is.na(locAllMat))) stop("NAs found in locAllMag in multiFill()")
  if (!is.character(subs)) stop("subs in multiFill() must be character")
  if (length(subs)!=nrow(locAllMat)) stop("Mismatched locAllMat and subs")
  if (nrow(locAllMat)==0) return(text)
  if (any(locAllMat[,2]>nchar(text))) stop("locations out of range in multiFill()")
  text = paste("*",text,"*",sep="")
  for (i in rev(1:nrow(locAllMat))) {
    text = paste(substring(text, 1, locAllMat[i,1]), subs[i],
                 substring(text, locAllMat[i,2]+2), sep="")
  }
  return(substring(text,2,nchar(text)-1))
}

