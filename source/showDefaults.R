###############################################################################
# Convenience function to show defaults in a model saved as a string with     #
# or in a text file.                                                          #
###############################################################################
showDefaults <- function(model, cases=NULL) {
  if (!is(model, "character")) stop("showDefaults() needs a character form of a model")
  if (length(grep("\n",model))>0) model = strsplit(model, split="\\n")[[1]]
  if (length(model)==1) {
    if (!file.exists(model))
      stop("Cannot find file ", model, " in ", getwd())
    model = readLines(model)
  }

  if (!is.null(cases)) 
    model <- selectText(model, cases, allowNoVarList=TRUE)

  fpRE = "[-+]?([0-9]*\\.?[0-9]+|([0-9]+\\.[0-9]+|[0-9]+){1}([eE][-+]?[0-9]+)?)"
  setMatch = paste("[.]?[a-zA-Z]+[._a-zA-Z0-9]*[:blank:]*=[:blank:]*(\\?|", fpRE,")", sep="")
  setText = str_locate_all(model, setMatch)
  setLines = which(sapply(setText,function(x)nrow(x)>0))
  if (length(setLines)==0) {
    return("No defaults")
  }
  allNames = NULL
  for (line in setLines) {
    mat = setText[[line]]
    for (loc in 1:nrow(mat)) { # can have multiple set()s per line
      setLoc = mat[loc,]
      substr = substring(model[line], setLoc[1], setLoc[2])
      eq = str_locate(substr, "=")
      nam = str_trim(substring(substr, 1, eq[1,1]-1))
      val = str_trim(substring(substr, eq[1,1]+1))
      allNames = rbind(allNames, data.frame(name=nam,value=val))
    }  # while looking for another set() on the same line
  } # for each line with at least one set()

  allNames = allNames[!duplicated(paste(allNames[,1],allNames[,2])),]
  allNames = allNames[order(as.character(allNames[,1])),]
  rownames(allNames) = NULL

  err = duplicated(allNames[,1])
  if (any(err)) allNames = cbind(allNames, duplicated=c("","Y         ")[1+err])

  return(allNames)
}

