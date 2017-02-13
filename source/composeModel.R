# Apply substitutions and pseudo-functions to construct a model
# "model" may be a string with newlines, a string vector, or a filename.
#
# This also adds the option to use "\" at the end of a line to indicate
# line continuation (as opposed to R's ending a line if it can be
# considered syntactically complete).
#
# An outfile may be written.
# A character vector is returned.
#
composeModel = function(model, subs=NULL, cases=NULL, varList=NULL, wd=getwd(),
                        allowNoVarList=FALSE, warnUseless=FALSE, outFile=NULL,
                        allowStochExpr=TRUE) {
  if (!is.character(model)) stop("model must be of mode character")
  if (length(model)==1 && length(grep("\n",model,fixed=TRUE))==0) {
    model0 <- model
    oldwarn <- options("warn")
    options(warn=2)
    if (wd!=getwd()) model <- paste(wd,model,sep="/")
    model <- try(readLines(model, warn=TRUE), silent=TRUE)
    options(oldwarn)
    if (is(model,"try-error")) {
       if (length(grep("incomplete final line",model[1]))) {
         model <- readLines(model0, warn=FALSE)
       } else {
         stop("Error reading model file ", model0)
       }
    }
  } else {
    if (length(model)==1) model <- strsplit(model, "\n")[[1]]
  }

  ### Remove comments
  commentLines <- grep("^[[:blank:]]*#", model)
  if (length(commentLines)>0) model <- model[-commentLines]
  commentLines <- str_locate(model, "#")[,1]
  Sel <- which(!is.na(commentLines))
  if (length(Sel)>0) {
    model[Sel] <- substring(model[Sel],1,commentLines[Sel]-1)
    model[Sel] <- gsub("[[:space:]]*$", "", model[Sel])
  }

  ### Substitute constants for variables
  model <- subsText(model, subs, warnUseless=warnUseless)

  ### Implement IFCASE text selection and LC() and FOR() syntax
  model <- selectText(model, cases, varList, allowNoVarList=allowNoVarList)
  if (allowStochExpr) model <- stochExprSub(model)

  ### Clean up expressions across multiple lines: put on one line
  ### Lines are combined if the R expression is incomplete or the line
  ### ends in a backslash. (In most cases you need to type two backslashes.)
  ### (This will probably fail with some unusual line continuation styles.)
  ### Note: this can drop attributes, so LCPreSuf is handled specially
  LCPreSuf <- attr(model, "LCPreSuf")
  model <- gsub("[[:space:]]*$", "", model)
  toGrab <- which(substring(model, nchar(model)) %in% c("+","-","*","/",",",":","\\"))
  while(length(toGrab)>0) {
    if (toGrab[length(toGrab)]==length(model))
      stop("Model ends precipitously: ", model[length(model)])
    # Really need the "for" loop because of consecutive line continuations
    tG1 <- toGrab[1]
    for (i in rev(toGrab)) {
      Len <- nchar(model[i])
      if (substring(model[i],Len)=="\\") Len <- Len-1
      add <- if (substring(model[i],Len)==" ") "" else " "
      piece1 <- substring(model[i],1,Len)
      if (i!=tG1) piece1 <- str_trim(piece1)
      piece2 <- str_trim(model[i+1])
      model[i] <- paste(piece1, piece2, sep=add)
    }
    model <- model[-(toGrab+1)]
    toGrab <- which(substring(model, nchar(model)) %in% c("+","-","*","/",":","\\"))
  }
  if (!is.null(LCPreSuf)) attr(model, "LCPreSuf") <- LCPreSuf

  if (is.character(outFile)) {
    if (wd!=getwd() && length(grep("(/|\\\\)",wd)==0))
      outFile <- paste(wd, outFile ,sep="/")
    write(paste(model, collapse="\n"), outFile)
  }

  return(model)
}
