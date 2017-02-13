###############################################################################
# subsText() is a function to get substitute values for variables in the      #
# model syntax (WinBugs code as text).   In RUBE, model syntax can contain    #
# variables with default (numeric) values.  This makes for easier to read     #
# code because constants stand out better, and it is more efficient for       #
# running WinBUGS models because, particularly for hyperparameters, default   #
# values can be specified, but not hardcoded; then for prior sensitivity      #
# testing, you can explicitly change any of these values.                     #
# Variable substitution is based on the contruction FOO=bar, where "bar" is   #
# the default substitution value for "FOO", but WinBUGS will never see the    #
# "FOO" variable.                                                             #
# "FOO" is any valid R variable name and "bar" is any valid (atomic) R value. #
# "text" is any vector of character strings.                                  #
# "subs" is a named list of substitution text and their values.               #
# (If you really want no default value in the code, use "FOO=?".)             #
# If "warnUseless" is TRUE, a warning is generated if "subs" contains any     #
# variables not in the text.                                                  #
# Example: subsText("b1 ~ dnorm(MU.1=0, PREC.1=1e-4)", list(MU.1=5))          #
#          returns "b1 ~ dnorm(5, 1e-4)".                                     #
###############################################################################
subsText <- function(text, subs=NULL, warnUseless=FALSE) {
  #fpRE <- "[-+]?([0-9]*\\.?[0-9]+|([0-9]+\\.[0-9]+|[0-9]+){1}([eE][-+]?[0-9]+)?)"
  #setMatch <- paste("[.]?[a-zA-Z]+[._a-zA-Z0-9]*[:blank:]*=[:blank:]*(\\?|", fpRE,")", sep="")
  setMatch <- "[.]?[a-zA-Z]+[._a-zA-Z0-9]*[:blank:]*=[:blank:]*"
  if (!is.null(subs) && !(is.list(subs) && !is.null(names(subs)) &&
                        !any(names(subs)=="")))
    stop("subs must be NULL or a fully named list.")
  if (!is.null(subs)) subNames <- names(subs)
  setText <- str_locate(text, setMatch)
  setLines <- which(!is.na(setText[,1]))
  if (length(setLines)==0) {
    if (!is.null(subs) && warnUseless)
      warning("Useless subs: ", paste(subNames, collapse=", "))
    return(text)
  }
  allNames <- character(0)
  for (line in setLines) {
    while (TRUE) { # can have multiple set()s per line
      #setLoc <- str_locate(text[line], setMatch)[1,]
      #if (is.na(setLoc[1])) break
      #substr <- substring(text[line], setLoc[1], setLoc[2])
      #eq <- str_locate(substr, "=")
      #if (!is.na(eq[1,1])) {
      #  nam <- str_trim(substring(substr, 1, eq[1,1]-1))
      #  val <- str_trim(substring(substr, eq[1,1]+1))
      #} else {
      #  stop("Logic error: RE in subsText")
      #}
      setLoc <- str_locate(text[line], setMatch)[1,1]
      if (is.na(setLoc)) break
      ncl <- nchar(text[line])
      if (setLoc==ncl) stop("bad substitution attempt: ", str_trim(text[line]))
      # find end of substitution expression
      chars <- strsplit(text[line], "")[[1]]
      parenCnt <- 0
      eq <- str_locate(substring(text[line],setLoc), "=")[1,1]
      for (i in (setLoc+eq):ncl) {
        if (parenCnt==0 && chars[i]==",") {i <- i - 1; break}
        if (chars[i]=="(" && i!=ncl) {
          parenCnt <- parenCnt + 1
        } else if (chars[i]==")") {
          parenCnt <- parenCnt - 1
          if (parenCnt==-1) {i <- i - 1; break}
        } else if (i==ncl) {
          if (parenCnt==0) break
          stop("bad substitution attempt: ", str_trim(text[line]))
        } 
      }
      endLoc <- i
      val <- str_trim(substring(text[line], setLoc+eq, endLoc))
      nam <- str_trim(substring(text[line], setLoc, setLoc+eq-2))
      allNames <- c(allNames, nam)
      subLoc <- ifelse(is.null(subs), NA, match(nam, subNames))
      if (is.na(subLoc)) {
        if (val=="?") stop(nam, " is not supplied and has no default.")
        #val <- try(suppressWarnings(as.numeric(val)), silent=TRUE)
        envEB <- new.env(parent=baseenv())
        assign("pow",function(a,b)a^b, envir=envEB)
        val <- try(eval(parse(text=val), envir=envEB), silent=TRUE)
        if (is(val, "try-error") || is.na(val))
          stop("Cannot evaluate default value ", val, " for ", nam)
      } else {
        val <- subs[[subLoc]]
      }
      #text[line] <- paste(substring(text[line], 1, setLoc[1]-1),
      #                   format(val, scientific=FALSE),
      #                   substring(text[line], setLoc[2]+1), sep="")
      text[line] <- paste(substring(text[line], 1, setLoc-1),
                         format(val, scientific=FALSE),
                         substring(text[line], endLoc+1), sep="")
    }  # while looking for another set() on the same line
  } # for each line with at least one set()
  if (warnUseless && !is.null(subs)) {
    err <- setdiff(subNames,allNames)
    if (length(err)>0)
      warning("Useless subs: ", paste(err, collapse=", "))
  }
  return(text)
}

