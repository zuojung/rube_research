# Get variables from a simple expression.  Simple means that the only
#   parentheses or brackets, if any, are in the first and last positions.
# Non-arrays return "@" "@@", etc. as subtext based on arg. count, and
#   variables in "vars".
# Arrays return "$", "$$" etc. as subtext based on dim. plus "vars".
# Examples: 3+a*b/6 ==> text="@", vars=c("a","b")
#           (3+a*b, d/6) ==> text="@@", vars=c("a","b","d")
#           [4, 5] ==> text="$$", vars=""
#           3:k ==> text="@", vars="k"
#
expressionVars <- function(text, engine) {
  if (!is.character(text) || length(text)!=1) stop("Program errorl see Howard: " ,text)
  textLen = nchar(text)
  ch = substring(text, textLen)
  if (ch=="]") {
    if (textLen<2 || substring(text,1,1)!="[") return(NA)
    isArray = TRUE
    text = substring(text, 2, textLen-1)
    textLen = textLen - 2
  } else {
    isArray=FALSE
    if (ch==")") {
      if (textLen<3 || substring(text,1,1)!="(") return(NA)
      text = substring(text, 2, textLen-1)
      textLen = textLen - 2
    }
  }
  
  if (engine!="jags" && length(grep("[\\^]+", text))!=0) stop("^ is not allowed in winBUGS: use pow()")
  # Required in case of "[,]":
  nArgs = 1 + nrow(str_locate_all(text, ",")[[1]])
  text = str_trim(strsplit(text,",")[[1]])
  if (isArray) {
    subtext = paste(rep("$", nArgs), collapse="")
  } else {
    subtext = paste(rep("@", nArgs), collapse="")
  }
  
  vars = NULL
  if (length(text)==0) return(list(text=subtext, vars=unique(vars)))
  for (i in 1:length(text)) {
    parts = text[[i]]
    # First change (-)#e(-)# to +1 to simplify further analysis
    parts = gsub("[\\-]?[[:digit:]]+[.][[:digit:]]*[eE][\\-]?[[:digit:]+]", "+1", parts)
    parts = gsub("[\\-]?[[:digit:]]+[eE][\\-]?[[:digit:]+]", "+1", parts)
    if (!is.na(str_locate(text[i], "[-+*/]+[[:blank:]][-+*/]+")[1,1]))
      warning("Consecutive operators in ", text[i])
    for (op in c("+","-","*","/",":"))
      parts = str_trim(unlist(strsplit(parts, op, fixed=TRUE)))
    # NOTE: SHOULD BE IMPROVED
    is.number = c(grep("^[\\-]?[[:digit:]]+[L]?$", parts),
                  grep("^[\\-]?[[:digit:]]+[.]?[[:digit:]]*$", parts),
                  grep("^[\\-]?[[:digit:]]*[.]?[[:digit:]]+$", parts))
    is.fakenum = setdiff(grep("^[[:digit:]}]+", parts), is.number)
    if (length(is.fakenum)>0) stop("Bad number format: ", parts)
    if (length(is.number)>0) {
      vars = c(vars, parts[-is.number])
    } else {
      vars = c(vars, parts)
    }
  }
  return(list(text=subtext, vars=unique(vars)))
} # end expressionVars()

