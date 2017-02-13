# WinBUGS does not allow numeric expressions as arguments to
# stochastic functions.  This function replaces those with
# their numeric equivalents.
stochExprSub <- function(model) {
  stochLocs <- str_locate(model, "~")[,1]
  stochLines <- which(!is.na(stochLocs))
  for (line in stochLines) {
    this <- model[line]
    tilde <- str_locate(this, "~")[1,1]
    that <- substring(this, 1, tilde)
    RHS <- substring(this, tilde+1)
    censLoc <- str_locate(RHS,
      "I[[:blank:]]*\\(.*,.*\\)$")
      #"I[[:blank:]]*\\([[:blank:]]*[[:digit:]Ee.+-]*[[:blank:]]*,[[:blank:]]*[[:digit:]Ee.+-]*[[:blank:]]*\\)$")
    if (is.na(censLoc[1,1])) {
      censString <- ""
    } else {
      censString <- substring(RHS, censLoc[1,1])
      RHS <- str_trim(substring(RHS,1, censLoc[1,1]-1))
    }
    tmp <- strsplit(RHS,"")[[1]]
    paren1 <- str_locate(RHS, "\\(")[1,1]
    if (is.na(paren1)) next
    parenEnd <- rev(str_locate_all(RHS, "\\)")[[1]][,1])[1]
    if (is.na(parenEnd) || parenEnd!=nchar(RHS)) next
    args <- strsplit(substring(RHS, paren1+1, parenEnd-1),"")[[1]]
    inBrackets <- which(c(0,cumsum(as.numeric(args=="[") - as.numeric(args=="]")))==1) +
                  paren1 -1
    inParens <- which(c(0,cumsum(as.numeric(args=="(") - as.numeric(args==")")))==1) +
                  paren1 -1
    commas <- setdiff(str_locate_all(RHS, ",")[[1]][,1], c(inBrackets,inParens))
    pieces <- length(commas) + 1
    commas <- c(paren1, commas, parenEnd)
    that <- paste(that, substring(RHS, 1, paren1), sep="")
    envEB <- new.env(parent=baseenv())
    assign("pow",function(a,b)a^b, envir=envEB)
    for (i in 1:pieces) {
      piece <- substring(RHS, commas[i]+1, commas[i+1]-1)
      if (nchar(piece)==0) {
        that <- paste(that, ")", sep="")
      } else {
        val <- try(eval(parse(text=piece), envir=envEB), silent=TRUE)
        if (is(val,"try-error") || is(val,"function")) {
          that <- paste(that, piece, ifelse(i==pieces, ")", ", "), sep="")
        } else {
          that <- paste(that, format(val, scientific=FALSE), 
                        ifelse(i==pieces, ")", ", "), sep="")
        }
      }
    }
    # If user names variables as known functions, e.g., "c", this is miscoded, so repair:
    repair <- str_locate_all(that, "[.]Primitive\\([^)]+\\)")[[1]]
    if (nrow(repair)>0) {
      NC <- nchar(that)
      if (any(repair[,1]==1 | repair[,2]==NC))
        stop(gettextf("cannot repair '%s'", that))
      for (i in nrow(repair):1) {
        that <- paste(substring(that, 1, repair[i,1]-1),
                      substring(that, repair[i,1]+12, repair[i,2]-2),
                      substring(that, repair[i,2]+1), sep="")
      }
    }
    model[line] <- paste(that, censString, sep="")
  }
  return(model)
}


