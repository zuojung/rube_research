# Recursive expression parser takes a single string input and
#   extracts all variable names, marking arrays with one or more
#   "$" characters to specify the dimension, and functions with
#   one or more "@" characters to specify the number of arguments.
#   The extracted variable names are appended to the string vector
#   "varList" and returned.  A numeric constant expression returns "".
#   Unexpected input returns NA.
# E.g., parseExpression("exp(3+x[i,j]) + log(5*(exp(ab+m))) + sum(start:end)", c("i","j","k"))
# [1] "i"  "j"  "k"  "ab"  "m"  "exp@"  "x$$"  "start"  "end"  "log@"  "sum@"
#
parseExpression <- function(text, varList=NULL, engine) {
  if (text=="dflat()") return(c(varList, "dflat")) # special case of no arguments
  ATOMIC_VAR <- "^[[:alpha:]]+[[:alnum:].]*$"
  #text <- gsub(" ", "",text)
  textLen <- nchar(text)
  if (textLen==0) return(NA)
  
  # Check parenthesis and bracket balance
  # E.g., from x[4] + 3*(4+exp(1+t[3+g[4]])-log(1-t)) make
  # pLevel     00000000011111122222222222221111222221
  # bLevel     01110000000000000001111222100000000000
  # fullLevel  01110000011111122223333444321111222221
  pLevel <- bLevel <- rep(0, textLen)
  ch <- substring(text,1,1)
  if (ch%in%c(")","]","{","}")) return(NA)
  if (ch=="(") pLevel[1] <- 1
  if (ch=="[") bLevel[1] <- 1
  pdrop <- bdrop <- 0
  if (textLen>1) {
    for (i in 2:textLen) {
      ch <- substring(text,i,i)
      if (ch%in%c("{","}")) return(NA)
      if (ch=="(") {
        if (pdrop==1) return(NA)
        pLevel[i] <- pLevel[i-1] + 1
      } else if (ch==")") {
        pLevel[i] <- pLevel[i-1] - pdrop
        pdrop <- 1
        start <- max(which(pLevel[1:(i-1)]==pLevel[i-1]))
        if (any(bLevel[start:(i-1)]!=bLevel[start])) return(NA)
      } else {
        pLevel[i] <- pLevel[i-1] - pdrop
        pdrop <- 0
      }
      if (ch=="[") {
        bLevel[i] <- bLevel[i-1] + 1
      } else if (ch=="]") {
        bLevel[i] <- bLevel[i-1] - bdrop
        bdrop <- 1
        start <- max(which(bLevel[1:(i-1)]==bLevel[i-1]))
        if (any(pLevel[start:(i-1)]!=pLevel[start])) return(NA)
      } else {
        bLevel[i] <- bLevel[i-1] - bdrop
        bdrop <- 0
      }
    }
  }
  if (pdrop!=0 && pLevel[textLen]!=1) return(NA)
  if (bdrop!=0 && bLevel[textLen]!=1) return(NA)
  if (!(pLevel[textLen]==0 || (pLevel[textLen]==1&&substring(text,textLen)==")"))) return(NA)
  if (!(bLevel[textLen]==0 || (bLevel[textLen]==1&&substring(text,textLen)=="]"))) return(NA)
  if (any(pLevel<0) || any(bLevel<0) ) return(NA)

  # Find pieces (recursively)
  fullLevel <- pLevel + bLevel
  if (length(fullLevel)==0 || any(is.na(fullLevel)) || any(fullLevel<0))
      stop("Call Howard about fullLevel; ",fullLevel)
  Mn <- min(fullLevel)
  if (Mn>0) fullLevel <- fullLevel - Mn
  Mx <- max(fullLevel)
  if (Mx==0) {
    tmp <- expressionVars(text, engine)
    vars <- unique(c(varList, tmp$vars))
    vars <- setdiff(vars,"@")
    if (length(vars)==0) vars <- ""
    return(vars)
  } else {
    level <- Mx
    starts <- which(fullLevel[1:textLen]==level & c(-1,fullLevel[1:(textLen-1)])!=level)
    ends <- which(fullLevel[1:textLen]==level & c(fullLevel[2:textLen],-1)!=level)
    Nsegs <- length(starts)
    if (Nsegs!=length(ends)) return(NA)
    other <- cbind(c(1,ends[-Nsegs]+1), starts-1)
    if (ends[Nsegs]!=textLen) other <- rbind(other, c(ends[Nsegs]+1,textLen))
    if (starts[1]==1) other <- other[-1,,drop=FALSE]
    other <- apply(other, 1, function(x) substring(text,x[1],x[2]))
    rslt <- apply(cbind(starts,ends), 1,
                  function(x) {
                    tmp <- substring(text, x[1], x[2])
                    return(expressionVars(tmp, engine))
                  }
                 )
    if (any(sapply(rslt,is.na))) {print("rsltNA");return(NA)}
    subtext <- sapply(rslt, function(x)x[[1]])
    vars <- unlist(sapply(rslt, function(x)x[[2]]))
    if (length(vars)==0) vars <- ""
    if (starts[1]==1) other <- c("",other)
    Nother <- length(other)
    Nsubtext <- length(subtext)
    if (Nother<Nsubtext) other <- c(other, rep("",Nsubtext-Nother))
    if (Nsubtext<Nother) subtext <- c(subtext, rep("",Nother-Nsubtext))
    text <- paste(unlist(rbind(other, subtext)), collapse="")
    varList <- c(varList,vars)
    tmp <- parseExpression(text,varList,engine)
    return(tmp)  # (could be NA if recursive parseExpression() fails)
  }
  
  return(NULL) # never gets here
} # end parseExpression()

