##################################################################
## Parse an R-like formula into all components with appropriate ##
## colon-separated interactions.  E.g., ab+cd*e+(f+g+h)^2 gives ##
## ab, cd, e, cd:e, f, g, h, f:g, f:h, g:h.                     ##
## The return value is a list with an element for each term and ##
## the list elements are string vectors of the term components. ##
## Extra spaces are allowed.                                    ##
## Multiple ^n terms are allowed.  Redundancies are removed.    ##
## Some forms such as A/B and A*(B+C) are NOT allowed.          ##
## An intercept is included unless the formula contains "-1".   ##
## Other uses of "-" are not currently supported.               ##
## Note: (A:B)^n is treated as (A+B)^n, as is (A*B)^n.          ##
##################################################################
parseFormula <- function(f, maxIA=4) {
  if (length(f)!=1 || !is.character(f)) stop("Bad formula")
  f <- str_trim(f)
  dropInt <- FALSE
  loc <- str_locate_all(f, "^[[:blank:]]*[-][[:blank:]]*1[[:blank:]]*[+]")[[1]]
  if (nrow(loc)==1) {
    dropInt <- TRUE
    loc[1,2] <- loc[1,2]
    f <- multiFill(f, loc, "") 
  }
  loc <- str_locate_all(f, "[-][[:blank:]]*1[[:blank:]]*[+]")[[1]]
  if (nrow(loc)>1) stop("multiple minuses in formula: ", f)
  if (nrow(loc)==1) {
    dropInt <- TRUE
    loc[1,2] <- loc[1,2] - 1
    f <- multiFill(f, loc, "") 
  }
  loc <- str_locate_all(f, "[-][[:blank:]]*1")[[1]]
  if (nrow(loc)>1) stop("multiple minuses in formula: ", f)
  if (nrow(loc)==1) {
    dropInt <- TRUE
    f <- multiFill(f, loc, "") 
  }

  # Generate interaction of order IAlevel from main effects ME.
  # Return a string vector of main effects and interactions.
  # "subIA" is the next lower level of interactions.  It is
  # important to preserve the order of lower order terms for
  # housekeeping outside of this function.  
  # This function is inefficent.  
  genIA <- function(ME, IAlevel=2, maxIA=4) {
    if (IAlevel>maxIA) stop("formula ", f, " exceeds ", maxIA, " interactions")
    if (IAlevel<1) stop("IAlevel must be >0")
    if (IAlevel<2) return(ME)
    if (IAlevel>length(ME)) IAlevel=length(ME)
    tmp <- ME
    for (i in 2:IAlevel) tmp <- outer(tmp, ME, paste, sep=":")    
    tmp <- as.character(tmp)
    tmp = unique(sapply(tmp, 
                        function(s, Ord) {
                          vars <- strsplit(s,":")[[1]]
                          vars <- vars[order(unique(match(vars, Ord)))]
                          if (any(duplicated(vars))) return(NA)
                          return(paste(vars, collapse=":"))
                        }, Ord=ME))
    return(tmp[!is.na(tmp)])
  }

  # First expand a*b... wherever it occurs
  tmp <- str_locate_all(f, "[[:alpha:]][[:alnum:]._]*(\\*[[:alpha:]][[:alnum:]._]*)+")[[1]]
  if (nrow(tmp)>0) {
    starExpand <- function(text, maxIA=4) {
      MEs <- sapply(strsplit(text,"\\*")[[1]], str_trim)
      if (length(MEs)>maxIA) stop("formula ", f, " exceeds ", maxIA, " interactions")
      allTerms <- MEs
      for (i in 2:length(MEs))
        allTerms <- c(allTerms, genIA(MEs, i, maxIA))
      return(paste(unique(allTerms), collapse=" + "))
    }
    subs=NULL
    for (i in 1:nrow(tmp))
      subs <- c(subs, starExpand(substring(f, tmp[i,1], tmp[i,2]), maxIA=maxIA))
    f <- multiFill(f, tmp, subs)
  }

  # Next expand (a+b...)^n wherever it occurs
  # Note: a:b inside parenthesis is treated as a+b for these purposes.
  tmp <- str_locate_all(f, "\\([^)]*\\)[[:blank:]]*\\^[[:blank:]]*[[:digit:]]+")[[1]]
  if (nrow(tmp)>0) {
    formExpand <- function(text, maxIA=4) {
      tmp <- str_locate(text, "^\\(.*\\)[[:blank:]]*\\^")
      pwr <- as.numeric(str_trim(substring(text,tmp[1,2]+1)))
      if (pwr>maxIA) stop("formula ", f, " exceeds ", maxIA, " interactions")
      tmp <- str_locate(text, "^\\(.*\\)")
      form <- str_trim(substring(text, 2, tmp[1,2]-1))
      allTerms <- sapply(strsplit(form,"\\+")[[1]], str_trim)
      # split up any interactions
      tmp <- grep(":", allTerms)
      for (i in seq(along=tmp)) {
        allTerms <- c(allTerms, strsplit(allTerms[tmp[i]],":")[[1]])
        allTerms <- allTerms[-tmp]
      }
      MEs <- allTerms <- unique(allTerms)
      if (pwr>1) {
        for (i in 2:pwr)
          allTerms <- c(allTerms, genIA(MEs, i, maxIA))
      }
      return(paste(unique(allTerms), collapse=" + "))
    }
    subs <- NULL
    for (i in 1:nrow(tmp)) {
      subs <- c(subs, formExpand(substring(f, tmp[i,1], tmp[i,2]), maxIA=maxIA))
    }
    f <- multiFill(f, tmp, subs)
  }

  # Final check for validity and redundancy
  allTerms <- unique(sapply(strsplit(f,"\\+")[[1]], str_trim))
  tmp <- grep(":", allTerms)
  if (length(tmp)>0) {
    MEs <- allTerms[-tmp]
    IAs <- allTerms[tmp]
    revIAs <- sapply(strsplit(IAs,":"), function(x) paste(x[2],x[1],sep=":"))
    doubled <- outer(IAs,revIAs,"==")
    redundant <- apply(cbind(1:length(IAs),doubled),
                       MARGIN=1, function(x) any(x[2:(1+x[1])]))
    allTerms <- c(MEs, IAs[!redundant])
  }
  bad <- which(is.na(str_locate(allTerms, "^[[:alpha:]][[:alnum:]:._]*$")[,1]))
  if (any(bad)) stop("Bad terms in formula: ", paste(allTerms[bad],collapse=", "))

  # Convert terms (var and var:var formats) into a list
  lst <- lapply(allTerms, function(x) strsplit(x,":")[[1]])
  if (!dropInt) lst <- c(list(1), lst)

  return(lst)
}

