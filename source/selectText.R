##################################################################
# selectText() handles conditional text (for model code).        #
# It also implements the LC() and FOR() and BASE() functions     #
# (see below).                                                   #
#                                                                #
# The "cases" argument is a string vector.  Each listed          #
# "variable name" is taken to be defined and all non-listed      #
# ones are not defined from the point of view of the arguments   #
# to IFCASE() and ELSEIFCASE() for the purpose of defining which #
# model syntax text is kept vs. discarded.                       #
# The "varList" argument is a list of named elements refered     #
# to in the argument lists of the pseudo-functions LC() and      #
# FOR() and BASE().  Each is a single string that looks like     #
# an R formula (see parseFormula() for restrictions).  If a      #
# string vector is supplied it is collapse with "+".             #
#                                                                #
# There is an in-line version of case selectionin the form of:   #
#    [bar1] IFCASE(foo) bar2 [ELSECASE bar3] ENDCASE [bar4]      #
# which takes code "bar1 bar2 bar4" if "foo" is in the "cases"   #
# vector, and "bar1 bar3 bar4" otherwise.                        #
#                                                                #
# For the multi-line case the format is:                         #
#    IFCASE(foo)                                                 #
#      [any lines]                                               #
#    ELSEIFCASE(bar)                                             #
#      [any lines]                                               #
#    ...                                                         #
#    [ELSECASE]                                                  #
#      [any lines]                                               #
#    ENDCASE                                                     #
#                                                                #
# Case ids (cases) must be a letter optionally followed by any   #
# additional letters and numbers.                                #
# Case text inside (ELSE)IFCASE() can be logical expresssions    #
# containing parentheses, "|" for "or", "&" for "and", and "!"   #
# for "not".                                                     #
# Nested blocks are allowed.                                     #
# No warning is given if a block matches more than one case      #
# (and the first matching block is chosen).                      #
# Note: Case text is case sensitive.                             #
#                                                                #
# The pseudo-function LC(prefix, formula, suffix="", index=NULL) #
# works in a fashion such that                                   #
#  LC(g, BFORM, mp, i)                                           #
# generates the model code                                       #
#  b0mp + bmalemp*male[i] + magemp*age[i] +                      #
#         bmaleagemp*male[i]*age[i].                             # 
# when varList is defined as                                     #
#  list(BFORM="male*age")                                        #
# A formula can end with "-1" to drop the intercept.             #
# A missing formula generates nothing if "-1" is defined, or     #
# an intercept otherwise.
#                                                                #
# The pseudo-function FOR (prefix, formula, suffix="", codetext) #
# works in a fashion such that                                   #
#  FOR(g, BFORM, mp, ? ~ dnorm(0,PREC.MP=0.001)                  #
# generates the model code                                       #
#  b0mp ~ dnorm(0,PREC.MP=0.001)                                 #
#  bmalemp ~ dnorm(0,PREC.MP=0.001)                              #
#  bagemp ~ dnorm(0,PREC.MP=0.001)                               #
#  bmaleagemp ~ dnorm(0,PREC.MP=0.001)                           #
# when varList is defined as                                     #
#  list(BFORM="male*age")                                        #
# A formula can end with "-1" to drop the intercept              #
#                                                                #
# The pseudo-function BASE(formula, index)                       #
# is useful for focusing on a baseline case for an interaction.  #
#  E.g., isBase[i] <- BASE(male:old:white, i)                    #
# generates the model code                                       #
#  isBase[i] <- (1-male[i])*(1-old[i])*(1-white[i])              #
# Note that the product is constructed using only the main       #
# effect terms of the formula.                                   #
# No intercept term is generated regarless of the presences of   #
# "-1".                                                          #
#                                                                #
# Because LC/FOR/BASE are handled after CASE, selected cases     #
# that do not include an LC and/or FOR obviate the need to       #
# include elements of varList mentioned in those pseudo-         #
# functions.                                                     #
# There can be more than one LC() per line, but not multiple     #
# FOR() or BASE() calls per line.                                #
#                                                                #
# The return value is the processed model text.  It may have     #
# attributes "missingVarList" which holds the names of missing   #
# variables, and "LCPreSuf" which is a 2-column matrix of the    #
# prefix and suffix for each LC() substitution.                  #
##################################################################

selectText <- function(text, cases=character(0), varList=NULL,
                      allowNoVarList=FALSE) {
  missingVarList <- NULL

  textLen <- length(text)
  if (textLen==0) return(text)

  text <- gsub("IFCASE[[:blank:]]*\\(", "IFCASE(", text)

  if (is.list(cases)) cases <- unlist(cases)
  if (length(cases)==0) cases <- character(0)
  if (length(cases)==1 && cases=="") cases <- character(0)
  if (!is.character(cases)) stop("selectText() needs 'cases' to be strings")
  if (!is.null(varList)) {
    if (!is.list(varList)) stop("varList must be a list")
    if (length(varList)>0) {
      if (is.null(names(varList)) || any(names(varList)==""))
        stop("All elements of varList must be named.")
      todo <- sapply(varList,function(x)length(x)>1)
      if (any(todo))
        for (i in which(todo)) 
          varList[[i]] <- paste(varList[[i]], collapse="+")
    }
  }

  # Evaluate all IFCASE and ELSEIFCASE conditions as strings
  ifLocMat <- str_locate(text, "IFCASE\\([[:alnum:]!().|&[:blank:]]+\\)")
  if (all(is.na(ifLocMat[,1]))) {
    noSelects <- TRUE
  } else {
    noSelects <- FALSE
    elseLocMat <- str_locate(text, "ELSECASE")
    elseifLocMat <- str_locate(text, "ELSEIFCASE\\([[:alnum:]!().|&[:blank:]]*\\)")
    if (any(!is.na(elseifLocMat[,1]))) # unmark ELSEIFCASE as IFCASE
      ifLocMat[!is.na(elseifLocMat[,1]),] <- NA
    endLocMat <- str_locate(text, "ENDCASE")
    ifLoc <- which(!is.na(ifLocMat[,1]))
    elseifLoc <- which(!is.na(elseifLocMat[,1]))
    elseLoc <- which(!is.na(elseLocMat[,1]))
    endLoc <- which(!is.na(endLocMat[,1]))
  }
 
  # Add additional conditions from single (in-line) IFCASE()s
  if (!noSelects) {
    inline <- intersect(ifLoc, endLoc)
    if (length(inline)==0) {
      inlineStrings <- NULL
    } else {
      err <- str_locate(text[inline], "ELSEIF")
      if (any(!is.na(err[,1]))) stop("In-line IFCASE() does not allow ELSEIF")
      ifLoc <- setdiff(ifLoc, inline)
      elseLoc <- setdiff(elseLoc, inline)
      endLoc <- setdiff(endLoc, inline)
      multiIfLocMat <- str_locate_all(text[inline], "IFCASE\\([[:alnum:]!()|&[:blank:]]+\\)")
      multiElseLocMat <- str_locate_all(text[inline], "ELSECASE")
      multiEndLocMat <- str_locate_all(text[inline], "ENDCASE")
      tmp <- matrix(unlist(multiIfLocMat), ncol=2, byrow=TRUE)
      inlineStrings <- substring(text[rep(inline, sapply(multiIfLocMat,nrow))],
                        7+tmp[,1], tmp[,2]-1)
    }
   }

  # Get case strings from inside parentheses after IFCASE and IFELSECASE
  if (!noSelects) {
    caseStrings <- rep(NA, textLen)
    if (length(ifLoc)>0) {
      caseStrings[ifLoc] <- substring(text[ifLoc], 7+ifLocMat[ifLoc,1], ifLocMat[ifLoc,2]-1)
      if (length(elseifLoc)>0)
        caseStrings[elseifLoc] <- substring(text[elseifLoc], 11+elseifLocMat[elseifLoc,1],
                                           elseifLocMat[elseifLoc,2]-1)
    }

    # Get all in-use CASE variables and assign per "cases" into environment varEnv
    if (all(is.na(caseStrings))) {
      vars <- unlist(strsplit(inlineStrings, "[^[:alnum:].]+"))
    } else {
      vars <- unlist(strsplit(c(inlineStrings,caseStrings[!is.na(caseStrings)]), "[^[:alnum:].]+"))
    }  
    vars <- unique(vars[!vars==""])
    Tvars <- intersect(vars, cases)
    Fvars <- setdiff(vars, cases)
    varEnv <- new.env()
    for (v in Tvars) assign(v, TRUE, envir=varEnv)
    for (v in Fvars) assign(v, FALSE, envir=varEnv)
  }

  # Define case evaluation function
  TFNA <- function(str) {
    rslt <- try(eval(parse(text=str), envir=varEnv), silent=TRUE)
    if (is(rslt, "try-error")) return(NA)
    return(rslt)
  }

  # Evaluate all inline IFCASE (ELSE) ENDCASE statements
  # (No nesting allowed)
  if (!noSelects) {
    for (i in inline) {
      loc1 <- str_locate(text[i], "IFCASE\\([[:alnum:]!().|&[:blank:]]+\\)")[1,]
      if (is.na(loc1[1])) next
      while (TRUE) {
        loc2 <- str_locate(text[i], "ELSECASE")[1,]
        loc3 <- str_locate(text[i], "ENDCASE")[1,]
        if (is.na(loc3[1])) stop("No ENDCASE:", text[i])
        case <- substring(text[i],loc1[1]+7,loc1[2]-1)
        caseEval <- TFNA(case)
        if (is.na(caseEval)) stop("Can't evaluate IFCASE(): ", text[i])
        pre <- substring(text[i],1,loc1[1]-1)
        post <- substring(text[i],loc3[2]+1)
        if (is.na(loc2[1]) || loc2[1]>loc3[1]) {
          if (caseEval) {
            mid  <- substring(text[i], loc1[2]+1, loc3[1]-1)
          } else {
            mid  <- ""
          }
        } else {
          if (caseEval) {
            mid  <- substring(text[i], loc1[2]+1, loc2[1]-1)
          } else {
            mid  <- substring(text[i], loc2[2]+1, loc3[1]-1)
          }
        }
        text[i] <- paste(pre, mid, post, sep="")
        loc1 <- str_locate(text[i], "IFCASE\\([[:alnum:]!().|&[:blank:]]+\\)")[1,]
        if (is.na(loc1[1])) break
      }
    }
  }

  # Evaluate all IFCASE and ELSEIFCASE statements
  noSelects <- !exists("caseStrings") || all(is.na(caseStrings))
  if (!noSelects) {
    evalAll <- sapply(caseStrings[!is.na(caseStrings)], TFNA)
    if (any(is.na(evalAll)))
      stop("Cannot evaluate expression within IF(ELSE)CASE: ",
           paste(names(evalAll)[which(is.na(evalAll))[1]]))
    caseStrings[!is.na(caseStrings)] <- substring(as.character(evalAll),1,1)

    # Replace all IFCASE() and ELSEIFCASE() and ELSECASE() to hold T or F
    origText <- text
    text[ifLoc] <- paste("IFCASE(", caseStrings[ifLoc], ")", sep="")
    text[elseifLoc] <- paste("ELSEIFCASE(", caseStrings[elseifLoc], ")", sep="")
    text[elseLoc] <- "ELSEIFCASE(T)"
    elseifLoc <- c(elseifLoc, elseLoc)

    # Iteratively select text for first IFCASE() found  
    while (TRUE) {
      ifLocMat <- str_locate(text, "IFCASE\\([TF]{1}\\)")
      elseifLocMat <- str_locate(text, "ELSEIFCASE\\([TF]{1}\\)")
      if (any(!is.na(elseifLocMat[,1]))) # unmark ELSEIFCASE as IFCASE
        ifLocMat[!is.na(elseifLocMat[,1]),] <- NA
      endLocMat <- str_locate(text, "ENDCASE")
      ifLoc <- which(!is.na(ifLocMat[,1]))
      elseifLoc <- which(!is.na(elseifLocMat[,1]))
      endLoc <- which(!is.na(endLocMat[,1]))
      if (length(ifLoc)==0) break
      start <- min(ifLoc)

      # Process IFCASE outer nested block
      cs <- cumsum((!is.na(ifLocMat[,1])) - (!is.na(endLocMat[,1])))
      tmp <- cs[start:length(cs)]==0
      if (!any(tmp)) stop("Can't find ENDCASE for ", str_trim(origText[start]))
      end <- start-1+which(tmp)[1]
      candidate <- setdiff(start:end, which(cs>1))
      blockTest <- c(start, intersect(candidate, which(!is.na(elseifLocMat[,1]))))
      blockStart <- blockTest + 1
      blockEnd <- c(intersect(candidate, which(!is.na(elseifLocMat[,1]))-1), end-1)
      TF <- substring(text[blockTest[1]],8,8)
      if (length(blockTest>1)) TF <- c(TF, substring(text[blockTest[-1]],12,12))
      if (!any(TF=="T")) {
        text <- as.character(text[-c(start:end)])
        origText <- as.character(origText[-c(start:end)])
      } else {
        Sel <- which(TF=="T")[1]
        text <- as.character(text[-setdiff(start:end, blockStart[Sel]:blockEnd[Sel])])
        origText <- as.character(origText[-setdiff(start:end, blockStart[Sel]:blockEnd[Sel])])
      }
    } # end while TRUE
  }

  # Expand LC() according to LC(prefix, varVec, suffix="", IAlevel=1, [index=NULL])
  # Note: any spacing is optional between "LC" and "(".
  # Note: must be non-greedy in finding ")" that ends "LC()".
  LCLoc <- str_locate(text, "LC[[:blank:]]*\\([^)]*\\)")[,1]
  LCLines <- which(!is.na(LCLoc))
  LCPreSuf <- NULL
  for (line in LCLines) {
    LCLocMat <- str_locate_all(text[line], "LC[[:blank:]]*\\([^\\()]*\\)")[[1]]
    for (item in rev(1:nrow(LCLocMat))) {
      str <- substring(text[line], LCLocMat[item,1]+2, LCLocMat[item,2]-1)
      tmp <- str_locate(str, "\\(")[1,][1]
      str <- substring(str, tmp+1)
      if (length(grep("['\"]",str))>0) 
        stop("Bad LC() call: ", text[line], " Quotes not allowed")
      args <- sapply(strsplit(str, ",")[[1]], str_trim)
      nargs <- length(args)
      if (nargs<2 || nargs>4) 
         stop("Bad LC() call: ", text[line], "; need 2 to 4 arguments")
      prefix <- args[1]
      hasInt <- FALSE
      if (is.null(varList) || is.null(varList[[args[2]]])) {
        if (allowNoVarList) {
          missingVarList <- unique(c(missingVarList, args[2]))
          #allVars <- list(paste("MISSING.",args[2],sep=""))
          allVars <- "0"
        } else {
          stop("In \"", str_trim(text[line]),"\", ", args[2], " is not in varList.")
        }
      } else {
        allVars <- parseFormula(varList[[args[2]]])
        if (length(allVars[[1]])==1 && allVars[[1]]==1) {
           hasInt <- TRUE
           allVars <- allVars[-1]
        }
      }
      if (nargs<3) {
        suffix <- ""
      } else {
        suffix <- args[3]
      }
      if (nchar(prefix)==0 && nchar(suffix)==0) 
        stop("need prefix and/or suffix in LC() call: ", text[line])
      if (nargs<4) {
        index <- ""
      } else {
        index <- paste("[", args[4], "]", sep="")
      }
      parms <- sapply(allVars, 
                      function(x, before, after) {
                        tmp <- paste(x,collapse="")
                        return(paste(before,tmp,after,sep="",collapse=""))
                      },
                      before=prefix, after=suffix)
      LCPreSuf <- rbind(LCPreSuf, c(prefix,suffix))
      rownames(LCPreSuf)[nrow(LCPreSuf)] <- args[[2]]
      if (length(allVars)==1 && allVars=="0") {
        subtext <- "b0"
      } else {
        vars <- sapply(allVars, 
                       function(x, ind) {
                         tmp <- paste(x,index,sep="")
                         return(paste(tmp,collapse="*"))
                       },
                       ind=index)
        subtext <- paste(parms, vars, sep="*", collapse=" + ")
      }
      if (hasInt) {
        if (prefix=="") 
          stop("Bad LC() call: ", text[line], "; prefix is not optional when intercept is present")
        subtext <- paste(prefix,"0",suffix," + ",subtext,sep="")
      }
      text[line] <- paste(substring(text[line],1,LCLocMat[item,1]-1),
                         subtext,
                         substring(text[line],LCLocMat[item,2]+1), sep="")
    }  # for each LC() call on a given line (in reverse order)
  }  # for each line with an LC() call

  # Expand FOR() according to FOR(prefix, varVec, suffix="", codeText)
  # Only one FOR() per code line is allowed, but multiple "?" fill-in
  # codes are allowed in the FOR() codeText. 
  FORLocMat <- str_locate(text, 
     "FOR[[:blank:]]*\\([[:blank:]]*[^,]*[,][^,]*[,][^,]*[,].*\\)")
  FORLines <- which(!is.na(FORLocMat[,1]))
  for (line in rev(FORLines)) {  # reverse because lines are being added
    if (str_trim(substring(FORLocMat[line,1],FORLocMat[line,2]))!="")
      stop("In ", text[line], " FOR statement must be the only thing on the line.")
    str <- substring(text[line], FORLocMat[line,1]+3, FORLocMat[line,2]-1)
    tmp <- str_locate(str, "\\(")[1,][1]
    str <- substring(str, tmp+1)
    if (length(grep("['\"]",str))>0) stop("Bad FOR() call: ", text[line], " Quotes not allowed.")
    # could be commas in argument 4 (code)
    commas <- str_locate_all(str,",")[[1]][,1]
    args <- c(str_trim(substring(str,1,commas[1]-1)),
             str_trim(substring(str,commas[1]+1,commas[2]-1)),
             str_trim(substring(str,commas[2]+1,commas[3]-1)),
             str_trim(substring(str,commas[3]+1)))
    if (nchar(args[2])==0) stop("no formula in FOR() call: ", text[line])
    prefix <- args[1]
    suffix <- args[3]
    if (nchar(prefix)==0 && nchar(suffix)==0) 
       stop("need prefix and/or suffix in FOR() call: ", text[line])
    if (is.null(varList) || is.null(varList[[args[2]]])) {
      if (allowNoVarList) {
        missingVarList <- unique(c(missingVarList, args[2]))
        parms <- paste(prefix, "0", suffix, sep="", collapse="")
      } else {
        stop("In \"", str_trim(text[line]),"\", ", args[2], " is not in varList.")
      }
    } else {
      allVars <- parseFormula(varList[[args[2]]])
      if (prefix=="") 
        stop("Bad FOR() call: ", text[line], "; prefix is not optional when intercept is present")
      parms <- sapply(allVars, 
                      function(x, before, after) {
                        tmp <- paste(x,collapse="")
                        return(paste(before,tmp,after,sep="",collapse=""))
                      },
                      before=prefix, after=suffix)
       if (length(allVars[[1]])==1 && allVars[[1]]==1)  {
         if (prefix=="") 
           stop("Bad FOR() call: ", text[line], "; prefix is not optional when intercept is present")
         parms[1] <- paste(prefix,"0",suffix,sep="",collapse="")
       }
    }
    code <- args[4]
    QMLoc <- str_locate_all(code, "\\?")[[1]]
    if (nrow(QMLoc)==0) stop("missing \'?\' in ",text[line])
    K <- length(parms)
    # Need one line per parameter:
    newtext <- NULL
    for (k in 1:K) {
      indent <- paste(rep(" ",max(0,FORLocMat[line,1]-1)),sep="",collapse="")
      newtext <- c(newtext, paste(indent, 
         multiFill(code, QMLoc, rep(parms[k],nrow(QMLoc))), sep=""))
    }
    if (line==length(text)) {  
      tmp <- NULL  # (Should not be needed if text has "}" as it's last line.)
    } else {
      tmp <- text[(line+1):length(text)]
    }
    text <- c(text[1:(line-1)], newtext, tmp)
  }  # for each line with an FOR() call
  
  # Expand BASE() according to BASE(formula, index) giving, e.g.,
  #    isBase[i] <- (1-v1[i])*(1-v2[i]).
  # Note: currently only one BASE per line is supported.
  BASELoc <- str_locate(text, 
    "BASE[[:blank:]]*\\([^,]+[,][[:alnum:][:blank:]._]*\\)")
  BASELines <- which(!is.na(BASELoc[,1]))
  for (line in BASELines) {
    str <- substring(text[line], BASELoc[line,1]+4, BASELoc[line,2]-1)
    tmp <- str_locate(str, "\\(")[1,][1]
    str <- substring(str, tmp+1)
    if (length(grep("['\"]",str))>0) 
      stop("bad BASE() call ", text[line], ": quotes not allowed")
    commas <- str_locate_all(str,",")[[1]][,1]
    if (length(commas)>1) stop("BASE() call needs one comma: ", text[line])
    args <- c(str_trim(substring(str,1,commas[1]-1)),
             str_trim(substring(str,commas[1]+1)))
    if (nchar(args[1])==0) stop("no formula in BASE() call: ", text[line])
    if (is.null(varList) || is.null(varList[[args[1]]])) {
      if (allowNoVarList) {
        missingVarList <- unique(c(missingVarList, args[1]))
        varVec <- paste("MISSING.",args[1],sep="")
      } else {
        stop("In \"", str_trim(text[line]),"\", ", args[1], " is not in varList.")
      }
    } else {
      varVec <- parseFormula(str_trim(varList[[args[1]]]))
      if (length(varVec[[1]])==1 && varVec[[1]]==1) varVec <- varVec[-1]
      tmp <- sapply(varVec,length)
      if (sum(tmp==1)==0)
        stop("no main effects in formula for BASE() call: ", text[line])
      varVec <- unlist(varVec[which(tmp==1)])
    }
    index <- paste("[", str_trim(args[2]), "]", sep="")
    subtext <- paste(paste("(1-",varVec,index,")",sep=""), collapse=" * ")
    text[line] <- paste(substring(text[line],1,BASELoc[line,1]-1),
                       subtext,
                       substring(text[line],BASELoc[line,2]+1), sep="")
  }  # for each line with an BASE() call

  if (!is.null(missingVarList)) attr(text,"missingVarList") <- missingVarList
  if (!is.null(LCPreSuf)) attr(text,"LCPreSuf") <- LCPreSuf
  return(text)
} # End of selectText() function

