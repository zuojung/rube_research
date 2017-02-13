# Take a single text string (from cleanCode()) and determine if it is
#   "model", "for", "endfor", "assign", or "stochastic" and return the
#   type with a descriptive analysis.
# E.g., extractSyntax("for (i in j:exp(m*pow(r,2))) {")
# $sType      [1] "for"
# $text       [1] "for (i in j:exp(m*pow(r,2))) {"
# $index      [1] "i"
# $startItems [1] "j"
# $endItems   [1] "r"     "m"     "pow@@" "exp@"
#
extractSyntax <- function(text, engine=c("WinBUGS","jags")) {
  engine <- match.arg(engine)
  if (engine=="WinBUGS") {
    validDists <- c("dbern@", "dbeta@@", "dbin@@","dnegbin@@","dcat@","dmulti@@","dchisq@",
                    "ddexp@@","dlogis@@","dlnorm@@","ddirch@","dexp@","dgamma@@",
                    "gen.gamma@@@","dmnorm@@","dnorm@@","dpar@@","dpois@","dt@@@","dunif@@",
                    "dweib@@","dwish@@", "dmt@@@",
                    # From http://www.winbugs-development.org.uk/
                    "djl.dnorm.trunc@@@@", "cj.dgumbel@@","nd.dpolyweib@@@",
                    # partially documented
                    "dflat",
                    # from geoBUGS
                    "car.normal@@@@", "car.l1@@@@", "car.proper@@@@@@@",
                    "spatial.exp@@@@@@", "spatial.disc@@@@@", "spatial.pred@@@@",
                    "spatial.unipred@@@@", "pois.conv@", "mv.car@@@@")
  } else {
    validDists <- c("dbeta@@","dbinom@@","dchisq@","dchisqr@","ddexp@@","dexp@","df@@",
                    "dgamma@@","dgen.gamma@@@","dlogis@@","dlnorm@@","dnchisqr@@",
                    "dnorm@@","dpar@@","dt@@@","dunif@@","dweib@@","dweibull@@",
                    "dbetabin@@@","dbern@","dbin@@","dcat@","dyhper@@@@",
                    "dnegbin@@","dnbinom@@","dpois@",
                    "dinterval@@","dmstate@@@","dnormmix@@@",
                    "ddirch@","ddirich@","dmnorm@@","dwish@@","dmt@@@","dmulti@@")
  }
  # Censor defaults: NA means not applicable, NaN means parameter dependant
  distCensDefault <- cbind(c(NA,0,NA,0,NA,NA,0,
                             0,-Inf,0,NA,0,0,
                             0,-Inf,-Inf,NaN,0,-Inf,NaN,
                             0,NA,-Inf,NA,NA,NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                           c(NA,1,NA,Inf,NA,NA,Inf,
                             Inf,Inf,Inf,NA,Inf,Inf,
                             Inf,Inf,Inf,Inf,Inf,Inf,NaN,
                             Inf,NA,Inf,NA,NA,NA, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  validAssignFuncs <- c("logit","log","cloglog","probit")
  if (engine=="WinBUGS") {
    validFunctions <- c("abs@","cloglog@","cos@","cut@","equals@@","exp@","inprod@@",
                        "interp.lin@@@","inverse@","log@","logdet@","logfact@","loggam@",
                        "logit@","max@@","mean@","min@@","phi@","pow@@","sin@","probit@",
                        "sd@","sqrt@","step@","sum@","rank@@","ranked@@","round@","trunc@",
                        # From http://www.winbugs-development.org.uk/
                        "djl.3comp.disp@","djl.pkIVbol3@@@","djl.2comp.disp@","djl.pkIVbol2@@@",
                        "sparse.mat.vec.product@@@@","elicitor.piecewise@@@@@","minmax@")
                        # "I", "for" dealt with specially
  } else {
    validFunctions <- c("abs@","arccos@","acos@","arccosh@","acosh@","arcsin@","asin@",
                        "arcsinh@","asinh@","arctan@","atan@","arctanh@", 
                        "cos@","cosh@","cloglog@",
                        "dbern@@", "dbeta@@@", "dbin@@@", "dchisqr@@", "ddexp@@@", "dexp@@",
                        "df@@@", "dgamma@@@", "dgen.gamma@@@@", "dhyper@@@@@", "dlogis@@@",
                        "dlnorm@@@", "dnegbin@@@", "dnchisqr@@@", "dnorm@@@", "dpar@@@",
                        "dpois@@", "dt@@", "dweib@@@",
                        "equals@@","exp@","icloglog@","ifelse@@@",
                        "ilogit@","inprod@@","interp.lin@@@","inverse@","log@","logdet@",
                        "logfact@","loggam@","logit@","length@","dim@",
                        "max@","max@@","max@@@","max@@@@","max@@@@@",  # IMPROVE THIS!!!!
                        "min@","min@@","min@@@","min@@@@","min@@@@@",  # IMPROVE THIS!!!!
                        "mean@","phi@","pow@@","probit@","rank@","round@","sd@","sin@","sinh@",
                        "sort@","sqrt@","step@","sum@","t@","tan@","tanh@","trunc@",
                        "pbern@@", "qbern@@", "pbeta@@@", "qbeta@@@", "pbin@@@", "qbin@@@", 
                        "pchisqr@@", "qchisqr@@", "pdexp@@@", "qdexp@@@", "pexp@@", "qexp@@",
                        "pf@@@", "qf@@@", "pgamma@@@", "qgamma@@@", "pgen.gamma@@@@", "qgen.gamma@@@@",
                        "phyper@@@@@", "qhyper@@@@@", "plogis@@@", "qlogis@@@", "plnorm@@@", "qlnorm@@@",
                        "pnegbin@@@", "qnegbin@@@", "pnchisqr@@@", "qnchisqr@@@",
                        "pnorm@@@", "qnorm@@@", "ppar@@@", "qpar@@@", "ppois@@", "qpois@@",
                        "pt@@", "qt@@", "pweib@@@", "qweib@@@")
                        # "for" dealt with specially
    ## NEED TO HANDLE a %*% b matrix multiplication and logical operators!!!!!!!!!!!
  }

  # A function to check function calls (returns variables used and error messages
  #   for non-supported functions and/or bad argument counts).
  checkFunctions <- function(strvec) {
    if (length(strvec)==0) return(list(vars=strvec, errMsg=NULL))
    if (!is(strvec,"character")) stop("Howard screwed up: strvec not character")
    loc <- str_locate(strvec, "@")[,1]
    isFun <- !is.na(loc)
    if (!any(isFun)) return(list(vars=strvec, errMsg=NULL))
    funs <- strvec[isFun]
    strvec <- strvec[!isFun]
    loc <- loc[isFun]
    basefuns <- substring(funs, 1, loc)
    trueAt1 <- str_locate(validFunctions, "@")[,1]
    nonfuns <- is.na(match(basefuns, substring(validFunctions,1,trueAt1)))
    errMsg <- NULL
    if (any(nonfuns)) {
      errMsg <- paste("Not a valid", engine, "function:",
                paste(substring(basefuns[nonfuns],1,nchar(basefuns[nonfuns])-1), collapse=", "))
      basefuns <- basefuns[!nonfuns]
      funs <- funs[!nonfuns]
    }
    badArgs <- is.na(match(funs, validFunctions))
    if (!any(badArgs)) {
      return(list(vars=strvec, errMsg=errMsg))
    }
    errMsg <- c(errMsg, paste("Improper argument count:",
                paste(substring(basefuns[badArgs],1,nchar(basefuns[badArgs])), collapse=", ")))
    return(list(vars=strvec, errMsg=errMsg))
  }

  if (length(text)>1 || class(text)!="character")
    stop("extractSyntax requires a single string as input")
  ATOMIC_VAR <- "^[[:alpha:]]+[[:alnum:]._]*$"
  text <- gsub("([[:alnum:]])[[:blank:]]+\\(", "\\1\\(", text)
  textLen <- nchar(text)

  ########################################
  ### Check for "model {" and "data {" ###
  ########################################
  parts <- str_trim(strsplit(text, " ")[[1]])
  if (parts[1]=="model{" || (parts[1]=="model" && parts[2]=="{")) {
    return(list(sType="model", text=text))
  }
  if (parts[1] == "data{" || (parts[1] == "data" && parts[2] == "{")) {
    return(list(sType = "data", text = text))
  }

  ####################################################
  ### Check for "for ( var in index1 : index2 ) {" ###
  ####################################################
  tmp <- str_locate(text, "for\\s*\\(")[1,]
  if (!is.na(tmp[1]) && tmp[1]==1) {
    tmp2 <- str_locate_all(text, "\\)\\s*\\{")[[1]]
    tmpR <- nrow(tmp2)
    if (tmp[2]==textLen || tmpR==0 || tmp2[tmpR,1]<=tmp[2] || tmp2[tmpR,2]!=textLen)
      return(list(sType="error", text=text, errMsg="Bad 'for' statement syntax."))
    rem <- substring(text, tmp[2]+1, tmp2[tmpR,1]-1)
    parts <- str_trim(strsplit(rem, " in")[[1]])
    if (length(parts)>2)
      parts[2] <- paste(parts[-1], collapse=" in")
    if (length(parts)!=2)
      return(list(sType="error", text=text, errMsg="'for' statement requires 'in'."))
    index <- parts[1]
    tmp <- str_locate(index, ATOMIC_VAR)
    if (nrow(tmp)!=1 || is.na(tmp[1,1]) || tmp[1,1]!=1 || tmp[1,2]!=nchar(index))
      return(list(sType="error", text=text, errMsg="Invalid index variable in 'for' statement."))
    fromTo <- strsplit(parts[2], ":")[[1]]
    if (length(fromTo)!=2)
      return(list(sType="error", text=text, errMsg="'for' statment requires ':'."))
    start <- parseExpression(fromTo[1], varList=NULL, engine)
    if (is.null(start)) stop("Howard goofed (null start)")
    if (any(is.na(start)))
      return(list(sType="error", text=text, errMsg="Cannot parse 'from' value of 'for' statment."))
    end <- parseExpression(fromTo[2], varList=NULL, engine)
    if (is.null(end)) stop("Howard goofed (null end)")
    if (any(is.na(end)))
      return(list(sType="error", text=text, errMsg="Cannot parse 'to' value of 'for' statment."))
    
    tmp <- checkFunctions(start)
    if (!is.null(tmp$errMsg)) {
      return(list(sType="error", text=text, errMsg=tmp$errMsg))
    } else {
      start <- tmp$vars
    }
    tmp <- checkFunctions(end)
    if (!is.null(tmp$errMsg)) {
      return(list(sType="error", text=text, errMsg=tmp$errMsg))
    } else {
      end <- tmp$vars
    }
    return(list(sType="for", text=text, index=index, startItems=start, endItems=end))
  } # end found "for"

  ###########################################################
  ### Check for "end for" (as isolated right curly brace) ###
  ###########################################################
  if (text=="}") return(list(sType="endfor"))

  ###############################################################################
  ### Check for reverse "assignment"; could be, e.g., a -> dnorm(x,log(y[2])) ###
  ###############################################################################
  if (length(grep("->", text))!=0)
    return(list(sType="error", text=text, errMsg="Reverse assignment (->) is not allowed."))
  
  #################################################################################
  ### Check for "assignment"; could be, e.g., logit(a[i+K]) <- exp(x,log(y[2])) ###
  #################################################################################
  tmp <- grep("<-", text)
  if (length(tmp)>1) return(list(sType="error", text=text, errMsg="Statement with more than one '<-'"))
  if (length(tmp)==1) { # apparently a valid assignment statement
    parts <- str_trim(strsplit(text, "<-", fixed=TRUE)[[1]])
    if (length(parts)!=2) 
      return(list(sType="error", text=text, errMsg="Malformed assignment statement."))
    # Assure ~ and <- are not in either part
    err <- str_locate(parts,"(<-|~)")[,1]
    if (any(!is.na(err))) 
      return(list(sType="error", text=text, errMsg="Malformed assignment statement."))
    br2 <- nchar(parts[1])
    vars <- NULL
    var <- parts[1]
    # Check if using an assignment function, e.g., logit(p)
    loc <- str_locate(var, "[[:alpha:]]+[(]+")
    if (is.na(loc[1,1])) {
      assignFunc <- NULL
    } else {
      nch <- nchar(var)
      if (substring(var, nchar(var))!=")" || loc[1,2]==nch)
        return(list(sType="error", text=text, errMsg="Malformed assignment statement."))
      assignFunc <- substring(parts[1], 1, loc[1,2]-1)
      if (is.na(match(assignFunc, validAssignFuncs)))
        return(list(sType="error", text=text, errMsg="Invalid assignment function."))
      var <- str_trim(substring(var,loc[1,2]+1, nch-1))
    } # end if function around assignment variable
    if (length(grep(ATOMIC_VAR, var))==0) { # Handle array-type assignment variable
      nch <- nchar(var)
      if (substring(var, nch) != "]") 
        return(list(sType="error", text=text, errMsg="Bad assignment variable.."))
      br1 <- str_locate(var, fixed("["))
      if (is.na(br1[1,1]) || br1[1,1]==1)
        return(list(sType="error", text=text, errMsg="Bad brackets for assignment variable."))
      arrVarOne <- paste(substring(var, 1, br1[1,1]-1), "$", sep="")
      tmp <- parseExpression(var, engine=engine)
      mtch <- match(arrVarOne, substring(tmp,1,nchar(arrVarOne)))
      if (is.na(mtch)) return(list(sType="error", text=text, errMsg="Can't parse assignment variable."))
      var <- tmp[mtch]
      tmp <- tmp[-mtch]
      if (length(tmp)>1 || tmp!="") vars <- c(vars, tmp)
    }

    # Process RHS of assignment
    tmp <- parseExpression(parts[2], engine=engine)
    if (any(is.na(tmp))) 
      return(list(sType="error", text=text, errMsg="Can't parse assignment expression"))
    if (length(tmp)>1 || tmp!="") vars <- c(vars, tmp)
    
    tmp <- checkFunctions(vars)
    if (!is.null(tmp$errMsg)) {
      return(list(sType="error", text=text, errMsg=tmp$errMsg))
    } else {
      vars <- tmp$vars
    }

    return(list(sType="assign", text=text, var=var, assignFunc=assignFunc, vars=unique(vars)))
  } # end if assignment statement

  #################################################################################
  ### Check for "stochastic"; could be, e.g., a~dnorm(x,log(y[2]))I(log(y[1]),) ###
  ### Except: I() is not allowed in jags, but T() is.                           ###
  #################################################################################
  tmp <- grep("~", text)
  if (length(tmp)>1) 
    return(list(sType="error", text=text, errMsg="Only one '~' allowed per statement."))
  if (length(tmp)==1) {
    vars <- NULL
    parts <- str_trim(strsplit(text, "~", fixed=TRUE)[[1]])
    if (length(parts)!=2)
      return(list(sType="error", text=text, errMsg="Malformed stochastic statement."))
    err <- str_locate(parts,"(<-|~)")[,1]
    if (any(!is.na(err))) 
      return(list(sType="error", text=text, errMsg="Malformed stochastic statement."))
    # get array vars of variable assigned to
    node <- parts[1]
    parsedNode <- parseExpression(node, engine=engine)
    if (any(is.na(parsedNode))) 
      return(list(sType="error", text=text, errMsg="Can't parse assignment variable name."))
    br2 <- nchar(node)
    if (substring(node, br2)=="]") {
      br1 <- str_locate(node, fixed("["))
      if (is.na(br1[1,1])) 
        return(list(sType="error", text=text, errMsg="Can't parse assignment variable name."))
      if (br1[1,1]==1) 
        return(list(sType="error", text=text, errMsg="Can't parse assignment variable name."))
      node <- substring(node, 1, br1[1,1]-1)
      tmp <- match(node, substring(parsedNode, 1, nchar(node)))
      if (is.na(tmp)) 
        return(list(sType="error", text=text, errMsg="Can't parse assignment variable name."))
      node <- parsedNode[tmp]
      parsedNode <- parsedNode[-tmp]
      parsedNode <- parsedNode[parsedNode!=""]
      if (length(parsedNode)>0) vars <- c(vars, parsedNode)
    }
    
    # get censoring limit on distribution (winBugs only)
    br1 <- str_locate(parts[2], "\\)[[:blank:]]*I[[:blank:]]*\\(")
    if (nrow(br1)>1) 
      return(list(sType="error", text=text, errMsg="Malformed stochastic censoring limit."))
    if (is.na(br1[1,1])) {
      limit <- NULL
    } else {
      if (engine=="jags") return(list(sType="error", text=text, errMsg="I() censoring not allowed in jags"))
      limitString <- str_trim(substring(parts[2], br1[1,1]+1))
      limitVars <- parseExpression(limitString, engine=engine)
      if (any(is.na(limitVars))) 
        return(list(sType="error", text=text, errMsg="Bad stochastic censoring limit."))
      limitVars <- setdiff(limitVars, "I@@")
      limitString <- str_trim(substring(limitString, 2))
      tmp <- nchar(limitString)
      if (substring(limitString,tmp) != ")")
        return(list(sType="error", text=text, errMsg="Malformed stochastic node."))
      limit <- getArgs(substring(limitString,2,tmp-1))
      if (is.na(limit[[1]])) limit[[1]] = -Inf
      if (is.na(limit[[2]])) limit[[2]] = Inf
      if (length(limit)!=2 || any(is.character(limit)))
        return(list(sType="error", text=text, errMsg="Malformed stochastic node."))
      parts[2] <- str_trim(substring(parts[2], 1, br1[1,1]))
    }
    # get truncation limit on distribution (jags only)
    br1 <- str_locate(parts[2], "\\)[[:blank:]]*T[[:blank:]]*\\(")
    if (nrow(br1)>1) 
      return(list(sType="error", text=text, errMsg="Malformed stochastic truncation limit."))
    if (is.na(br1[1,1])) {
      limit <- NULL
    } else {
      if (engine!="jags") return(list(sType="error", text=text, errMsg="T() truncation not allowed in winBUGS"))
      limitString <- str_trim(substring(parts[2], br1[1,1]+1))
      limitVars <- parseExpression(limitString, engine=engine)
      if (any(is.na(limitVars))) 
        return(list(sType="error", text=text, errMsg="Bad stochastic truncation limit."))
      limitVars <- setdiff(limitVars, "T@@")
      limitString <- str_trim(substring(limitString, 2))
      tmp <- nchar(limitString)
      if (substring(limitString,tmp) != ")")
        return(list(sType="error", text=text, errMsg="Malformed stochastic node."))
      limit <- getArgs(substring(limitString,2,tmp-1))
      if (is.na(limit[[1]])) limit[[1]] = -Inf
      if (is.na(limit[[2]])) limit[[2]] = Inf
      if (length(limit)!=2 || any(is.character(limit)))
        return(list(sType="error", text=text, errMsg="Malformed stochastic node."))
      parts[2] <- str_trim(substring(parts[2], 1, br1[1,1]))
    }
    # get dist and vars; check if dist is on valid list and has correct argument count
    loc <- str_locate(parts[2], fixed("("))
    tmp <- nchar(parts[2])
    if (is.na(loc[1,1]) || substring(parts[2],tmp,tmp)!=")")
      return(list(sType="error", text=text, 
                  errMsg="Distribution must be ~dfun(...) or ~dfun(...)I(...)."))
    if (loc[1,1]<2) 
      return(list(sType="error", text=text, 
                  errMsg="Distribution must be ~dfun(...) or ~dfun(...)I(...)."))
    dist <- substring(parts[2], 1, loc[1,1]-1)
    tmp <- parseExpression(parts[2], engine=engine)
    if (any(is.na(tmp))) return(list(sType="error", text=text, errMsg="Can't parse distribution."))
    if (parts[2]!="dflat()") {
      nch <- nchar(dist)
      loc <- which(substring(tmp,1,nch+1)==paste(dist,"@",sep=""))
      if (length(loc)!=1 && dist!="dflat") return(list(sType="error", text=text, errMsg="Can't parse distribution."))
      dist <- tmp[loc]
      tmp <- tmp[-loc]
      if (length(tmp)>=1 || tmp!="") vars <- unique(c(vars, tmp))  # 2/13/2013: >1 to >=1
    }
    if (is.na(match(dist, validDists))) {
      loc <- str_locate(dist, "@")[1,1]
      tmp <- match(substring(dist,1,loc), substring(validDists,1,loc))
      if (is.na(tmp)) {
        return(list(sType="error", text=text,
                    errMsg=paste(substring(dist,1,loc-1), "is not on the list",
                                 "of valid distributions")))
      } else {
        tmp <- nchar(substring(validDists[tmp], nch+1))
        return(list(sType="error", text=text,
                    errMsg=paste("Distribution ", substring(dist,1,loc-1), " requires ",
                                  tmp, " argument", ifelse(tmp==1,"","s"), sep="" )))
      }
    }
    
    # add defaults to limit
    if (!is.null(limit)) {
      Sel <- match(dist, validDists)
      for (limPos in 1:2) {
        if (is.na(limit[[limPos]])) {
          lim <- distCensDefault[Sel,limPos]
          if (is.na(lim))
            return(list(sType="error", text=text, errMsg="Censoring not valid for this distribution."))
          if (is.nan(lim)) {
            limit[[limPos]] <- ""
          } else {
            limit[[limPos]] <- lim
          }
        }
      }
      limit <- list(string=paste("I(",limit[[1]],",",limit[[2]],")",sep=""),
                    vars=limitVars, low=limit[[1]], high=limit[[2]])
    }

    tmp <- checkFunctions(unique(vars))
    if (!is.null(tmp$errMsg)) {
      return(list(sType="error", text=text, errMsg=tmp$errMsg))
    } else {
      vars <- tmp$vars
    }

    return(list(sType="stochastic", 
                text=paste(parts[[1]],"~",parts[[2]],limit$string,sep=""),
                node=node, dist=dist,
                vars=vars, limit=limit))
  }

  return(list(sType="error", text=text, errMsg="Unidentifiable statement type."))
}  # end extractSyntax()

