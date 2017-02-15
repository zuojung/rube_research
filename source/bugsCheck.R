# Input: 
#   model is a text vector of model code with substitutions of
#         all types already done through composeModel() and with
#         a variety of clean-up tasks, including putting multi-line
#         statements into a single statement, separating multi-
#         statement lines into multiple lines, and removing
#         all comments, as performed by cleanCode().
#   data is a list of data values for the model.
#   inits is a list of initial parameter values for the model.
#   cullData and cullInit determine whether to flag (if FALSE)
#         data or inits values not mentioned in the model.
#   categorizeOnly is a flag to determine whether an abbreviated
#         analysis (if TRUE) is performed, only so that rube
#         has enought information to perform culling.
#
# Value: A "bugsCheck" object which is a list with components:
#        problems: a vector of strings describing problems found
#        forVars: a vector of variable used as "for" indices
#        data: a vector of all data variables
#        uninitialized: a vector of all uninitialized stochastics
#        assigns: a vector of all variables defined by assignment
#        inits: a matrix with initialization details.
#        stochs: a matrix of stochastics, possibly with detail
#           information on distribution, initial values, parameters,
#           and prior mean and sd.
# 
# Details: This function classifies all WinBUGS/jags model declaration statements
#        and collates variable types in the model and optionally the
#        data and initializations.  Specific problems that prevent
#        running in WinBUGS/jags are flagged as "problems".  Possible
#        problems are flagged as "concerns".
#        Line numbers in problems and concerns refer to the cleaned-up
#        code of the $model component of the bugsCheck object.
#
# TODO: Detail variation in components of vectors/arrays.
# TODO: Extend list of covered distributions for tallyVars (stochs)
#
bugsCheck <- function(model, data=NULL, inits=NULL, 
                      cullData=FALSE, cullInits=FALSE,
                      categorizeOnly=FALSE, engine=c("WinBUGS","jags"),
                      wd = wd, custom.dist) {

  engine <- match.arg(engine)
  browser()
  # Brief check of valid input
  if (!is.character(model) || length(model)<=1)
    stop("model argument must be a model in a string vector")

  # Handle multiple chain initializations: for now, use chain 1
  if (!is.null(inits) && all(sapply(inits,class)=="list"))
    inits <- inits[[1]]

  # Create return object of class bugsCheck
  bcObject <- list(model=model, plainVars=NULL,
                   forVars=NULL, uninitialized=NULL, 
                   assigns=NULL, stochs=NULL,
                   data=NULL, inits=NULL, constants=NULL,
                   problems=NULL, concerns=NULL)
  class(bcObject) <- "bugsCheck"
  tmp <- attr(model, "missingVarList")
  if (!is.null(tmp)) {
    bcObject$problems <- c(bcObject$problems, 
      paste("Missing from varLists:", paste(tmp, collapse=", ")))
    attr(bcObject$model, "missingVarList") <- NULL
  }
        
  #######################################################
  ## There should be nothing before or after the model ##
  #######################################################
  mstart <- which(model=="model {")
  dbstart <- which(model == "data {")
  if (engine != "jags" && length(dbstart) > 0) {
    bcObject$problems <- c(bcObject$problems, "WinBugs does not support data blocks.")
    return(bcObject)
  }
  if (length(mstart) == 0) {
    bcObject$problems <- c(bcObject$problems, "No model statement found.")
    return(bcObject)
  }
  if (length(mstart) > 1) {
    bcObject$problems <- c(bcObject$problems, "Multiple model statements found.")
    return(bcObject)
  }
  if (length(dbstart) > 1) {
    bcObject$problems <- c(bcObject$problems, "Multiple data statements found.")
    return(bcObject)
  }
  hasDataBlock <- ifelse(length(dbstart) == 1, TRUE, FALSE)
  if (hasDataBlock && dbstart != 1) {
    bcObject$problems <- c(bcObject$problems, "Data statement must be on first line.")
    return(bcObject)
  }
  if (!hasDataBlock && mstart != 1) {
    bcObject$problems <- c(bcObject$problems, "Model statement must be on the first line.")
    return(bcObject)
  }
  #
  braceCount <- delimTally(model, assumeSingles=TRUE)
  bracePos <- as.numeric(colnames(braceCount))
  cumBrace <- cumsum(braceCount[1,]) - cumsum(braceCount[2,])
  cumBrace0 <- which(cumBrace == 0)
  if (length(cumBrace0) < 1 + hasDataBlock) {
    bcObject$problems <- c(bcObject$problems, "Malformed data and/or model blocks.")
    return(bcObject)
  }
  if (bracePos[cumBrace0[1 + hasDataBlock]] < length(model)) {
    bcObject$problems <- c(bcObject$problems, "Code found after the model.")
    return(bcObject)
  }
  endDataBlock <- if(hasDataBlock) bracePos[cumBrace0[1]] else NULL


  ################################################
  ## Extract syntax, i.e., categorize each line ## 
  ################################################
  Sel <- which(!is.na(str_locate(model, "=")[,1]))
  if (length(Sel)>0) {
    bcObject$problems <- c(bcObject$problems, paste("Equal signs not allowed in WinBugs/jags; line(s) ",
                           paste(Sel, collapse=", "), "in $model"))
  }
  syntax <- sapply(model, extractSyntax, engine=engine, 
                   wd = wd, custom.dist = custom.dist)
  ## Matching brace to initial "model {" statement looks like an "endfor".
  if (!syntax[[length(syntax)]]$sType=="endfor") {
    bcObject$problems <- c(bcObject$problems, "Brace mismatch in $model.")
    return(bcObject)
  }
  syntax[[length(syntax)]] <- list(sType="endmodel", text="}")
  if (!is.null(endDataBlock)) 
    syntax[[endDataBlock]] <- list(sType = "enddata", text = "}")
  names(syntax) <- paste("Line",1:length(syntax),sep="")

  
  ##################################################################
  # Check that each line was read by extractSyntax() without error #
  ##################################################################
  Sel <- which(sapply(syntax, function(x)x$sType=="error"))
  if (length(Sel>0)) {
    tmp <- apply(cbind(Sel, sapply(syntax[Sel], function(x) x$errMsg)),
                1, function(x) paste("At line", x[1], "in $model:", x[2]))
    names(tmp) <- NULL
    bcObject$problems <- c(bcObject$problems, tmp)
    return(bcObject)
  }

  ##################################################################
  # Define special array name conversion (a$$ <==> a[,]) functions #
  ##################################################################
  # Add $'s to names of a list() to indicate dimension if non-atomic
  markArrays <- function(lst) {
    if (is.null(lst)) return(lst)
    lstNames <- names(lst)
    nonAtomic <- sapply(lst, function(x){is.array(x) | length(x)>1})
    if (any(nonAtomic)) {
      dims <- sapply(lst, function(x){ifelse(is.array(x),length(dim(x)),1)})
      lstNames[nonAtomic] <- paste(lstNames[nonAtomic],
         substring(paste(rep("$",max(dims)),collapse=""),1,dims[nonAtomic]),sep="")
    }
    return(lstNames)
  }
  # Remove $'s
  fixMarks <- function(vec, mark=c("$","@"), addBrackets=ifelse(mark=="$",TRUE,FALSE)) {
    if (length(vec)==0) return(NULL)
    mark <- match.arg(mark)
    if (is.null(vec)) return(NULL)
    pos <- str_locate(vec, fixed(mark))[,1]
    if (any(!is.na(pos) & pos==0)) stop("Howard screwed up: pos=0")
    toStrip <- !is.na(pos)
    if (any(toStrip)) {
      commas <- nchar(vec[toStrip])-pos[toStrip]
      commas <- substring(paste(rep(",",max(commas)),collapse=""), max(commas)-commas+1)
      vec[toStrip] <- substring(vec[toStrip],1,pos[toStrip]-1)
      if (addBrackets) vec[toStrip] <- paste(vec[toStrip],"[",commas,"]",sep="")
    }
    return(vec)
  }
  
  
  ###############################
  # Categorize model line types #
  ###############################
  mLength <- length(model)
  # get line numbers of "for" statements:
  fors <- which(sapply(syntax, function(x) x$sType=="for"))
  bcObject$forVars <- sapply(syntax[fors], function(x) x$index)
  # get line numbers of end for "}"s:
  endfors <- which(sapply(syntax, function(x) x$sType=="endfor"))
  
  # Assure "for" loop nesting does not repeat variables
  forPerLine <- list(character(0))
  for (i in 2:mLength) {
    tmp1 <- match(i, fors)
    tmp2 <- match(i, endfors)
    if (!is.na(tmp1)) {
      forPerLine <- c(forPerLine, list(c(forPerLine[[i-1]], bcObject$forVars[tmp1])))
    } else if (!is.na(tmp2)) {
      if (length(forPerLine[[i-1]])==0) {
        # Should not happen (interpreted as "after the model"):
        bcObject$problems <- c(bcObject$problems, 
                               paste("On line", i, "in $model: Improper 'for' nesting"))
      } else {
        forPerLine <- c(forPerLine, list(c(forPerLine[[i-1]][-length(forPerLine[[i-1]])])))
      }
    } else {
      forPerLine <- c(forPerLine, list(forPerLine[[i-1]]))
    }
  }
  dup <- sapply(forPerLine,function(x)any(duplicated(x)))
  if (any(dup)) {
    dup <- which(dup)[1]
    bcObject$problems <- c(bcObject$problems,
                           paste("On line", dup, "in $model: duplicate nested 'for' index"))
  }

  # Assure stochastics and assigns only use "for" indices that are active 
  bcObject$forVars <- sort(unique(unlist(sapply(forPerLine,function(x)x))))
  for (i in 2:(length(syntax)-1)) {
    sType <- syntax[[i]]$sType
    if (sType=="for") {
      tmp <- intersect(bcObject$forVars,c(syntax[[i]]$startItems, syntax[[i]]$endItems))
      if (length(tmp)>0) {
        tmp <- setdiff(tmp, forPerLine[[i]])
        if (length(tmp)>0) {
          bcObject$problems <- c(bcObject$problems, paste("On line", i,
                       "in $model, there is use of inactive 'for' index variable(s):",
                       paste(tmp, collapse=", ")))
        }
      }
    } else if (sType=="assign") {
      tmp <- intersect(bcObject$forVars,syntax[[i]]$vars)
      if (length(tmp)>0) {
        tmp <- setdiff(tmp, forPerLine[[i]])
        if (length(tmp)>0) {
          bcObject$problems <- c(bcObject$problems, paste("On line", i,
                       "in $model, there is use of inactive 'for' index variable(s):",
                       paste(tmp, collapse=", ")))
        }
      }
    } else if (sType=="stochastic") {
      tmp <- intersect(bcObject$forVars,c(syntax[[i]]$vars,syntax[[i]]$limit$vars))
      if (length(tmp)>0) {
        tmp <- setdiff(tmp, forPerLine[[i]])
        if (length(tmp)>0) {
          bcObject$problems <- c(bcObject$problems, paste("On line", i,
                       "in $model, there is use of inactive 'for' index variable(s):",
                       paste(tmp, collapse=", ")))
        }
      }
    }
  }
  
  # get variables used as start and end values of for loops
  startEndList <- unique(unlist(sapply(syntax[fors], function(x) x$startItems)))
  startEndList <- unique(c(startEndList, unlist(sapply(syntax[fors], function(x) x$endItems))))
  # store all right-hand-side variables
  RHSvars <- startEndList


  ############################
  # Get stochastic variables #
  ############################
  # get line numbers of "stochastic" statements
  stochs <- which(sapply(syntax, function(x) x$sType=="stochastic"))
  stochVars <- unique(as.character(sapply(syntax[stochs], function(x) x$node)))
  err <- intersect(bcObject$forVars, stochVars)
  if (length(err)>0) {
    bcObject$problems <- c(bcObject$problems,
                           paste("Variable(s) used as both stochastic node(s) and for variable(s): ",
                           paste(fixMarks(err), sep=", "), ".", sep=""))
  }
  RHSvars <- unique(c(RHSvars, unlist(sapply(syntax[stochs], function(x) x$vars))))
  RHSvars <- unique(c(RHSvars, unlist(sapply(syntax[stochs], function(x) {
      if (is.null(x$limit)) return(NULL); return(x$limit$vars) }))))
      

  ###########################################
  # Get assigned (non-stochastic) variables #
  ###########################################
  # get line numbers of "assign" statements:
  assigns <- which(sapply(syntax, function(x) x$sType=="assign"))
  assignVars <- as.character(sapply(syntax[assigns], function(x) x$var))
  conc <- unique(assignVars[duplicated(assignVars)])
  #for (var in conc) {
  #  bcObject$concerns <- c(bcObject$concerns,
  #                         paste(fixMarks(var,"$"),
  #                               " is assigned on multiple lines in $model: ",
  #                         paste(assigns[assignVars%in%var], collapse=", "), ".", sep=""))
  #}
  RHSvars <- unique(c(RHSvars, unlist(sapply(syntax[assigns], function(x) x$vars))))
  RHSvars <- RHSvars[!RHSvars==""]
  assignVars <- unique(assignVars)
  err <- intersect(bcObject$forVars, assignVars)
  if (length(err)>0) {
    bcObject$concerns <- c(bcObject$concerns,
      paste("Variable(s) used as both assigned variable(s) and for variable(s): ",
            paste(fixMarks(err), sep=", "), ".", sep=""))
  }
  #err <- intersect(stochVars, assignVars)
  #if (length(err)>0) {
  #  bcObject$concerns <- c(bcObject$concerns,
  #    paste("Variable(s) used as both assigned variable(s) and stochastic variable(s): ",
  #          paste(fixMarks(err), sep=", "), ".", sep=""))
  #}

  ######################
  # Get data and inits #
  ######################
  dataNames <- markArrays(data)
  initNames <- markArrays(inits)
  
  # Check for duplicated between data and inits
  err <- intersect(dataNames,initNames)
  if (length(err)>0) {
    arrays <- grep("\\$",err)
    tmp <- err[setdiff(seq(along=err),arrays)]
    # Handle atomic duplicates
    if (length(tmp)>0) {
      bcObject$problems <- c(bcObject$problems,
                             paste("In data and inits: ",
                             paste(tmp, collapse=", "), sep=""))
    }
    # Handle array duplicates
    tmp <- err[arrays]
    if (length(tmp)>0) {
      errDim <- errPat <- NULL
      for (var in fixMarks(tmp, mark="$", addBrackets=FALSE)) {
        d = data[[var]]
        i = inits[[var]]
        if (length(dim(d))!=length(dim(i)) || any(dim(d)!=dim(i))) {
          errDim = c(errDim, var)
        } else {
          nBoth <- sum(is.na(d)==is.na(i))
          if (nBoth>0) errPat <- c(errPat, var)
        }
      } # end for each array in both data and inits
      if (length(errDim)>0) {
        bcObject$problems <- c(bcObject$problems,
                               paste("In data and inits with inconsistent dimensions: ",
                               paste(errDim, collapse=", "), sep=""))
      }
      if (length(errPat)>0) {
        bcObject$problems <- c(bcObject$problems,
                               paste("In data and inits with inconsistent NA patterns: ",
                               paste(errPat, collapse=", "), sep=""))
      }
    } # end if arrays are in both data and inits
  } # end if any commonalities between data and inits

  #####################################
  # Check for data/inits not in model #
  #####################################
  err <- setdiff(dataNames, c(stochVars, assignVars, RHSvars))
  if (!cullData && length(err)>0) {
    bcObject$problems <- c(bcObject$problems,
                 paste("In data but not in model: ",
                   paste(fixMarks(err), collapse=", "), sep=""))
  }
  err <- setdiff(initNames, c(stochVars, assignVars, RHSvars))
  if (!cullInits && length(err)>0) {
    bcObject$problems <- c(bcObject$problems,
                 paste("Initialized but not in model: ",
                   paste(fixMarks(err), collapse=", "), sep=""))
  }

  ###########################################
  # Check for inconsistent array dimensions #
  ###########################################
  tmp <- fixMarks(unique(c(dataNames, initNames, stochVars, assignVars, RHSvars)), 
                  addBrackets=FALSE)
  err <- tmp[duplicated(tmp)]
  if (length(err)>0) {
    bcObject$problems <- c(bcObject$problems,
                 paste("Variable(s) with inconsistent dimensions: ",
                 paste(unique(err), collapse=", "), sep=""))
  }

  ##############################################
  # Tally starting values for stochastic nodes #
  ##############################################
  dists <- fixMarks(sapply(syntax[stochs], function(x)x$dist), "@")
  distLimits <-  sapply(syntax[stochs], function(x) x$limit) # truncation limits
  if (!is.null(dists)) {
    err <- dists%in%c("dunif","dbern","ddirch","dmnorm","dmulti","dwish",
                      "dflat","car.normal","car.l1","car.proper","spatial.exe",
                      "spatial.disc","spatial.pred","spatial.unipred",
                      "pois.conv", "mv.car") &
           sapply(distLimits,function(x)!is.null(x))
    if (any(err)) 
      bcObject$problems <- c(bcObject$problems, "I(...) limits not allowed on ",
                             paste(paste(unique(dists[err]),"()",sep=""),collapse=", "),
                             ".", sep="")
  }

  nodes <- fixMarks(sapply(syntax[stochs], function(x)x$node), addBrackets=FALSE)
  nodeNames <- fixMarks(sapply(syntax[stochs],function(x)x$node),"$",FALSE)
  distText <- sapply(syntax[stochs], function(x) {
      lim <- x$limit$string
      x <- str_trim(strsplit(x$text,"~")[[1]][2])
      if (!is.null(lim)) x<- str_trim(substring(x,1,nchar(x)-nchar(lim))) 
      return(substring(x, str_locate(x,fixed("("))[1,1]+1,nchar(x)-1))
  })

  distParams <- lapply(distText, getArgs)

  tallyVars <- fixMarks(nodes)
  stochTally <- matrix("", length(tallyVars), 5)
  colnames(stochTally) <- c("Distr", "Size", "Parameters -> (mean,sd)",
                            "InitialValues [Range]", "Flags")
  rownames(stochTally) <- nodeNames

  # Fill in available info for each stochastic node
  for (i in seq(along=tallyVars)) {
    # Calculate distribution limits "I(,)" using "data" if needed
    if (!is.null(distLimits[[i]])) {
      famLimString <- distLimits[[i]]$string
      #pos <- str_locate(famLimString, "^I\\\\([^,]*[,]")[1,]
      #famLims <- getArgs(substring(famLimString,pos[1]+2, nchar(famLimString)-1))
      famLims <- c(distLimits[[i]]$low, distLimits[[i]]$high)
      # next two problem detections should be redundant
      #if (!all(sapply(famLims,is.numeric))) {
      #  bcObject$problems <- c(bcObject$problems, 
      #    paste(gettextf("Bad censor limits for %s: must be non-symbolic", syntax[stochs][i])))
      #  famLims <- c(NA,NA)
      #}
      #if (length(famLims)!=2) {
      #  bcObject$problems <- c(bcObject$problems, 
      #    paste(gettextf("Bad censor limits for %s: must be 'I(value, value)'", syntax[stochs][i])))
      #  famLims <- c(NA,NA)
      #}
      #famLims <- unlist(famLims)
      stochTally[i,1] <- paste(dists[i],famLimString, sep="_")
    } else {
      famLims <- c(NA, NA)
      stochTally[i,1] <- dists[i]
    }

    # Fill in statistics of what's in data/inits, if any
    DIMin <- DIMax <- DIMean <- DISD <- NA
    val <- inits[[tallyVars[i]]]
    if (is.null(val)) val <- data[[tallyVars[i]]]
    if (!is.null(val)) {
      NOK <- sum(is.finite(val))
      DIMin <- ifelse(NOK>=1, min(val,na.rm=TRUE), NA)
      DIMax <- ifelse(NOK>=1, max(val,na.rm=TRUE), NA)
      DIMean <- ifelse(NOK>=1, mean(val,na.rm=TRUE), NA)
      # as.numeric is needed because sd(n by k matrix) has length k:
      DISD <- ifelse(NOK>=2, sd(as.numeric(val),na.rm=TRUE), NA)
      if (NOK<=1) {
        stochTally[i,2] <- "1"
        stochTally[i,4] <- paste(round(DIMean,3))
      } else {
        if (is.null(dim(val))) {
          stochTally[i,2] <- paste(length(val))
        } else {
          stochTally[i,2] <- paste(dim(val), collapse="x")
        }
        # as.numeric is needed because sd(n by k matrix) has length k
        stochTally[i,4] <- paste(round(DIMean,3), " +/- ", 
                                 round(DISD,3), 
                                 " [", round(DIMin,3), ", ",
                                 round(DIMax,3), "]", sep="")
      }
    }

    # Fill in characterisics of distribution and flag problems
    cround <- function(x,digits=3) ifelse(is.character(x), NA, round(x,digits=digits))
    if (!is.null(distParams[[i]])) {
      famMean <- famSD <- famMin <- famMax <- NA
      stochTally[i,3] <- paste(distParams[[i]], collapse=", ")
      if (dists[i]=="dnorm") {
        famMin <- ifelse(is.na(famLims[[1]]), -Inf, famLims[[1]])
        famMax <- ifelse(is.na(famLims[[2]]), Inf, famLims[[2]])
        famMean <- distParams[[i]][[1]]
        famSD <- try(round(sqrt(1/distParams[[i]][[2]]),3), silent=TRUE)
        if (is(famSD,"try-error")) famSD <- NA
        if (is.na(famSD) && is.character(distParams[[i]][[2]]))
          famSD <- paste("1/sqrt(",distParams[[i]][[2]],")",sep="")
        # If censored (CODE OTHER DISTS TOO)
        if (all(sapply(famLims,is.numeric)) && is.finite(famMean) && is.finite(famSD)) {
          tmp <- rnorm(10000, famMean, famSD)
          lower <- ifelse(is.infinite(famMin), min(tmp), famMin)
          upper <- ifelse(is.infinite(famMax), max(tmp), famMax)
          tmp <- tmp[tmp>=lower & tmp<=upper]
          if (length(tmp)>100) {
            famMean <- round(mean(tmp),3)
            famSD <- round(sd(tmp),3)
          } else {
            famMean <- famSD <- NA
          }
        }
        stochTally[i,3] <- paste(stochTally[i,3], " (", famMean, ", ",
                                 famSD, ")", sep="")
      } else if (dists[i]=="dunif") {
        famMean <- try(0.5*(distParams[[i]][[1]]+distParams[[i]][[2]]), silent=TRUE)
        if (is(famMean,"try-error")) famMean <- NA
        famSD <- try(sqrt((distParams[[i]][[2]]-distParams[[i]][[1]])^2/12), silent=TRUE)
        if (is(famSD,"try-error")) famSD <- NA
        famMin <- distParams[[i]][[1]]
        famMax <- distParams[[i]][[2]]
        stochTally[i,3] <- paste(stochTally[i,3], " (", famMean, ", ",
                                 cround(famSD), ")", sep="")
        if (!is.null(val)) {
          OK <- !is.na(val)
          if (any(val[OK]<=distParams[[i]][[1]] | val[OK]>=distParams[[i]][[2]]))
            bcObject$problems <- c(bcObject$problems, paste("Bad initial", nodes[i]))
         }
      } else if (dists[i]=="dgamma") {
        famMean <- try(distParams[[i]][[1]]/distParams[[i]][[2]], silent=TRUE)
        if (is(famMean,"try-error")) famMean <- NA
        famSD <- try(sqrt(distParams[[i]][[1]]/distParams[[i]][[2]]^2), silent=TRUE)
        if (is(famSD,"try-error")) famSD <- NA
        famMin <- ifelse(is.na(famLims[[1]]), 0, famLims[[1]])
        famMax <- ifelse(is.na(famLims[[2]]), Inf, famLims[[2]])
        stochTally[i,3] <- paste(stochTally[i,3], " (", cround(famMean), ", ",
                                 cround(famSD), ")", sep="")
      } else if (dists[i]=="dbeta") {
        tmp <- try(distParams[[i]][[1]] + distParams[[i]][[2]], silent=TRUE)
        if (is(try,"try-error")) {
          famMean <- NA
        } else {
          famMean <- try(distParams[[i]][[1]]/tmp,silent=TRUE)
          if (is(famMean, "try-error")) famMean <- NA
        }
        famSD <- try(sqrt((distParams[[i]][[1]]*distParams[[i]][[2]])/tmp^2/(1+tmp)), silent=TRUE)
        if (is(famSD,"try-error")) famSD <- NA
        famMin <- ifelse(is.na(famLims[[1]]), 0, max(0,famLims[[1]]))
        famMax <- ifelse(is.na(famLims[[2]]), 1, min(1,famLims[[2]]))
        stochTally[i,3] <- paste(stochTally[i,3], " (", cround(famMean), ", ",
                                 cround(famSD),
                                 ")", sep="")
      } else if (dists[i]=="dexp") {
        famMean <- try(1/distParams[[i]][[1]], silent=TRUE)
        if (is(famMean,"try-error")) famMean <- NA
        famSD <- try(sqrt(1/distParams[[i]][[1]]), silent=TRUE)
        if (is(famSD,"try-error")) famSD <- NA
        famMin <- ifelse(is.na(famLims[[1]]), 0, max(0,famLims[[1]]))
        famMax <- ifelse(is.na(famLims[[2]]), Inf, famLims[[2]])
        stochTally[i,3] <- paste(stochTally[i,3], " (", cround(famMean), ", ", 
                                 cround(famSD), ")", sep="")
      } else if (dists[i]=="dbern") {
        famMean <- distParams[[i]][[1]]
        famSD <- try(sqrt(distParams[[i]][[1]]*(1-distParams[[i]][[1]])), silent=TRUE)
        if (is(famSD,"try-error")) famSD <- NA
        famMin <- 0
        famMax <- 1
        stochTally[i,3] <- paste(stochTally[i,3], " (", cround(famMean), ", ", 
                                 cround(famSD), ")", sep="")
      } else if (dists[i]=="dbin") {
        famMean <- try(distParams[[i]][[1]] * distParams[[i]][[2]], silent=TRUE)
        if (is(famMean,"try-error")) famMean <- NA
        famSD <- try(sqrt(famMean*(1-distParams[[i]][[2]])), silent=TRUE)
        if (is(famSD,"try-error")) famSD <- NA
        famMin <- ifelse(is.na(famLims[[1]]), 0, max(0,famLims[[1]]))
        famMax <- try(ifelse(is.na(famLims[[2]]), distParams[[i]][[2]], 
                         min(distParams[[i]][[2]], famLims[[2]])), silent=TRUE)
        if (is(famMax, "try-error")) famMax <- NA
        stochTally[i,3] <- paste(stochTally[i,3], " (", cround(famMean), ", ", 
                                 cround(famSD), ")", sep="")
      } else if (dists[i]=="dlnorm") {
        famMean <- try(exp(distParams[[i]][[1]]+1/(2*distParams[[i]][[2]])), silent=TRUE)
        if (is(famMean,"try-error")) famMean <- NA
        famSD <- try(sqrt(exp(1/distParams[[i]][[2]]-1)*
                          exp(2*distParams[[i]][[1]]+1/distParams[[i]][[2]])), silent=TRUE)
        if (is(famSD,"try-error")) famSD <- NA
        famMin <- ifelse(is.na(famLims[[1]]), 0, max(0,famLims[[1]]))
        famMax <- ifelse(is.na(famLims[[2]]), Inf, famLims[[2]])
        stochTally[i,3] <- paste(stochTally[i,3], " (", cround(famMean), ", ", 
                                 cround(famSD), ")", sep="")
      } else if (dists[i]=="dflat") {
        famMean <- NA
        famSD <- NA
        famMin <- ifelse(is.na(famLims[[1]]), 0, max(0,famLims[[1]]))
        famMax <- ifelse(is.na(famLims[[2]]), Inf, famLims[[2]])
        stochTally[i,3] <- paste(stochTally[i,3], " (", cround(famMean), ", ", 
                                 cround(famSD), ")", sep="")
      } # end choosing among progammed distributions

      # Look for improbable/impossible data or inits
      if (is.character(DIMin)||is.character(DIMax)){cat("DIMin/DImax is char\n");browser()}
      if (dists[i]=="dcat" && !is.na(DIMin) && !is.na(DIMax)) {
        stochTally[i,5]=""
        tmpV <- fixMarks(syntax[stochs][[i]]$node,addBrackets=FALSE)
        tmp <- data[[tmpV]]
        if (is.null(tmp)) tmp <- inits[[tmpV]]
        if (!is.null(tmp)) {
          tmp <- tmp[!is.na(tmp)]
          if (any(tmp!=floor(tmp)) || any(tmp<1)) {
          stochTally[i,5]="!!!"
          bcObject$problems = c(bcObject$problems, 
                 paste("\"", rownames(stochTally)[i], "\"",
                       " has values outside its distribution's range.", sep=""))
          }
        } 
      } else if (is.na(DIMin) || is.na(DIMax) || is.na(famMin) || is.na(famMax) ||
          is.character(DIMin) || is.character(DIMax) || is.character(famMin) || is.character(famMax)) {
        stochTally[i,5]=""
      } else {
        if (DIMin<famMin || DIMax>famMax) {
          stochTally[i,5]="!!!"
          bcObject$problems = c(bcObject$problems, 
                 paste("\"", rownames(stochTally)[i], "\"",
                       " has values outside its distribution's range.", sep=""))
        } else if(is.numeric(famMean) && is.numeric(famSD)) {
          if (DIMin < famMean-3*famSD || DIMax > famMean+3*famSD) {
            stochTally[i,5]="**"
            bcObject$concerns = c(bcObject$concerns, 
                                  paste("\"", rownames(stochTally)[i], "\"", 
                                        " has values beyond 3 s.d.", sep=""))
          } else if (DIMin < famMean-2*famSD || DIMax > famMean+2*famSD) {
            stochTally[i,5]="*"
          }
        }
      }
    } # end if the distribution has parameters 
  } # end filling in to stochTally for each stochastic node

  # Convert to factors to remove quote marks and make it more readable
  err <- duplicated(rownames(stochTally))
  if (any(err)) {
    #bcObject$concerns <- c(bcObject$concerns,
    #  paste("Duplicate distribution specification for", 
    #        paste(rownames(stochTally)[err],collapse=", ")))
    stochTally <- stochTally[!err,]
  }
  if (nrow(stochTally)<=1) {
    stochTally <- as.data.frame(stochTally)
  } else {
    stochTally <- data.frame(apply(stochTally,2,factor))
    names(stochTally) <- c("Distr","Size","Parameters (mean,sd)",
                           "Initial Value(s) [Range]", "Flags")
  }
  if (!is.null(stochTally) && any(stochTally$Size=="0")) 
    bcObject$problems <- c(bcObject$problems, paste("Stochastic(s) of length zero:",
                             paste(rownames(stochTally)[stochTally$Size=="0"],collapse=", ")))
    
  #########################
  # Initialization detail #
  #########################
  initDetails <- NULL
  if (!categorizeOnly && !is.null(inits)) {
    # Use only the first of multiple initializations (FOR NOW)
    if (all(sapply(inits,class)=="list"))
      inits <- inits[[1]]
    initDetails <- t(sapply(inits, function(x) {
                        size <- ifelse(is.null(dim(x)),
                                       paste(length(x)),
                                       paste(dim(x),collapse="x"))
                        x <- as.numeric(x)
                        NAs <- sum(!is.finite(x))
                        NOK <- length(x)-NAs
                        mn <- ifelse(NOK<2, "", paste(round(min(x,na.rm=TRUE),2)))
                        mx <- ifelse(NOK<2, "", paste(round(max(x,na.rm=TRUE),2)))
                        men <- ifelse(NOK==0, "", paste(round(mean(x,na.rm=TRUE),2)))
                        SD <- ifelse(NOK<2, "", paste(round(sd(x,na.rm=TRUE),2)))
                        return(c(size, mn, mx, men, SD, NAs))
                        }))
    colnames(initDetails) <- c("Size","Min","Max","Mean","SD","NAs")
    # Convert to factors to remove quote marks and make it more readable
    err <- duplicated(rownames(initDetails))
    if (any(err)) {
      bcObject$problems <- c(bcObject$problems,
        paste("Duplicate init values:", paste(rownames(initDetails)[err],collapse=", ")))
      initDetails <- initDetails[!err,]
    }
    if (nrow(initDetails)==1) {
      initDetails <- as.data.frame(initDetails)
    } else {
      initDetails <- data.frame(apply(initDetails,2,factor))
      names(initDetails) <- c("Size","Min","Max","Mean","SD","NAs")
    }
  }

  ###############
  # Data detail #
  ###############
  dataDetails <- NULL
  if (!categorizeOnly && !is.null(data)) {
    dataDetails <- t(sapply(data, function(x) {
                        size <- ifelse(is.null(dim(x)),
                                       paste(length(x)),
                                       paste(dim(x),collapse="x"))
                        x <- as.numeric(x)
                        NAs <- sum(!is.finite(x))
                        NOK <- length(x)-NAs
                        mn <- ifelse(NOK<2, "", paste(round(min(x,na.rm=TRUE),2)))
                        mx <- ifelse(NOK<2, "", paste(round(max(x,na.rm=TRUE),2)))
                        men <- ifelse(NOK==0, "", paste(round(mean(x,na.rm=TRUE),2)))
                        SD <- ifelse(NOK<2, "", paste(round(sd(x,na.rm=TRUE),2)))
                        return(c(size, mn, mx, men, SD, NAs))
                        }))
    colnames(dataDetails) <- c("size","min","max","mean","sd","NAs")
    # Convert to factors to remove quote marks and make it more readable
    err <- duplicated(rownames(dataDetails))
    if (any(err)) {
      bcObject$problems <- c(bcObject$problems,
        paste("Duplicate data values:", paste(rownames(dataDetails)[err],collapse=", ")))
      dataDetails <- dataDetails[!err,]
    }
    if (nrow(dataDetails)==1) {
      dataDetails <- as.data.frame(dataDetails)
    } else {
      dataDetails <- data.frame(apply(dataDetails,2,factor))
      names(dataDetails) <- c("Size","Min","Max","Mean","SD","NAs")
    }
  }
  if (!is.null(dataDetails) && any(dataDetails$Size=="0")) 
    bcObject$problems <- c(bcObject$problems, paste("Data of length zero:",
                             paste(rownames(dataDetails)[dataDetails$Size=="0"],collapse=", ")))

  
  #################
  # Return values #
  #################
  stripBrackets <- function(vars) {
    pos <- str_locate(vars, "\\[[.]*\\]")
    hasBrack <- !is.na(pos[,1])
    for (p in seq(along=vars)[hasBrack])
      vars[p] <- substring(vars[p], 1, pos[i,1]-1)
    vars
  }

  uninit <- union(setdiff(setdiff(stochVars,dataNames), initNames),
                 setdiff(RHSvars,c(dataNames,initNames,stochVars,assignVars,bcObject$forVars)))
  if (length(uninit)==0) uninit <- NULL

  constantNames <- setdiff(dataNames, stochVars)
  if (!is.null(constantNames)) {
    if (is.null(dataDetails)) {
      bcObject$constants <- fixMarks(constantNames)
      dataNames <- setdiff(dataNames, constantNames)
    } else {
      tmp <- rownames(dataDetails)%in%fixMarks(constantNames,addBrackets=FALSE)
      bcObject$constants <- dataDetails[tmp,]
      dataNAs <- dataDetails[!tmp,"NAs",drop=FALSE]
      dataDetails <- stochTally[rownames(stochTally)%in%rownames(dataNAs),]
      tmp <- dataNAs[match(rownames(dataNAs),rownames(dataDetails)),]
      dataDetails <- cbind(dataDetails[,1:2], NAs=tmp, dataDetails[,3:ncol(dataDetails)])
      partialData <- intersect(rownames(dataDetails), rownames(initDetails))
      fullySuppliedAsData <- setdiff(setdiff(rownames(dataNAs), fixMarks(assignVars)), partialData)
      if (length(fullySuppliedAsData)>0)
        stochTally <- stochTally[!rownames(stochTally)%in%fullySuppliedAsData,]
      stochTally <- cbind(stochTally[,1:2], NAs=I(rep("",nrow(stochTally))), stochTally[,3:ncol(stochTally)])
      if (!is.null(initDetails)) {
        tmp <- which(rownames(initDetails)%in%rownames(stochTally))
        stochTally[match(rownames(initDetails)[tmp],rownames(stochTally)), "NAs"] <- 
          as.character(initDetails[tmp,"NAs"])
      }
    }
  }

  if (is.null(dataDetails)) {
    bcObject$data <- fixMarks(dataNames)
  } else {
    bcObject$data <- dataDetails
  }
  bcObject$uninitialized <- fixMarks(uninit)
  bcObject$stochs <- stochTally
  tmp <- ifelse(is.null(dim(dataDetails)), 0, nrow(dataDetails))
  if (is.null(dim(stochs)) && nrow(bcObject$stochs)+tmp<2) 
    bcObject$problems=c(bcObject$problems, "Every Bayesian model has at least two stochastic nodes.")
  bcObject$assigns <- fixMarks(assignVars)

  # If jags instead of winBugs, data block may be needed
  if (engine == "jags" && !hasDataBlock) {
    tmp <- gsub("[[\\]]", "", bcObject$assigns)
    tmp2 <- c(rownames(bcObject$stochs), rownames(bcObject$data))
    tmp3 <- intersect(tmp, tmp2)
    if (length(tmp3)>0) {
      bcObject$problems <- c(bcObject$problems, 
        paste("jags requires you to move assignment(s) to a data block for",
              paste(tmp3, collapse=", ")))
    }
  }

  bcObject$plainVars <- unique(fixMarks(stripBrackets(
                          c(assignVars, stochVars, RHSvars)),"$",FALSE))
  if (is.null(initDetails)) {
    bcObject$inits <- fixMarks(initNames)
  } else {
    bcObject$inits <- initDetails
  }
  return(bcObject)
}

