####################################################################################
# Augment a rube object by adding new parameters based on formulas using old ones. #
# The 'add' argument is a named list of expressions or string using standard R     #
# expression notation.   Matrix components are not allowed.  For vector parameters #
# in a formula one element will be created for each element in the existing        #
# parameter or [#:#] can be used to create fewer elements.  Of course in formulas  #
# multiple each parameter must be non-vector or have the same number of elements   #
# all other vector parameters.                                                     #
####################################################################################
#
augment <- function(rubeResult, add=NULL) {
  # Check input
  if (is.null(add)) stop("'add' must be specified")
  if (!is.list(add)) stop("'add' must be a named list of expression or strings")
  if (is.null(names(add)) || any(names(add)=="")) stop("'add' must have names")
  if (is(rubeResult,"rube") && !(is(rubeResult,"bugs") || is(rubeResult,"rjags")))
    stop("MCMC failed or was not attempted; try summary() on your rube() output")

  # Get chain info
  objDim <- dim(rubeResult$sims.array)
  n.chains <- objDim[2]
  n.iter <- objDim[1]

  # Verify correct form of 'add' expressions
  add <- lapply(add, function(x) {try(ifelse(is.expression(x),x,parse(text=x)))})
  temp <- sapply(add, is, class2="try-error")
  if (any(temp)) stop("Bad 'add' expression(s): ", names(add)[temp])

  # get existing scalar and vector (but not array) variable names and lengths
  longNames <- dimnames(rubeResult$sims.array)[[3]]
  longNames <- setdiff(longNames, 
                       grep("[[][[:digit:]]+(,[[:digit:]]+)+[]]", longNames, value=TRUE))
  oldVarIndices <- unique(gsub("[[].+[]]", "", longNames))
  oldVarNames <- names(oldVarIndices) <- oldVarIndices # (needed for lapply() names)
  temp1 <- regexpr("[[][[:digit:]]+[]]", longNames)
  temp2 <- nchar(longNames)
  oldVarIndices <- lapply(oldVarIndices,
                          function(x) {
                            Sel <- substring(longNames,1,nchar(x)+1) == paste(x,"[",sep="") 
                            locs <- substring(longNames[Sel], temp1[Sel]+1, temp2[Sel]-1)
                            return(as.numeric(locs))
                          })
  isScalar <- sapply(oldVarIndices, function(x) length(x)==0)

  # Create a list with one element for each 'add' value.  The element is NULL if 
  # a single new parameter is to be created.  If the expression references arrays
  # it is checked for consistency and a matrix is returned with one column for 
  # each vector parameter referenced.  The rows are the indices used.  (This
  # allows complex indexing such as abc[seq(5,20,5)].)
  newInd <- lapply(add,
                   function(x) {
                     strx <- as.character(x)
                     vecNames <- all.names(x)
                     ind <- match(paste(vecNames, "[", sep=""),
                                  paste(oldVarNames[!isScalar], "[", sep=""))
                     vecNames <- vecNames[!is.na(ind)]
                     if (length(vecNames)==0) return(NULL)
                     vecIndices <- lapply(vecNames,
                                          function(y) {
                                            isExplicit <- length(grep(paste(y,"[[]",sep=""), strx)>0)
                                            if (!isExplicit) return(oldVarIndices[[y]])
                                            indLocs <- gregexpr(paste(y,"[[].+?[]]",sep=""), strx)
                                            indSize <- sapply(indLocs,
                                                function(z) {
                                                  indText <- substring(strx, z+nchar(y)+1, 
                                                                       z+attr(z,"match.length")-2)
                                                  allInd <- try(eval(parse(text=indText)))
                                                  if (is(allInd,"try-error"))
                                                    stop("bad indexing in ",strx)
                                                  if (any(is.na(match(allInd, oldVarIndices[[y]]))))
                                                    stop("reference to non-existant index(es) in ", strx)
                                                  return(allInd)
                                                })
                                          })
                     vecLengths <- sapply(vecIndices, length)
                     if (any(vecLengths!=vecLengths[1]))
                       stop("inconsistent vector lengths in ",strx)
                     vecIndices <- do.call(cbind, vecIndices)
                     colnames(vecIndices) <- vecNames
                     return(vecIndices)
                   })

  # Add in empty new parameter columns with full variable names
  nPerAdd <- sapply(newInd, function(x) ifelse(is.null(x),1,nrow(x)))
  addLoc <- 1 # which element are we adding?
  newNames <- rep("", sum(nPerAdd))
  for (i in 1:length(newInd)) {
    if (is.null(newInd[[i]])) {
      newNames[addLoc] <- names(newInd)[i]
      addLoc <- addLoc + 1
    } else {
      n <- nrow(newInd[[i]])
      newNames[addLoc:(addLoc+n-1)] <- paste(names(newInd)[i],"[",1:n,"]",sep="")
      addLoc <- addLoc + n
    }
  }
  newArray <- array(NA, c(objDim[1], objDim[2], objDim[3]+length(newNames)))
  newArray[,,1:objDim[3]] <- rubeResult$sims.array
  dimnames(newArray) <- list(NULL, NULL,
                             c(dimnames(rubeResult$sims.array)[[3]], newNames))

  # Add in new parameters
  addLoc <- 1
  for (i in 1:length(add)) {
    nNew <- ifelse(is.null(newInd[[i]]),1,nrow(newInd[[i]]))
    nam <- all.names(add[[i]])
    subs <- nam[nam %in% oldVarNames]
    txt <- paste(" ", as.character(add[[i]]), " ", sep="")
    for (j in 1:n.chains) {
      txtj <- txt
      for (k in seq(along=subs)) {
        # pull out a specific chunk containing "myVar[myIndices]"
        complexInd <- gregexpr(paste("[^[:alnum:]._]+",subs[k],"[[].+?[]][^[:alnum:]._]+",sep=""), txtj)[[1]]
        if (length(complexInd)==1 && complexInd==-1) {
          if (subs[k] %in% oldVarNames[isScalar]) {
            # convert myVar to newArray[,j,"myVar"] (for the specific "j")
            txtj <- gsub(paste("([^[:alnum:]._\"]+)(",subs[k],")([^[:alnum:]._\"]+)",sep=""),
                         paste("\\1newArray[,",j,",\"\\2\"]\\3",sep=""),
                         txtj)
          } else {
            # convert myVar to newArray[,j,start:end] (for the specific "j")
            vecInd <- gregexpr(paste("[^[:alnum:]._\"]+",subs[k],"[^[:alnum:]._\"]+",sep=""), txtj)[[1]]
            a <- substring(txtj, vecInd[m], vecInd[m]+attr(vecInd,"match.length")[m]-1)
            insideLoc <- regexpr(subs[k], a)
            inside <- range(which(substring(longNames,1,nchar(subs[k])+1) == 
                                  paste(subs[k],"[",sep="")))
            cols <- paste(inside[1], ":", inside[2], sep="")
            b <- paste(substring(a, 1, insideLoc-1),
                       "newArray[,", j, ",", cols, "]",
                       substring(a, insideLoc+attr(insideLoc,"match.length")), sep="")
            txtj <- sub(a, b, txtj, fixed=TRUE)
          }
        } else {
          # for each vec ref, convert myVar[1:4] to newArray[,j,paste('"myVar"[',1:4,']',sep="")]
          for (m in 1:length(complexInd)) {
            a <- substring(txtj, complexInd[m], complexInd[m]+attr(complexInd,"match.length")[m]-1)
            insideLoc <- regexpr("[[].+[]]", a)
            inside <- eval(parse(text=paste(substring(a,insideLoc+1,insideLoc+attr(insideLoc,"match.len")-2))))
            cols <- paste("c(", paste('\"', subs[k],'[', inside, ']\"', sep="", collapse=","), ")", sep="")
            b <- paste(substring(a, 1, insideLoc - nchar(subs[k]) -1),
                       "newArray[,", j, ",", cols, "]",
                       substring(a, insideLoc+attr(insideLoc,"match.length")), sep="")
            txtj <- sub(a, b, txtj, fixed=TRUE)
          }
        } # end if simple vs. complex (vector) variable 
      } # end for each variable in the new expression
      temp <- try(eval(parse(text=txtj)))
      if (is(temp,"try-error")) stop("failed 'add': ", names(add)[i])
      ##
      ## allow new parameters to refer to earlier new parameters (vector or not)
      ## subs <- union(subs, names(add)[i]) && also fix oldVarNames and isScalar
      ##
      if (is.null(newInd[[i]])) { # new scalar
        newArray[,j,objDim[3]+addLoc] <- temp
      } else { # new vector
        newArray[,j,(objDim[3]+addLoc):(objDim[3]+addLoc+nNew-1)] <- temp
      }
    } # end for each chain (j)
    if (is.null(newInd[[i]])) { # new scalar
      addLoc <- addLoc + 1
    } else { # new vector
      addLoc <- addLoc + nNew
   	 }
    ## Note: ?? extend oldVarNames, isScalar, and newInd to allow chaining
  } # end for each 'add' element
  rubeResult$sims.array <- newArray
  rm(newArray)

  # add in the new parameters' statistics
  temp <- makeStats(rubeResult$sims.array[,,(objDim[3]+1):dim(rubeResult$sims.array)[3], drop=FALSE])
  temp$summary <- temp$summary[,intersect(colnames(temp$summary), colnames(rubeResult$summary)), drop=FALSE]
  rubeResult$summary <- rbind(rubeResult$summary, temp$summary)
  rubeResult$mean <- c(rubeResult$mean, temp$mean)
  rubeResult$median <- c(rubeResult$median, temp$median)
  rubeResult$sd <- c(rubeResult$sd, temp$sd)
  if (any(names(rubeResult)=="MCMCerr"))
    rubeResult$MCMCerr <- c(rubeResult$MCMCerr, temp$MCMCerr)
  return(rubeResult)
}

# e.g., augment(rslt, list(sd="exp(log.sd.ri.zero)", ri="bz0+RIzero[2:5]"))
