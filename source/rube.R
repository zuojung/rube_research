# rube() is a RWinBUGS/R2jags wrapper plus with many auxiliary functions.
# model = name of model file or, if the length of "model" is >1 or it 
#         contains newlines, an in-line model is assumed.
# data = a list of data or a data.frame or a function that makes the data
#        or a string vector of data values to be loaded from the current
#        environment.
# inits = function with argument "data" used to generate starting values
#   (second argument is "initsExtra") or starting values as a list.
# parameters.to.save = parameters for which WinBUGS/R2jags saves MCMC results.  NULL
#   places rube() in a model-check-only mode.
# dataParams = a list of non-default arguments to the data making function.
# initsExtra = second argument to inits().
# parameters.to.save = character vector of parameters to save.
# n.burnin = burn-in iteration count.
# n.iter = post burn-in iteration count.
# n.thin = fraction of iterations to save.
# bugs.seed = a random number unless you set it to NULL.  This reverses
#   the bugs() default, and gives different results for repeat runs.
# wd = working directory for model file and output.
# modelFile = name of file containing model after substitutions and
#   case selection (default is "tempModel.txt:)
# subs = list of named elements and their replacements for
#   the set(FOO[=bar]) syntax.  (See subsText() for details.)
# cases = a string vector of IFCASE()/ENDCASE lines to keep;
#   others are dropped (see selectText() below for details).
#   This is also the third argument to inits().
# If "data" has a list element called "unlist", with
#   elements matching names in "data", those elements
#   in "data" are unlist()ed and renamed to match the list names.
# If "data" has a list element called "drop", those
#   elements are not passed to bugs(), but are added to the bugs()
#   result as "extra" (unrelated to "initsExtra").
# Return value: output of bugs()
#
rube <- function(model, data=NULL, inits=NULL, parameters.to.save=NULL,
                 n.chains=ifelse(is.null(parameters.to.save),1,3), 
                 n.iter=2000, n.burnin=floor(n.iter/2),
                 n.thin = max(1, floor(n.chains * (n.iter - n.burnin)/n.sims)),
                 n.sims=1000, bin = (n.iter - n.burnin)/n.thin, 
                 debug = FALSE, modelCheck=c("always","onFail","never"),
                 cullData = TRUE, cullInits=TRUE, cullPts=TRUE,
                 cullWarn = TRUE,
                 DIC = TRUE, digits = 5, codaPkg = FALSE, 
                 subs=NULL, cases=NULL, varList=list(),
                 bugs.directory = ifelse(Sys.getenv("BUGSDIR")=="", 
                                         "c:/Program Files/WinBUGS14/",
                                         Sys.getenv("BUGSDIR")),
                 useWINE = .Platform$OS.type != "windows", 
                 WINE = NULL, newWINE = TRUE, WINEPATH = NULL,
                 program = c("auto","WinBUGS", "OpenBUGS", "winbugs", "openbugs","jags"), 
                 parallel = FALSE,
                 progress.bar = c("gui","text","none"),
                 RNGname = c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper", "Mersenne-Twister"),
                 wd=ifelse(Sys.getenv("BUGSWD")=="", 
                                         getwd(),
                                         Sys.getenv("BUGSWD")), 
                 bugs.seed=round(runif(1,1,2^31)), over.relax=FALSE,
                 dataParams=list(), initsExtra=NULL, warnUseless=FALSE,
                 modelFile="tempModel.txt", ignore=NULL) {

  if (!mode(model)=="character") stop("model must be a file name or model string")
  if (bin>(n.iter-n.burnin)/n.thin) stop("'bin' is too large and would increase total iterations")
  modelCheck <- match.arg(modelCheck)
  if (modelCheck!="always" && length(parameters.to.save>=1) && parameters.to.save[1]=="*")
    stop("wildcard for 'parameters.to.save' requires 'modelCheck=\"always\"'")
  progress.bar <- match.arg(progress.bar)
  RNGname <- match.arg(RNGname)

  # Use jags if jags is loaded OR program="jags"
  program <- match.arg(program)
  if (program=="auto") {
    if ("package:R2jags" %in% search()) {
      program <- "jags"
    } else {
      program <- "WinBUGS"
    }
  }       
    
  if (program != "jags" && parallel == TRUE) {
    warning("WinBUGS does not honor the 'parallel' request")
    parallel <- FALSE
  }
  if (program == "jags") modelCheck <- "always"  # to drop non-stochastic initializations
  if (program == "jags" && parallel == TRUE) {
    if (!is.function(inits)) {
      warning("turning off 'parallel' because 'inits' is not a function")
      parallel <- FALSE
    } else {
      initsFun = inits # save for running jags.parallel()
    }
  }

  # inits can be a filename, a list, or a string vector of objects.
  # Convert string vectors to lists.
  if (is.character(inits)) {
    if (length(inits)>1) {
      tmp <- lapply(inits ,function(x)try(get(x),silent=TRUE))
      err <- sapply(tmp, is, class="try-error")
      if (any(err))
        stop("cannot find object(s): ", paste(inits[err],collapse=", "))
      names(tmp) <- inits
      inits <- tmp
    } else {
      if (!file.exists(inits)) stop("file ", inits, " not found")
      tmp <- try(dget(inits))
      if (is(tmp, "try-error")) 
        stop(inits, " appears not to have been created with dput()")
      if (!is.list(tmp)) stop("inits in ", inits, "is not a list")
      inits <- tmp
    }
  }

  # Quick first check of data and inits
  if (!is.null(data) && !(is.list(data) || is.function(data) || is.character(data)))
    stop("data must be NULL, a list, a data.frame, a string, or a function")
  if (!is.null(inits) && !(is.list(inits) || is.function(inits)))
    stop("inits must be NULL, a list, or a function")

  # Make model formulation substitutions and write to a temp file
  model <- try(composeModel(model, subs=subs, cases=cases, varList=varList, wd=wd, 
                        allowNoVarList=TRUE,
                        warnUseless=warnUseless, outFile=modelFile,
                        allowStochExpr=TRUE), silent=TRUE)
  if (!is(model,"try-error")) model <- try(cleanCode(model), silent=TRUE)
  if (is(model,"try-error")) {
    tmp <- strsplit(model, ":")[[1]][-1]
    tmp2 <- substring(tmp, nchar(tmp)) == "\n"
    if (any(tmp2)) 
      tmp[tmp2] <- substring(tmp[tmp2], 1, nchar(tmp[tmp2])-1)
    check <- list(problems=paste("Problem with model: ",
                         paste(tmp,collapse=" "), sep=""))
    class(check) <- "bugsCheck"
    rtn <- list(check=check)
    class(rtn) <- "rube"
    warning(gettextf("model checking was unsuccessful"))
    return(rtn)
  }

  # Handle myriad forms of data input
  # Call data-maker
  data <- makeData(data, dataParams)


  ### Initial parameter values (may have "data", "cases", and/or "extra" arguments)
  thisInit <- NULL
  if (!is.null(inits)) {
    if (is.list(inits)) {
      if (all(sapply(inits, class)=="list")) {
        thisInit <- inits
      } else {
        if (n.chains>1) 
          warning("initializing multiple chains with identical values")
        for (i in 1:n.chains) thisInit <- c(thisInit, list(inits))
      }
    } else { # 'inits' is a function
      args <- match(c("data", "extra", "cases"), names(formals(inits)), nomatch=0)
      if (parallel==TRUE && any(args!=0))
        stop("parallel jags does not allow arguments to the 'inits' function")
      # run inits() for each chain to generate list-form inits
      args <- 1 + as.numeric(args[1]!=0) + as.numeric(args[2]!=0)*2 + as.numeric(args[3]!=0)*4
      for (i in 1:ifelse(parallel,1,n.chains)) {
        tmp <- try(switch(args, inits(),
                                inits(data=data),
                                inits(extra=initsExtra),
                                inits(data=data, extra=initsExtra),
                                inits(cases=cases),
                                inits(data=data,cases=cases),
                                inits(extra=initsExtra,cases=cases),
                                inits(data=data, extra=initsExtra, cases=cases)), silent=TRUE)
        if (is(tmp,"try-error")) {
          tmp <- substring(unclass(tmp), 1, nchar(tmp)-1)
          check <- list(problems=paste("Error calling your inits function (",
                                       deparse(substitute(inits)), "): ",
                                       tmp, sep=""))
          class(check) <- "bugsCheck"
          rtn <- list(check=check)
          class(rtn) <- "rube"
          return(rtn)
        }
        if (!is.list(tmp) || is.null(names(tmp)) || any(names(tmp)=="")) 
          stop("inits function must return a fully named list")
        thisInit <- c(thisInit, list(tmp))
      }
    }
  }

  ### Check model if desired (return check info if not parameters.to.save):
  check <- NULL
  if (is.null(parameters.to.save) || modelCheck=="always")
    check <- bugsCheck(model, data=data[names(data)!="drop"], inits=thisInit,
                       cullData=cullData, cullInits=cullInits, 
                       engine=ifelse(program=="jags","jags","WinBUGS"))

  ## By design, leaving out parameters.to.save gives model checking only, 
  ## without a WinBUGS/R2jags run.
  if (is.null(parameters.to.save)) {
    rtn <- list(check=check)
    class(rtn) <- "rube"
    return(rtn)
  }

  ## Check if inits match chains (above code handles inits as a function)
  if (length(thisInit)!=ifelse(parallel,1,n.chains)) {
    check$problems = c(check$problems, 
                       paste("bugs() not run because ", length(thisInit), 
                             " sets are in inits, but n.chains=", n.chains, sep=""))
    rtn <- list(check=check)
    class(rtn) <- "rube"
    return(rtn)
  }

  ## Function to determine whether or not to try bugs() based on
  ## problems found and the value of the 'ignore' parameter.
  quitEarly <- function(problems, ignore) {
    if (is.null(problems)) return(FALSE)
    doNotTry <- TRUE
    if (is.character(ignore)) {
      if (length(ignore)==1 && ignore=="all") {
        doNotTry <- FALSE
      } else {
        tmp <- lapply(ignore, function(x) 
                      grep(paste("^",x,sep=""),problems))
        if (length(unique(unlist(tmp)))==length(problems))
          doNotTry <- FALSE
      }
    }
    return(doNotTry)
  }

  ## Handle culling (if at least some model checking was allowed)
  if (modelCheck!="never") {
    if (is.null(check)) {
      check <- bugsCheck(model, data=data, inits=thisInit, cullData=cullData, 
                         cullInits=cullInits, categorizeOnly=TRUE,
                         engine=ifelse(program=="jags","jags","WinBUGS"))
    }
    if (quitEarly(check$problems, ignore)) {
      warning(gettextf("rube did not attempt to run %s: use summary() to see the problem list", program))
      check$problems <- c(check$problems, paste("rube did not attempt to run ", program, ".", sep=""))
      rtn <- list(check=check)
      class(rtn) <- "rube"
      return(rtn)
    }

    # Cull data
    allVars <- check$plainVars
    dropData <- setdiff(names(data), allVars)
    if (cullData) {
      for (drp in dropData) data[[drp]] <- NULL
      if (cullWarn && length(dropData)>0) 
        warning(wrap(dropData,"dropping","from data",print=FALSE))
      dropData <- character(0)
    }

    # Cull initializations
    dropInits <- NULL
    if (cullInits && !is.null(thisInit)  && !parallel) {
      if (!all(sapply(thisInit,class)=="list")) stop("Programming error 27352")
      dropInits <- setdiff(names(thisInit[[1]]), rownames(check$stochs))
      for (i in 1:length(thisInit)) {
        thisInit[[i]][match(dropInits,names(thisInit[[i]]))] <- NULL
      }
      if (cullWarn && length(dropInits)>0) 
        warning(wrap(dropInits,"dropping","from inits",print=FALSE))
      dropInits <- character(0)
    }

    # Cull parameters.to.save
    if (is.data.frame(check$data)) {
      alldata <- rownames(check$data)[check$data$NAs==0]
    } else {
      alldata <- NULL
    }

    if (length(parameters.to.save)>=1 && parameters.to.save[1]=="*") {
      toDrop <- parameters.to.save[-1]
      parameters.to.save <- rownames(check$stochs)
      if (length(toDrop)>0) 
        parameters.to.save <- setdiff(parameters.to.save, toDrop)
    }
    dropPts <- setdiff(parameters.to.save, setdiff(allVars,alldata))
    if (cullPts) {
      for (drp in dropPts) {
        parameters.to.save <- parameters.to.save[is.na(match(parameters.to.save,dropPts))]
      }
      if (cullWarn && length(dropPts)>0) 
        warning(wrap(dropPts,"dropping","from parameters.to.save",print=FALSE))
      dropPts <- character(0)
      if (length(parameters.to.save)==0) {
        warning(gettextf("rube did not attempt to run %s: no remaining parameters.to.save", program))
        check$problems <- c(check$problems, gettextf("No remaining parameters.to.save."), 
                            gettextf("Rube did not attempt to run %s.", program))
        rtn <- list(check=check)
        class(rtn) <- "rube"
        return(rtn)
      }
    }

    if (length(dropData)>0)
      check$problems <- c(check$problems,
        paste("Unculled data (", paste(dropData,collapse=", "), 
              ") will cause ", program, " to fail.", sep=""))
    if (length(dropInits)>0)
      check$problems <- c(check$problems,
        paste("Unculled inits (", paste(dropInits,collapse=", "), 
              ") will cause ", program, " to fail.", sep=""))
    if (length(dropPts)>0)
      check$problems <- c(check$problems,
        paste("Unculled parameters.to.save (", paste(dropPts,collapse=", "), 
              ") will cause ", program, " to fail.", sep=""))
    if (quitEarly(check$problems, ignore)) {
      rtn <- list(check=check)
      class(rtn) <- "rube"
      return(rtn)
    }
  }


  # Reformat (unlist) data if requested
  if (!is.null(data[["unlist"]])) data <- unlister(data)


  # Remove "drop" data
  tmp <- match("drop", names(data))
  if (is.na(tmp)) {
    extra <- NULL
  } else {
    extra <- data[[tmp]]
    data <- data[-tmp]    
  }

  # Protect against invalid WinBUGS/jags parameter(s)
  if (n.thin<1) {
    warning(gettextf("rube did not attempt to run %s: invalid n.thin", program))
    check$problems <- c(check$problems, gettextf("Invalid n.thin."), 
                       gettextf("Rube did not attempt to run %s.", program))
    rtn <- list(check=check)
    class(rtn) <- "rube"
    return(rtn)
  }

  
  ### Run WinBUGS or jags
  startTime <- Sys.time()
  if (!is.null(thisInit)) {
    if (!all(sapply(thisInit, class)=="list")) thisInit <- list(thisInit)
  }
  tmp <- duplicated(parameters.to.save)
  if (any(tmp)) {
    warning("dropping duplicate parameters.to.save:",paste(parameters.to.save[tmp], collapse=", "))
    parameters.to.save <-parameters.to.save[!tmp]
  }

  # Use jags if jags is loaded OR program="jags"
  if (program=="jags") {
    if (!require(R2jags)) stop("cannot load package 'R2jags'")
    # jags does not allow initialization of non-stochastic nodes.
    # If non-parallel, rube() runs any inits() function and discards these.
    toDrop <- gsub("(\\[|\\])", "", check$assigns)
    toDrop <- intersect(names(thisInit[[1]]), toDrop)
    if (length(toDrop)>0 && parallel==TRUE)
      stop("parallel jags will fail on non-stochastic initialization of:\n",
           paste(toDrop, collapse=", "))
    if (!parallel) {
      if (length(toDrop)>0) {
        for (i in 1:n.chains) {
          for (j in toDrop) thisInit[[i]][[j]] <- NULL
        }
      }
      rtn <- try(jags(data=data, inits=thisInit, 
                      parameters.to.save=parameters.to.save,
                      model.file = modelFile,
                      n.chains = n.chains, n.iter = n.iter,
                      n.burnin = n.burnin, n.thin = n.thin, 
                      DIC = DIC, working.directory = wd, 
                      jags.seed = bugs.seed, refresh=bin, 
                      progress.bar = progress.bar,
                      digits = digits, RNGname = RNGname))
    } else { # parallel
      # parallel requires data to be in the search() path and 'data' to be a string of names
      toSave <- intersect(names(data), objects(envir=.GlobalEnv))
      if (length(toSave)>0) {
        warning("parallel jags require prevention of masking of 'data' variables:\n",
                paste(toSave, collapse=", "))
        saveList <- vector("list",length(toSave))
        names(saveList) <- toSave
        for (i in toSave) saveList[[i]] <- get(i, envir=.GlobalEnv)
        rm(list=toSave, envir=.GlobalEnv)
      }
      dataNames <- names(data)
      attach(data)
      rtn <- try(jags.parallel(data=dataNames, inits=initsFun, 
                               parameters.to.save=parameters.to.save,
                               model.file = modelFile,
                               n.chains = n.chains, n.iter = n.iter,
                               n.burnin = n.burnin, n.thin = n.thin, 
                               DIC = DIC, working.directory = wd, 
                               jags.seed = bugs.seed, digits = digits,
                               RNGname = RNGname))
      detach("data")
      if (length(toSave)>0) {
        for (i in seq(along.with=toSave)) assign(toSave[i], saveList[[i]], envir=.GlobalEnv)
        rm(saveList, toSave)
      }
    }
    ## Need post processing for jags??????
  } else { # using WinBUGS
    rtn <- try(bugs(data=data, inits=thisInit, 
               parameters.to.save=parameters.to.save,
               model.file = modelFile,
               n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin, 
               n.thin = n.thin, n.sims = n.sims, bin = bin, debug = debug, 
               DIC = DIC, digits = digits, codaPkg = codaPkg, 
               bugs.directory = bugs.directory, 
               program = program, working.directory = wd, 
               useWINE = useWINE, WINE = WINE, 
               newWINE = newWINE, WINEPATH = WINEPATH,
               bugs.seed = bugs.seed, summary.only = FALSE, 
               save.history = debug | DIC, over.relax = over.relax), silent=TRUE)
  }

  ### Compute timing information
  runTime <- Sys.time() - startTime

  ### Check if successful
  if (is(rtn, "try-error")) {
    errs <- paste(str_trim(strsplit(unclass(rtn),"\n")[[1]][-1]), collapse=" ")
    if (substring(errs, 1, 20)=="Look at the log file") {
      errs <- NULL
    } else {
      warning(gettextf("%s failed: %s", program, errs))
    }
    if (modelCheck %in% c("onFail","always")) {
      if (modelCheck=="onFail")
        check <- bugsCheck(model, categorizeOnly=TRUE, data=data, inits=thisInit,
                           cullData=cullData, cullInits=cullInits,
                           engine=ifelse(program=="jags","jags","WinBUGS"))
      check$problems <- c(check$problems, paste(program, " run was unsuccessful.", sep=""))
    } else {
      check <- list(problems=c("bugs() failed and modelCheck was set to \"never\"",
                               "Try debug=TRUE and/or modelCheck=\"onFail\""))
      class(check) <- "bugsCheck"
    }
    rtn <- list(check=check, startTime=startTime, runTime=runTime, bugs.seed=bugs.seed)
    class(rtn) <- "rube"
    if (length(errs>0)) rtn$check$problems <- c(rtn$check$problems, errs)

    # Attempt to add log error from log.txt (important: file.info$mtime is truncated)
    if (program != "jags") {
      tstart <- floor(as.numeric(startTime))
      logf <- paste(wd,"/log.txt",sep="")
      if (!file.exists(logf) || tstart>file.info(logf)$mtime) {
        rtn$check$problems <- c(rtn$check$problems,
             "The last visible step in the WinBUGS on-screen Log window is what failed.")
        warning(gettextf("WinBUGS failed (and no WinBUGS log file produced)"))
      } else {
        tmp <- readLines(logf)
        standard <- match(substring(readLines("log.txt"),1,4),
                          c("disp","chec","mode","data","comp","init","gen.",
                            "comm","thin","upda","set(","dic.","coda","stat",
                            "this","dic.","DIC","hist","HIST","save","set.",""))
        errLine <- tmp[is.na(standard)]
        if (length(errLine)<5) {
          rtn$check$problems <- c(rtn$check$problems, errLine)
          warning(gettextf("WinBUGS failed: %s", errLine))
        }
      }
    }
    return(rtn)
  } # endif bugs() failed

  ### Further annotate the successful "bugs" object
  if (program == "jags") rtn = rtn$BUGSoutput
  rtn <- c(rtn, list(model=model, startTime=startTime, runTime=runTime, bugs.seed=bugs.seed))
  if (!is.null(check)) rtn <- c(rtn, list(check=check))
  if (!is.null(extra)) rtn <- c(rtn, list(extra=extra))
  LCPreSuf <- attr(model,"LCPreSuf")
  if (!is.null(LCPreSuf)) rtn <- c(rtn, list(LCPreSuf=LCPreSuf))
  if (!is.null(varList)) rtn <- c(rtn, list(varList=varList))
  rtn <- c(rtn, list(engine = ifelse(program=="jags", "jags", "WinBUGS")))

  # Attempt to add MCMC error from log.txt
  if (program != "jags") {
    logf <- paste(wd,"/log.txt",sep="")
    if (file.exists(logf)) {
      tmp <- readLines(logf)
      start <- grep("Node statistics", tmp)[1]+2
      if (!is.na(start)) {
        end <- grep("\\(",tmp[start:length(tmp)])[1]
        tmp <- tmp[start:(start+end-2)]
        tmp <- tmp[tmp!=""]
        tmp <- strsplit(tmp, "\t")
        tmp <- tmp[sapply(tmp,length)==10]
        tmpV <- sapply(tmp,function(x)as.numeric(x[5]))
        tmpN <- sapply(tmp,function(x) x[2])
        names(tmpV) <- tmpN
        rtn <- c(rtn, list(MCMCerr=tmpV))
      }
    }
  }
  class(rtn) <- c("rube", ifelse(program=="jags", "rjags", "bugs"))
  return(rtn)
}

