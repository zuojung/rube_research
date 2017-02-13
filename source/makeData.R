# Rube needs to feed winBUGS a names list as the 'data'.
# Rube flexibly allows any of the following ways of specifying the
#   data, and these are processed in this order:
#   1) A character vector of length 1.  This is first checked to see
#      if it is the name of a file created by dput(), in which case
#      dget() is run.  The result then goes through the remaining
#      processing (i.e., it can be, e.g., a data.frame)
#   2) A character vector.  This is checked to see if it consists of
#      variable names, in which case they are read and placed in a
#      list.  The result goes through the remaining processing.
#   3) A function that generates a data.frame or a list.  The
#      function may have no or any number of arguments.  The 
#      arguments are specified in 'data.Params', which may be NULL.
#      ('dataParams' comes from rube()'s 'dataParams' argument.)
#      Note that all non-default arguments must be supplied in
#      'dataParams'.  The result goes through the remaining
#      processing.
#   4) A data.frame.  Each column becomes a variable in a list,
#      and the variable "N" is created as the number of rows
#      in the data.frame (unless there is already a column named "N".
#   5) A list.  Elements of the list must be numeric (vectors,
#      matrices, or arrays) or data.frames.  Elements other than
#      data.frames must be named.  (If data.frame elements are
#      named, the name is ignored.)  Data.frame colums are converted
#      to their columns.  If there is only one data.frame and if
#      no elements are named "N", the variable "N" is created as the
#      number of rows in the data.frame 
# 
#  OUTPUT: a list of data elements if a valid list can be produced,
#          otherwise an error message is given.
makeData <- function(data, dataParams) {
  if (is.null(data)) return(NULL)

  # Handle possible dput() filename
  missingFile <- notDput <- FALSE
  if (is.character(data) && length(data)==1) {
    if (!file.exists(data)) {
      missingFile <- TRUE
    } else {
      tmp <- try(dget(data), silent=TRUE)
      if (is(tmp, "try-error")) {
        notDput <- TRUE
      } else {
        if (!is.list(tmp)) stop("data in ", data, "is not a list")
        data <- tmp
      }
    }
  }

  # Handle string of variable names
  if (is.character(data)) {
    tmp <- lapply(data, function(x)try(get(x),silent=TRUE))
    err <- sapply(tmp, is, class2="try-error")
    if (any(err)) {
      if (missingFile) stop(data, " is not visible as either a file or variable")
      if (notDput) 
        stop(data, " is a file that appears not to have been created with dput()")
      stop("cannot find object(s): ", paste(data[err],collapse=", "))
    }
    names(tmp) <- data
    data <- tmp
  }

  # Handle functional data creation
  if (is.function(data)) {
    funName <- deparse(substitute(data))
    if (!is.list(dataParams)) stop("dataParams must be a list")
    fargs <- formals(data)
    # Drop arguments not in data() unless data() has "..."
    if (is.na(match("...", names(fargs)))) {
      tmp <- which(is.na(match(names(dataParams), names(fargs))))
      if (length(tmp)>0) {
        warning("dataParams element(s) ", paste(names(dataParams)[tmp],sep="/"),
                " not in arguments to ", deparse(substitute(data)))
        dataParams <- dataParams[-tmp]
      }
    } else {
      fargs <- fargs[names(fargs)!="..."]
    }
    # Assure all needed parameters are present
    needed <- names(fargs)[sapply(fargs,is.name)]
    if (length(needed)>0) {
      err <- setdiff(needed, names(dataParams))
      if (length(err)>0)
        stop("Missing required argument(s) to ", deparse(substitute(data)), "(): ",
             paste(err, collapse=", "), " must be in dataParams")
    }
    # use the data() function to generate the data
    data <- do.call(data, dataParams)
    if (!is.list(data))
      stop("function ", funName, " did not return a list")
  }

  # Handle just a data.frame, also adding numeric attributes
  if (is.data.frame(data)) {
    # first assure that invalid names are not present
    nams <- names(data)
    tmp <- make.names(nams) != nams
    if (any(tmp)) {
      stop("invalid data.frame name(s): ", 
           paste(nams[tmp], collapse=", "))
    }
    # convert the data.frame to a list
    numAttr <- attributes(data)[sapply(attributes(data),mode)=="numeric" &
                          names(attributes(data))!="row.names"]
    Nsave <- nrow(data)
    if (ncol(data)==1) {
      tmp <- names(data)
      data <- list(data)
      names(data) <- tmp
    } else {
      data <- c(data)
    }
    if (!any(names(data)=="N")) data <- c(data, N=Nsave)
    dup <- names(numAttr) %in% names(data)
    if (any(!dup)) data <- c(data, numAttr)
    
  }

  # Expand data.frames (adding "N" if possible)
  dtfs <- sapply(data, is.data.frame)
  nams <- names(data)
  if (any(dtfs==FALSE) && (is.null(nams) || any(nams[which(!dtfs)]=="")))
    stop("data list has unnamed elements")
  if (any(dtfs)) {
    # To be very careful, first check for invalid names
    # (although illegal characters may get converted to periods,
    #  the variables will not match the names in the rube code)
    dnams <- unlist(lapply(data[dtfs],names))
    tmp <- dnams != make.names(dnams)
    if (any(tmp)) {
      stop("invalid name(s) in data.frame(s): ",  
        paste(dnams[tmp], collapse=", "))
    }
    # Do the combining based on the goal that the final expression
    # for combining data.frames and list element is of the form: 
    # c(list(N=data$N), data[[2]], data[[3]], recursive=FALSE), list(p=data$p))
    saveN <- ifelse(sum(dtfs)==1, nrow(data[[which(dtfs)]]), NA)
    expr <- NULL
    if (any(dtfs==FALSE)) {
      dnams <- names(data)[!dtfs]
      tmp <- dnams != make.names(dnams)
      if (any(tmp)) {
        stop("invalid name(s) in data: ",  
          paste(dnams[tmp], collapse=", "))
      }
      stdNames <- nams[which(!dtfs)]
      expr <- paste("list(", stdNames, "=data$", stdNames, ")",
                    sep="", collapse=", ")
    }
    dtfNums <- which(dtfs)
    expr <- paste(expr, 
                  paste("data[[", dtfNums, "]]", sep="", collapse=", "),
                  sep=", ")
    expr <- paste("c(", expr, ")")
    data = try(eval(parse(text=expr)), silent=TRUE)
    if (is(data,"try-error"))
      stop("Cannot convert data.frame(s) to list format")
    nams <- names(data)
    if (!is.na(saveN) && !any(nams=="N")) data[["N"]] = saveN  
  }

  # Check for duplicate variable names
  tmp <- duplicated(nams)
  if (any(tmp)) {
    stop("final data list has duplicated names: ",  
      paste(unique(nams[tmp]), collapse=", "))
  }
 

  # Check that all data are now numeric
  types <- sapply(data[names(data)!="drop"], mode)
  tmp <- types != "numeric"
  if (any(tmp)) {
    stop("final data list has non-numeric elements: ",  
      paste(unique(nams[tmp]), collapse=", "))
  }

  return(data)
}
