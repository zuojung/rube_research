## Allowing importing custom distribution that is in Jags but not in Rube
## Input: Working Directory (default Current), file name
## Process: with constrained user syntax, enter distribution, 
##          number of parameters, and limits of distrubiton
##          OR function, number of parameters
##          Then, add the parsed input into the list of legal
##          Distributions or function
## Output:  Distributions and Functions to add in extractSyntax
##          variable forms

## Helpers:
##
## Paste @ x parameter times after name
pasteParameters = function(name, parameters) {
  result = c()
  for (index in 1:length(name)) {
    temp.result = paste0(name[index],
                         paste(replicate(parameters[index], "@"), collapse = ""))
    result = c(result, temp.result)
  }
  return(result)
}

## Parse and organize censor list
pasteCensor = function(censorList) {
  result = c()
  for (index in 1:length(censorList)) {
    temp.censor = suppressWarnings(as.numeric(strsplit(censorList[index], ",")[[1]]))
    result = rbind(result, c(temp.censor[1],temp.censor[2]))
  }
  return(result)
}

## Issues: 1. readLines error unless space after last line
##         2. User unfriendly entry of censor value


## Parser Function to get ready for inputs
customDistParser <- function(dir = getwd(), file) {
  file.dir <- paste0(dir,"/",file)
  # print(file.dir)
  input <- readLines(file.dir)
  result = list()
  
  ## Parse the line:
  ## 1. Whether its a distribution or function
  ## 1a. What is the CensDefault for the distribution
  ## 2. How many parameters are there
  
  dist.line <- grep("Distribution:", input, value = TRUE)
  dist.line <- gsub(" ","",gsub("Distribution:", "", dist.line))
  dist.list <- strsplit(dist.line, ",")[[1]]
  
  dist.para.line <- grep("Distribution Parameters:", input, value = TRUE)
  dist.para.line <- gsub(" ","",gsub("Distribution Parameters:", "", dist.para.line))
  dist.para.list <- as.numeric(strsplit(dist.para.line, ",")[[1]])
  
  dist.censor.line <- grep("Censor defaults:", input, value = TRUE)
  dist.censor.line <- gsub(" ","",gsub("Censor defaults:", "", dist.censor.line))
  dist.censor.list <- strsplit(dist.censor.line, ";")[[1]]
  
  fxn.line <- grep("Function:", input, value = TRUE)
  fxn.line <- gsub(" ","",gsub("Function:", "", fxn.line))
  fxn.list <- strsplit(fxn.line, ",")[[1]]
  
  fxn.para.line <- grep("Function Parameters:", input, value = TRUE)
  fxn.para.line <- gsub(" ","",gsub("Function Parameters:", "", fxn.para.line))
  fxn.para.list <- as.numeric(strsplit(fxn.para.line, ",")[[1]])
  
  if (length(dist.list) == length(dist.para.list) & 
     length(dist.censor.list) == length(dist.para.list)) {
    
    result$valid.dist = pasteParameters(dist.list, dist.para.list)
    
    tmp.censor = pasteCensor(dist.censor.list)
    result$censor = tmp.censor
  } else{
    stop("Error: Number of Distributions and Parameters does not match")
  }
  
  if (length(fxn.list) == length(fxn.para.list)) {
    result$valid.fxn = pasteParameters(fxn.list, fxn.para.list)
  } else{
    stop("Error: Number of Functions and Parameters does not match")
  }
  
  return(result)
}

kk = customDistParser(file = "test.custDist.txt")
