## Custom Distribution and Function Entry
## The series of functions will allow users to enter functions
## and distributions that are currently not included in rube
## but is featured in JAGS in a constrained format
##
## Input: Working Directory (default Current), file name
## Process: with constrained user syntax, enter distribution, 
##          number of parameters, and limits of distrubiton
##          OR function, number of parameters
##          Then, add the parsed input into the list of legal
##          Distributions or function
## Output:  Distributions and Functions to add in extractSyntax
##          variable forms
customDistParser <- function(dir = getwd(), file) {
  file.dir <- paste0(dir,"/",file)
  input <- readLines(file.dir, warn = FALSE)
  result = list()
  
  ## Split Parsed strings of distribution or functions only of they have
  ## valid values -- to avoid error when there are no user inputs in
  ## distribution or functions
  ## Input: String of parsed user input, and the type of parsing operation needed
  ## Process: Depending on whether it's name, parameter or censor_default
  ##          parse the string according to type if the string is not empty
  ## Output: Parsed user input string
  conditionalstrsplit = function(string, type) {
    if (length(string) != 0) {
      if (type == "name") {
        result = strsplit(string, ",")[[1]]
      } 
      else if (type == "parameter") {
        result = as.numeric(strsplit(string, ",")[[1]])
      }
      else if (type == "censor_default") {
        result = strsplit(string, ";")[[1]]
      }
    } else {
      result = c()
    }
    return(result)
  }
  
  dist.line <- grep("Distribution:", input, value = TRUE)
  dist.line <- gsub(" ","",gsub("Distribution:", "", dist.line))
  dist.list <- conditionalstrsplit(dist.line, "name")
  
  dist.para.line <- grep("Distribution Parameters:", input, value = TRUE)
  dist.para.line <- gsub(" ","",gsub("Distribution Parameters:", "", 
                                     dist.para.line))
  dist.para.list = conditionalstrsplit(dist.para.line, "parameter")
  
  dist.censor.line <- grep("Censor defaults:", input, value = TRUE)
  dist.censor.line <- gsub(" ","",gsub("Censor defaults:", "", 
                                       dist.censor.line))
  dist.censor.list = conditionalstrsplit(dist.censor.line, "censor_default")
  
  fxn.line <- grep("Function:", input, value = TRUE)
  fxn.line <- gsub(" ","",gsub("Function:", "", fxn.line))
  fxn.list <- conditionalstrsplit(fxn.line, "name")
  
  fxn.para.line <- grep("Function Parameters:", input, value = TRUE)
  fxn.para.line <- gsub(" ","",gsub("Function Parameters:", "", 
                                    fxn.para.line))
  fxn.para.list <- conditionalstrsplit(fxn.para.line, "parameter")
  
  ## Paste @ symbol after names
  ## Input: name of the distribution or function, number of parameters
  ## Process: Paste the @ symbol for (number of parameters) times after the name
  ## Output: String formatted for validDists or ValidFunctions addition
  pasteParameters = function(name, parameters) {
    result = c()
    if (length(name) != 0 & length(parameters) != 0) {
      for (index in 1:length(name)) {
        temp.result = paste0(name[index],
                             paste(replicate(parameters[index], "@"), 
                                   collapse = ""))
        result = c(result, temp.result)
      }
    } else {
      result = c()
    }
    return(result)
  }
  
  ## Parse and organize censor default string
  ## Input: string of censor defaults
  ## Process: Seperate the censor distribution by comma and attach them back
  ##          to a vector, and then row bind all censor defaults for all
  ##          distributions
  ## Output: Matrix of number of distribution rows and 2 columns
  pasteCensor = function(censorList) {
    result = c()
    if (length(censorList) != 0) {
      for (index in 1:length(censorList)) {
        temp.censor = suppressWarnings(as.numeric(strsplit(censorList[index], 
                                                           ",")[[1]]))
        if (length(temp.censor) == 2) {
          result = rbind(result, c(temp.censor[1],temp.censor[2]))
        } else {
          stop("Must have 2 Censor Defaults for Custom Distributions")
        }
      }
    } else {
      result = c()
    }
    return(result)
  }
  
  ## String formatting and pasting
  if (length(dist.list) == length(dist.para.list) & 
     length(dist.censor.list) == length(dist.para.list)) {
    result$valid.dist = pasteParameters(dist.list, dist.para.list)
    result$censor = pasteCensor(dist.censor.list)
  } else{
    stop("Number of Custom Distributions, Parameters or Censor Defaults do not match")
  }
  
  if (length(fxn.list) == length(fxn.para.list)) {
    result$valid.fxn = pasteParameters(fxn.list, fxn.para.list)
  } else{
    stop("Number of Custom Functions and Parameters do not match")
  }
  
  return(result)
}