# List of all files
sourcecode.list = list.files(path = "./source")

# Helper 1:
# Find function from source code list
sourceFunctionFind = function(fxname, filedir = "./source",
                              fetch.type = "Whatever") {
  fileList = list.files(path = filedir, full.names = TRUE)
  result = c()
  for (file in fileList){
    if (fetch.type == "Function") {
      defineFxn = paste(fxname, "<-")
    } else {
      defineFxn = fxname
    }
    # defineFxn = fxname
    fetch = grep(defineFxn, readLines(file))
    if (length(fetch > 0)) {
      result = c(result, file)
    }
  }
  return(result)
}

# What to do
# 1. Create a simple script for Rube
# 2. Attempt to run
# 3. Map the procedure
# 4. Locate and understand how the distributions work
# 5. Propose a method to allow customized distributions


## Step One: Simple Script for Rube ##
mastery.data <- list(x=4) # very little data!!
mastery.learning.model <- "model {
r <- 3
x ~ dnegbin(p,r)
p <- sqrt(2*u + 0.25) - 0.5
u ~ dunif(0,1)
}"
mastery.init <- function() {
  return(list(u=runif(1)))
}
# rube.test <- rube(mastery.learning.model, mastery.data, mastery.init)

## Step Two: Attempt to Run ##
## Step Three: Map the Procedre ##
# Run Rube:
# 1.Could not find function rube
source("./source/rube.R")
# 2. Model checking was unsuccessful
source("./source/composeModel.R")
  # 2a. could not find function "str_locate"
  library(stringr)
  # 2b. could not find function "subsText"
  source("./source/subsText.R")
  # 2c. could not find function "selectText"
  source("./source/selectText.R")
  # 2d could not find function "stochExprSub"
  source("./source/stochExprSub.R")
# 3. could not find function "cleanCode"
source("./source/cleanCode.R")
  # 3a could not find function "delimTally"
  source("./source/delimTally.R")
# 4. could not find function "makeData
source("./source/makeData.R")
#5. could not find function "bugsCheck"
source("./source/bugsCheck.R")
  # 5a. in match.fun(FUN) : object 'extractSyntax' not found
  source("./source/extractSyntax.R")
  # 5b. in FUN(X[[i]], ...) : could not find function "parseExpression"
  source("./source/parseExpression.R")
  # 5c. Error in parseExpression(parts[2], engine = engine) : 
  #   could not find function "expressionVars"
  source("./source/expressionVars.R")
  # 5d. in match.fun(FUN) : object 'getArgs' not found
  source("./source/getArgs.R")

# Full run!

debug(rube)
rube(mastery.learning.model, mastery.data, mastery.init)



# debug(sourceFunctionFind)
sourceFunctionFind("makeData")
sourceFunctionFind("getArgs", fetch.type = "Function")
sourceFunctionFind("rube")

## Step Four: Locate and understand how the distributions work ##
# Distribution Limitation:
sourceFunctionFind("priorExplore") # Graphical interaction?
sourceFunctionFind("valid distributions") # in extractSyntax
sourceFunctionFind("extractSyntax")

# 1. It appears that the distributions are checked during "bugsCheck",
#    If the distribution is not in a given list, it will return error
#    embedded in the sublist - problems

mastery.data <- list(x=4) # very little data!!
mastery.learning.model <- "model {
  r <- 3
  x ~ dweiner(a,b,c,d)
  p <- sqrt(2*u + 0.25) - 0.5
  u ~ dunif(0,1)
}"
mastery.init <- function() {
  return(list(u=runif(1)))
}
rube(mastery.learning.model, mastery.data, mastery.init, custom.dist = "test.custDist.txt")


##########################################################
# Use RWiener package for dwiener distribution test
