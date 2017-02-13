# Input is a vector of strings representing lines of a winBUGS model.
# Output is a cleaned-up vector of strings.
# Clean-up removes comments and blank lines, changes braces to K&R style,
# puts statements separated by semicolons (;) onto separate lines,
# and changes multiline statements to a single line.
# Comments are from "#" to the end of the line.
# Brace style has left braces only at the end of a line and right
# braces isolated as the only element of a line.
#
# E.g., cleanCode(c("model { x <-3; y~dunif(0,x+", "2)","  ", "z~dnorm(x,1)}"))
#       returns:
#       "model {"
#       "x <-3"
#       "y~dunif(0,x+ 2)"
#       "z~dnorm(x,1)"
#       "}"
#
cleanCode <- function(model) {
  # Preserve attributes
  oldAttributes <- attributes(model)
 
  # Convert tabs to spaces
  model <- gsub("\t", " ", model, fixed=TRUE)

  # Remove all comments
  comments <- grep("#", model, value=TRUE)
  comPos <- grep("#", model)
  if (length(comPos)>0) {
    model[comPos] = sapply(strsplit(comments,"#"),function(x)x[1])
  }
  
  # Remove blank lines
  model <- sub("^ +", "", model)
  model <- sub(" +$", "", model)
  model <- model[model!=""]

  ### Remove semicolons
  semiPos <- rev(grep(";", model))
  for (i in seq(along=semiPos)) {
    line <- semiPos[i]
    insrt <- strsplit(model[line],";")[[1]]
    insrt <- sub("^ +", "", insrt)
    insrt <- sub(" +$", "", insrt)
    insrt <- insrt[insrt!=""]
    m0 <- if (line>1) model[1:(line-1)] else NULL
    nn <- length(model)
    mn <- if (line<nn) model[(line+1):nn] else NULL
    model <- c(m0, insrt, mn)
  }
  # Store regularized model in .model:
  assign(".model", model, .GlobalEnv)
  

  # Check { and } matching
  braceCount <- delimTally(model, assumeSingles=FALSE)
  if (length(braceCount)==0) stop("Models must be enclosed in curly braces")
  bracePos <- as.numeric(colnames(braceCount))

  if (sum(braceCount[1,])<1) stop("Need at least one '{'")
  if (sum(braceCount[2,])<1) stop("Need at least one '}'")
  if (any(cumsum(braceCount[1,])<cumsum(braceCount[2,]))) {
    cumBrace <- cumsum(braceCount[1,])-cumsum(braceCount[2,])
    err <- bracePos[which(cumBrace<0)[1]]
    stop("'}' without matching '{' on line ", err,  " in .model :",
         model[err])
  }
  if (sum(braceCount[1,])<sum(braceCount[2,])) {
    stop(sum(braceCount[2,])-sum(braceCount[1,]), " more '}'s than '{'s")
  }
  if (sum(braceCount[1,])>sum(braceCount[2,])) {
    stop(sum(braceCount[1,])-sum(braceCount[2,]), " more '{'s than '}'s")
  }

  ### Clean up { and } in K&H style: { at end of previous line and } alone
  # Isolate right braces
  tmp <- bracePos[braceCount[2,]>0]
  pos <- rev(tmp[nchar(model[tmp])>1])
  redo <- FALSE # need to re-do delimTally() due to changing line count?
  for (i in seq(along=pos)) {
    line <- pos[i]
    tmp <- strsplit(paste(" ",model[line]," "),"}")[[1]]
    insrt <- rep("}",length(tmp)*2-1)
    insrt[seq(1,by=2,len=length(tmp))] <- tmp
    insrt <- sub("^ +", "", insrt)
    insrt <- sub(" +$", "", insrt)
    insrt <- insrt[insrt!=""]
    if (length(insrt)>1) redo <- TRUE
    m0 <- if (line>1) model[1:(line-1)] else NULL
    nn <- length(model)
    mn <- if (line<nn) model[(line+1):nn] else NULL
    model <- c(m0, insrt, mn)
  }
  if (redo) {
    braceCount <- delimTally(model, assumeSingles=TRUE)
    bracePos <- as.numeric(colnames(braceCount))
  } 

  # move up left braces to end of line
  pos <- rev(bracePos[braceCount[1,]>0])
  for (i in seq(along=pos)) {
    line <- pos[i]
    tmp <- strsplit(paste(" ",model[line]," "),"{", fixed=TRUE)[[1]]
    insrt <- rep("{",length(tmp)*2-1)
    insrt[seq(1,by=2,len=length(tmp))] <- tmp
    insrt <- sub("^ +", "", insrt)
    insrt <- sub(" +$", "", insrt)
    insrt <- insrt[insrt!=""]
    bpos <- which(insrt=="{")
    if (any(diff(bpos)==1)) {
      stop("Adjacent '{'s at line ", line, " in .model: ", model[line])
    }
    if (bpos[1]==1) {
      if (line==1) stop("Model cannot start with '{'")
      model[line-1] <- paste(model[line-1],"{")
      insrt <- insrt[-1]
      bpos <- bpos[-1]
      if (length(bpos)>0) bpos <- bpos - 1
    }
    if (length(bpos)>0) {
      insrt[bpos-1] <- paste(insrt[bpos-1],"{")
      insrt <- insrt[-bpos]
    }
    if (line==1) {
      m0 <- NULL
    } else {
      m0 <- model[1:(line-1)]
    }
    nn <- length(model)
    mn <- if(line<nn) model[(line+1):nn] else NULL
    model <- c(m0, insrt, mn)
  }
  
  # Check for balanced parentheses
  unbal <- as.numeric(which(sapply(model, function(x) {
                 x=strsplit(x,"")[[1]]
                 return(sum(x=="(") != sum(x==")"))})))
  if (length(unbal)>0) {
    if (length(unbal)==1) {
      stop("unbalanced () at ", unbal, " in .model: ", model[unbal])
    } else {
      stop("multiple unbalanced () starting at ", unbal[1], " in .model: ", model[unbal[1]])
    }
  }
  model <- gsub("^[[:space:]]*", "", model)
  model <- model[nchar(model)>0]
  attributes(model) <- oldAttributes
  return(model)
} # end cleanCode()

