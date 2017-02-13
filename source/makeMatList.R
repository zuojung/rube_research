##############################################################
# Recompute sims.matrix and sims.list as permuted sims.array #
##############################################################
makeMatList <- function(sims.array) {
  allNames <- dimnames(sims.array)[[3]]
  sims.matrix <- matrix(NA, nrow=prod(dim(sims.array)[1:2]), 
                        ncol=dim(sims.array)[3],
                        dimnames=list(NULL, allNames))
  for (i in 1:dim(sims.array)[3]) {
    sims.matrix[,i] <- sample(sims.array[,,i])
  }
  temp <- as.numeric(regexpr("[", allNames, fixed=TRUE)) - 1
  isVec <- allNames[temp==-2]
  temp[temp==-2] <- nchar(allNames[temp==-2])
  wholeNames <- unique(substring(allNames,1,temp))
  dimension.short <- as.numeric(is.na(match(wholeNames,isVec)))
  sims.list <- vector("list", length(wholeNames))
  names(sims.list) <- wholeNames
  dn <- dimnames(sims.array)[[3]]
  for (i in 1:length(wholeNames)) {
    v <- wholeNames[i]
    if (dimension.short[i]==0) {
      Sel <- which(dn==v)
      if (length(Sel)!=1)
        stop("cannot convert single variable to sims.list")
      sims.list[[i]] <- as.numeric(sims.matrix[,Sel])
    } else {
      Sel <- which(substring(dn,1,nchar(v)+1)==paste(v,"[",sep=""))
      if (length(Sel)<1)
        stop("cannot convert vector variable to sims.list")
      sims.list[[i]] <- sims.matrix[,Sel]
      dimnames(sims.list[[i]]) <- NULL
    }
  }
  return(list(sims.list=sims.list, sims.matrix=sims.matrix))
}
