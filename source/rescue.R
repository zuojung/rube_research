## When WinBUGS fails, but "bin" is set, the results up to within "bin" iterations
## of the crash will be saved in codaIndex.txt and coda#.txt.
## With what=MCMC, rescue() places the last "n" recorded iterations into a
## (partially complete) rube object.  With what=startVals, the last recorded
## values (or n before that if n>0) will be recorded in a list for uses
## as starting values for another run.
## The value n=NA indicates take all possible iterations when what=MCMC;
## otherwise it indicates to take the last recorded values as the starting values.
### WILL NOT WORK CORRECTLY WITH ARRAYS
rescue <- function(what=c("MCMC","startVals"), n=NA) {
  if ("package:R2jags" %in% search()) stop("rescue() does not work with jags")
  what <- match.arg(what)

  # Just get starting values from last successful recorded iteration
  if (what == "startVals") {
    ### WILL NOT WORK CORRECTLY WITH ARRAYS
    if (is.na(n)) n <- 0
    if (n<0) n <- -n
    K  <-  length(list.files(pattern="coda[[:digit:]]+[.]txt"))
    index  <-  read.table("codaIndex.txt")
    N  <-  index[1,3]
    if (n<0 || n>=N) stop("bad n")
    M  <-  nrow(index)
    rows  <-  seq(from=N-n, by=N, length=M)
    lst  <-  NULL
    for (k in 1:K) {
      code  <-  paste(index[,1], "=", read.table(paste("coda",k,".txt",sep=""))[rows,2], sep="")
      dev <- which(substring(code,1,9)=="deviance=")
      if (length(dev)>0) code <- code[-dev]
      leftBracket <- str_locate(code, "\\[")[,1]
      isArr <- !is.na(leftBracket)
      hasArr <- any(isArr)
      if (hasArr) {
        arr <- code[isArr]
        code <- code[!isArr]
      }
      # handle non-arrays
      lst  <-  c(lst, list(eval(parse(text=paste("list(",paste(code,collapse=","),")")))))
      # handle arrays
      if (hasArr) {
        arrNames <- unique(substring(arr,1,leftBracket[isArr]-1))
        for (arrName in arrNames) {
          this <- arr[arrNames==arrName]
          eqls <- str_locate(this,"=")[,1]
          if (any(is.na(eqls))) stop("error parsing array equal sign")
          brackets <- str_locate(this,"\\[.*\\]")
          if (any(is.na(brackets[,1]))) stop("error parsing array equal sign")
          indices <- substring(this, brackets[,1]+1, brackets[,2]-1)
          right <- substring(this, eqls+1, nchar(this)-1)
          commas <- str_locate_all(indices[1],",")
          dm <- length(commas[[1]][,1]) + 1
          if (dm<1) stop("error parsing array commas")
          if (dm>3) stop("arrays with >3 dimensions not coded yet")
          if (dm==1) {
            ind <- try(suppressWarnings(as.numeric(indices)), silent=TRUE)
            if (is(ind,"try-error") || any(is.na(ind))) stop("can't read array indices")
            vals <- try(suppressWarnings(as.numeric(right)), silent=TRUE)
            if (is(vals,"try-error") || any(is.na(vals))) stop("can't read array values")
            tmpA <- rep(NA,max(ind))
            tmpA[ind] <- vals
          } else if (dm==2) {
            c1 <- sapply(commas, function(x)x[1,1])
            ind1 <- try(suppressWarnings(as.numeric(substring(indices,1,c1-1))), silent=TRUE)
            if (is(ind1,"try-error") || any(is.na(ind1))) stop("can't read array indices")
            ind2 <- try(suppressWarnings(as.numeric(substring(indices,c1+1))), silent=TRUE)
            if (is(ind2,"try-error") || any(is.na(ind2))) stop("can't read array indices")
            vals <- try(suppressWarnings(as.numeric(right)), silent=TRUE)
            if (is(vals,"try-error") || any(is.na(vals))) stop("can't read array values")
            rmax <- max(ind1)
            cmax <- max(ind2)
            tmpA <- matrix(NA, rmax, cmax)
            tmpA[cbind(ind2,ind1)] <- vals
          } else {
            c1 <- sapply(commas, function(x)x[1,1])
            ind1 <- try(suppressWarnings(as.numeric(substring(indices,1,c1-1))), silent=TRUE)
            if (is(ind1,"try-error") || any(is.na(ind1))) stop("can't read array indices")
            c2 <- sapply(commas, function(x)x[2,1])
            ind2 <- try(suppressWarnings(as.numeric(substring(indices,c1+1,c2-1))), silent=TRUE)
            if (is(ind2,"try-error") || any(is.na(ind2))) stop("can't read array indices")
            ind3 <- try(suppressWarnings(as.numeric(substring(indices,c2+1))), silent=TRUE)
            if (is(ind3,"try-error") || any(is.na(ind3))) stop("can't read array indices")
            vals <- try(suppressWarnings(as.numeric(right)), silent=TRUE)
            if (is(vals,"try-error") || any(is.na(vals))) stop("can't read array values")
            rmax <- max(ind1)
            cmax <- max(ind2)
            tmax <- max(ind3)
            tmpA <- array(NA, c(rmax, cmax, tmax))
            tmpA[cbind(ind3,ind2,ind1)] <- vals
          }
          lst[[k]] <- c(lst[[k]], list(tmpA))
          names(lst[[k]])[length(lst[[k]])] <- arrName
        }
      }
    }
    return(lst)
  }

  ## what=MCMC: create a rube object
  K  <-  length(list.files(pattern="coda[[:digit:]]+[.]txt"))
  index  <-  read.table("codaIndex.txt")
  if (is.na(n)) n <- index[1,3]
  if (n<2 || n>index[1,3]) stop("bad n")
  allNames  <-  as.character(index[,1])
  #lbLoc  <-  str_locate(allNames, "\\[")[,1]
  #baseNames  <-  allNames
  #baseNames[!is.na(lbLoc)]  <-  substring(baseNames[!is.na(lbLoc)], 1, lbLoc[!is.na(lbLoc)]-1)
  #uNames  <-  unique(baseNames)
  M  <-  nrow(index)
  Names  <-  index[,1]
  rows  <-  apply(index, 1, function(r, p) {m=as.numeric(r[3]); return(paste(m-p+1,":",m,sep=""))}, p=n)
  rows  <-  eval(parse(text=paste("c(",paste(rows,collapse=","),")")))
  mat  <-  matrix(NA, M*n, K)
  for (k in 1:K) mat[,k]  <-  read.table(paste("coda",k,".txt",sep=""))[rows,2]
  sims.array  <-  array(NA, c(n,K,M))
  for (m in 1:M) {
    for (k in 1:K) {
      sims.array[,k,m]  <-  mat[(m*n-n+1):(m*n),k]
    }
  }
  # collapse arrays
  #if (!exists("str_locate")) library(stringr)
  #Sel  <-  str_locate(Names, "\\[.+\\]$")
  #if (any(!is.na(Sel[,1]))) {
  #  stop("arrays not implemented")
  #}
  dimnames(sims.array) <- list(NULL,NULL,Names)
  rslt  <-  list(sims.array=sims.array)
  dm <- dim(rslt$sims.array)
  rslt$n.keep <- dm[1]
  rslt$n.chains <- dm[2]
  rslt$summary <- summarize(sims.array)
  rslt$n.burnin <- NA
  rslt$n.thin <- NA
  rslt$isDIC <- FALSE
  class(rslt) <- c("rube","bugs")
  return(rslt)
}


