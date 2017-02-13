# summarize bugs() results: used with summary.rube(...,drop>0)
# obj is sims.array component of a bugs object
summarize <- function(arr) {
  if (dim(arr)[2]==1) {
    bsummary <- function(x)
      return(c(mean(x), sd(x), quantile(x,c(0.025,0.25,0.50,0.75,0.975))))
    rtn <- t(apply(arr[,1,],2,bsummary))
    colnames(rtn) <- c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%")
  } else {
    rtn <- matrix(NA, dim(arr)[3], 7, dimnames=list(dimnames(arr)[[3]],NULL))
    n <- dim(arr)[1]
    for (i in 1:dim(arr)[3]) {
      means <- apply(arr[,,i], 2, mean)
      vars <- apply(arr[,,i], 2, var)
      wts <- 1/vars / sum(1/vars)
      rtn[i,1] <- sum(means*wts)       # mean
      rtn[i,2] <- sqrt(sum(vars*wts))  # sd
    }
    collapsed <- as.list(as.data.frame(apply(arr, 3, as.numeric)))
    rtn[,3:7] <- t(sapply(collapsed, quantile, probs = c(0.025,0.25,0.50,0.75,0.975)))
    rtn <- cbind(rtn, Rhat(arr))
    colnames(rtn) <- c("mean", "sd", "2.5%", "25%", "50%", "75%", "97.5%", "Rhat")
  }

  return(rtn)
}
