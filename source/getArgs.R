# From a text string, presumably from inside f(),
# extract the arguments, converting any numeric ones.
getArgs <- function(txt, blankIsNA=TRUE) {
  chrs <- strsplit(txt,"")[[1]]
  level <- cumsum(chrs%in%c("[","("))-cumsum(chrs%in%c("]",")"))
  comma <- c(0, which(level==0 & chrs==","), nchar(txt)+1)
  rtn <- NULL
  for (i in 1:(length(comma)-1))
    rtn <- c(rtn, list(gsub(" ","",substring(txt,comma[i]+1,comma[i+1]-1))))
  rtn <- lapply(rtn, function(x) {
                       y <- suppressWarnings(as.numeric(x))
                       if (!is.na(y)) {
                         x <- y
                       } else {
                         if (blankIsNA && x=="") x <- NA
                       }
                       return(x)
                     })
  return(rtn)
}
