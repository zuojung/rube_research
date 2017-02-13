###########################################################
# unlister() used by rube(), but useful independently     #
# Object "data" is a list.  If one of its elements is     #
# named "unlist" which is a named list of strings that    #
# represent variable names, then each element of data     #
# that matches one of those strings is converted from a   #
# list to a vector, and renamed to the name in the        #
# "unlist" list.  E.g., unlist=list(A="B", C="C") will    #
# unlist "B" and rename it to "A", while "C" will just    #
# be unlisted.  Finally, the "unlist" element is removed. #
###########################################################
unlister <- function(data) {
  if (!is.na(match("unlist",names(data)))) {
    unlistNames = names(data$unlist)
    what = unlist(data$unlist)
    for (i in seq(along=what)) {
       data[what[i]] = list(as.numeric(unlist(data[[what[i]]])))
       names(data)[match(what[i],names(data))] = unlistNames[i]
    }
    data$unlist=NULL
  }
  return(data)
}

