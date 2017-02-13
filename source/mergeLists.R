#####################################################################
# Convenience function to substitute/add to a list of substitutions #
#####################################################################

mergeLists <- function(list1, list2) {
  if (!is.list(list1) || !is.list(list2))
    stop("Arguments must be lists.")
  names1 = names(list1)
  names2 = names(list2)
  if (is.null(names1) || is.null(names2) || any(names1=="") || any(names2==""))
    stop("All list elements must be named.")
  New = setdiff(names2, names1)
  Old = setdiff(names2, New)
  if (length(Old)>0) list1[match(Old,names1)] = list2[Old]
  if (length(New)>0) list1 = c(list1, list2[New])
  return(list1)
}

