model.character <- function(x, ..., indent="  ", lineNumbers=TRUE) {
  if (length(x)==1 && length(grep("^[[:space:]]*model[[:space:]]*[{]", x))==0) {
    if (!file.exists(x)) stop("cannot open model file ",x)
    temp <- try(readLines(x, warn=FALSE))
    if (is(temp,"try-error")) stop("cannot read contents of model file ",x)
    x <- temp
  }
  showModel(x, ..., indent=indent, lineNumbers=lineNumbers)
}

