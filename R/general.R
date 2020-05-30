#Miscelaneous functions
date2str <- function(date) {
  if (is.null(date) || is.na(date)) {
    return(NA)
  } else if ("POSIXct" %in% class(date)) {
    return(as.character(format(date, "%Y-%m-%d %H:%M:%S %Z")))
  } else return('')
}

str2date <- function(str_date) {
  x <- tryCatch(expr = {as.POSIXct(str_date)}, error = function(err){NA})
  return(x)
}

replace_markers <- function(string, data) {
  x <- gregexpr(pattern = "<[^>]+>", text = string, perl = TRUE)
  labels <- unlist(regmatches(x = string, x))
  fields <-  gsub("<([^>]+)>", replacement = '\\1', x =  labels, perl = TRUE)
  subs <- data[fields]
  for (i in seq_along(fields)) {
    string <- gsub(pattern = labels[i], replacement = subs[i], x = string)
  }
  return(string)
}
