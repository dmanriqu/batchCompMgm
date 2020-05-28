#Miscelaneous functions
date2str <- function(date) {
  if (is.null(date) || is.na(date)) {
    return(NA)
  } else if ("POSIXct" %in% class(date)) {
    return(as.character(format(date, "%Y-%m-%d %H:%M:%S %Z")))
  } else return('')
}

str2date <- function(strDate) {
  x <- tryCatch(expr = {as.POSIXct(strDate)}, error = function(err){NA})
  return(x)
}
tryCatch({as.POSIXct('2020-12-3')}, error = function(r)NA)
