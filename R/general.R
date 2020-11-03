#Miscelaneous functions
date2str <- function(date) {
  if (is.null(date) || is.na(date)) {
    return(NA)
  } else if ("POSIXct" %in% class(date)) {
    return(as.character(format(date, "%Y-%m-%d %H:%M:%S %Z")))
  } else return('')
}

str2date <- function(str_date, na = as.POSIXct(NA)) {
  x <- tryCatch(
    expr = {as.POSIXct(str_date)}, 
    error = function(err){na}
  )
  return(x)
}

replace_markers <- function(string, data) {
  x <- gregexpr(pattern = "<[^>]+>", text = string, perl = TRUE)
  labels <- unlist(regmatches(x = string, x))
  fields <-  gsub("<([^>]+)>", replacement = '\\1', x =  labels, perl = TRUE)
  subs <- data[fields]
  if(length(subs) < length(fields)) stop("Substitution fields not found")
  for (i in seq_along(fields)) {
    string <- gsub(pattern = labels[i], replacement = subs[i], x = string)
  }
  return(string)
}

generate_names_from_pattern <- function(obj, pattern, sep = '\n', before = '', after = '', file = ''){
  res <- character()
  if ('CompMgm' %in% class(obj)) {
    obj <- obj$params
  }
  if ('paramComp' %in% class(obj)){
    res <- paste(obj$string_from_fields(pattern), collapse = sep)
  } else {
    stop('Wrong type of object (can only be CompMgm or paramComp')
  }
  cat(before, res, after, file = file, sep ='')
}

serializer <- R6::R6Class(
  classname = 'serializer',
  private = list(
    .type = 'json',
    .class_builders = list(
      generic_registered = function(lst){
        if (is.null(lst$class)) return()
        x <- eval(parse(text = paste0(lst$class[1], '$new()')))
        x$load_list_definition(lst)
        return(x)
      },
      generic = function(){}
    ),
    .list2obj = function(lst){
      for (f in private$.class_builders){
        r <- f(lst)
        if (!is.null(r)) return(r)
      }
    }
    #deep_copy = function(name, value){
    #  
    #}
  ),
  active = list(
    format = function(){
      return(private$.type)
    }
  ),
  public = list(
    initialize = function(type = c('json', 'yaml')){
      private$.type <- type[1]
      if (!(type %in% (c('json', 'yaml')))){
        stop('Serialization format not recognized.') 
      }
    },
    serial_2_object = function(in_str){
      x <- serial_2_listdef(in_str)
      private$.list2obj(x)
    },
    serial_2_listdef =function(in_str){
      if (private$.type == 'json'){
        x <- jsonlite::fromJSON(txt = in_str, simplifyVector = TRUE) 
      } else if (private$.type == 'yaml'){
        x <- yaml::yaml.load(in_str)
      } else {stop('Serialization format not recognized.')}
    },
    listdef_2_serial = function(obj){
      if (private$.type == 'json'){
        s <- jsonlite::toJSON(
          obj, pretty = TRUE, null = "null",
          na = "null", auto_unbox = TRUE
        )
      } else if (private$.type == 'yaml'){
        s <- yaml::as.yaml(obj)
      }
      return(s) 
    },
    date2str = function(date) {
      if (is.null(date) || is.na(date)) {
        return(NA)
      } else if ("POSIXct" %in% class(date)) {
        return(as.character(format(date, "%Y-%m-%d %H:%M:%S %Z")))
      } else return('')
    },
    str2date = function(str_date, na = as.POSIXct(NA)) {
      x <- tryCatch(
        expr = {as.POSIXct(str_date)}, 
        error = function(err){na}
      )
      return(x)
    }
  )
) 
base_mgmObj <- R6::R6Class(
  #define basic interface and communication methods
  classname = 'base_mgmObj',
  private = list(
    .silent = NULL,
    .serializer = NULL,
    .replace_markers = function(string, data) {
      x <- gregexpr(pattern = "<[^>]+>", text = string, perl = TRUE)
      labels <- unlist(regmatches(x = string, x))
      fields <-  gsub("<([^>]+)>", replacement = '\\1', x =  labels, perl = TRUE)
      subs <- data[fields]
      if(length(subs) < length(fields)) stop("Substitution fields not found")
      for (i in seq_along(fields)) {
        string <- gsub(pattern = labels[i], replacement = subs[i], x = string)
      }
      return(string)
    },
    .message = function(...){
      if (private$.silent) invisible()
      message(...)
    }
  ),
  public = list(
    initialize = function(persist_format = c('json', 'yaml')){
      private$.serializer <- serializer$new(type = persist_format[1])
      private$.silent <- FALSE
    },
    get_list_definition = function() {
      stop('get_list_definition() not implemented')
    },
    load_list_definition = function(lst) {
      stop('load_list_definition() not implemented')
    },
    save = function(file_name = NULL){
      x <- self$get_list_definition()
      s <- private$.serializer$listdef_2_serial(x)
      if (!is.null(file_name)){
        cat(s, file = file_name)
      } else {
       return(s) 
      }
    },
    load = function(file_name = NULL, string = NULL){
      if (!is.null(file_name)){
        s <- readChar(file_name, file.info(file_name)$size) 
      } else if (!is.null(string)) {
        s <- string
      } else {
        stop('Need either a file name or a formatted string')
      }
      l <- private$.serializer$serial_2_listdef(s)
      self$load_list_definition(l)
    },
    set_silent_on = function(){
      private$.silent <- TRUE
    },
    set_silent_off = function(){
      private$.silent <- FALSE
    }
  )
)
