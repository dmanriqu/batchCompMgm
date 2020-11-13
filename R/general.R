#Miscelaneous functions
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
        x <- jsonlite::fromJSON(
          txt = in_str, 
          simplifyVector = TRUE,
          simplifyDataFrame = FALSE,
          simplifyMatrix = FALSE
        ) 
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
    date2str = function(obj_date) {
      x <- tryCatch(
        expr = {
          if (is.null(obj_date) || is.na(obj_date) || length(obj_date) == 0) 
            return(NULL)
          if (is(obj_date, 'POSIXlt') || is(obj_date, 'POSIXct' || is(obj_date, 'POSIXt'))){
            return(as.character(format(obj_date, "%Y-%m-%d %H:%M:%S %Z")))
          }
          NULL
        },
        error = function(e)return(NULL)
      )
      return(x)
    },
    str2date = function(str_date) {
      x <- tryCatch(
        expr = {
          if (is.null(str_date) || is.na(str_date) || length(str_date) == 0){
            stop()
          }
          as.POSIXlt(str_date)
        },
        error = function(err){NULL}
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
    },
    .get_time = function(){
      return(trunc(Sys.time(), unit = 'sec'))
    }
  ),
  active = list(
    persist_format = function(){
      private$.serializer$format
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

mgmObjFactory <- R6::R6Class(
  classname = 'mbmObjFactory',
  private = list(
    .classes = c('base_mgmObj', 'paramComp', 'paramBatchComp', 
                 'CTask', 'taskLog', 'CompMgm'),
    .validate = function(lst){
      if (!is.list(lst)){
        stop('Input need to be a list')
      }
      if (!'class' %in% names(lst)){
        stop('Not an object definition')
      }
      if (!is.character(lst$class)){
        stop('"class" field is not of type character.')
      }
      if (!lst$class[1] %in% private$.classes){
        stop("Class '", lst$class, "' not implemented.")
      }
    }
  ),
  public = list(
    list_def_2_obj = function(list_def){
      private$.validate(list_def)
      type <- list_def$class[1]
      o <- eval(parse(text = paste0(type, '$new()'))) 
      o$load_list_definition(list_def)
      return(o)
    },
    is_list_def = function(input){
      tryCatch(
       {
         private$.validate(input)
         TRUE
       },
        error = function(e){FALSE}
      )
    }
  )
)

o <- mgmObjFactory$new()
o$is_list_def(a$get_list_definition())

