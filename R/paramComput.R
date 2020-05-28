#' R6 class for parameters of computation 
#'
#' @examples
paramComput <- R6::R6Class(
# paramComput <- R6Class(
  'paramComput',
  private = list(
    Pvalues= NULL,
    Pdate = NULL,
    Padd = function(list_params){
      if(!is.list(list_params)) stop('Input must be a named list of parameters')
      if(sum(unique(names(list_params)) != '') != length(list_params)){
        stop('Every parameter must have a different name')
      } 
      if( length(intersect(names(self$params), names(list_params))) > 0)
        stop('Cannot have duplicate parameter names')
      private$Pvalues<- append(private$Pvalues, list_params)
    },
    Pfunc2str = function(funcStr){
      paste(deparse(funcStr), collapse = '\n')
    },
    Pstr2func = function(func){
      eval(parse(text=func))
    },
    # THis function will be set during initialization
    Peq_function = NULL
  ),
  active = list(
    values = function(){
        return(private$Pvalues)
    },
    date = function(){
      return(private$Pdate)
    }
  ),
  public = list(
    #' @description
    #' Change hair color.
    #' @param ... parameters to track.
    #' @examples
    #' x <- paramComput$new(a = 1, b = 2)
    initialize = function(file = NULL, strJSON  = NULL, parameter_names = NULL, parameter_list = NULL, eq_function = function(a,b){all.equal(a,b)}){
      if (!is.null(file)){
        self$loadJSONDef(file)
        return(self)
      } else if (!is.null(strJSON)) {
        self$loadJSONDef(strJSON )
        return(self)
      } else if (!is.null(parameter_names)) {
        #create list with parameter names and call private$Padd()
      } else if (!is.null(parameter_list)) {
        private$Padd(parameter_list)
      } else {
        stop('No initialization parameters provided. Aborting.')
      }
      if (names(parameter_list)[1] != 'id') {
        stop('First element of the parameter list must be "id"')
      }
      private$Pdate = Sys.time()
      private$Peq_function = eq_function
    },

    equal = function(obj){
      if (private$Pvalues$id != obj$values$id) {
        warning('Comparing objects with different ids.')
        return(FALSE)
      }
      private$Peq_function(self$values, obj$values)
    },
    print = function(){
      cat( paste(names(self$values), self$values, sep = ' = '),  sep = '\n')
      cat('Date:', as.character(self$date), '\n')
    },
    # load_values_from_list = function(lista){
    #   do.call(self$load_values, lista)
    # },
    # filename = function(naming_field, rule = '%.rds'){
    #   gsub(pattern = '%', replacement = private$Pparams[[naming_field]], x = rule)
    # },
    getDefinition = function(){
      list(class = 'paramComput', values = private$Pvalues, 
           date = date2str(private$Pdate),
           eq_function = private$Pfunc2str(private$Peq_function)
      )
    },
    loadDefinition = function(def){
      if(def$class != 'paramComput')
        stop('Wrong "class" attribute')
      private$Pvalues = def$values
      private$Pdate = str2date(def$date)
      private$Peq_function = private$Pstr2func(def$eq_function)
    },
    writeJSONDef = function(file = NULL){
      r <- self$getDefinition()
      if (is.null(file)){
        jsonlite::toJSON(x = r, pretty = TRUE, null = 'null', na = 'null', auto_unbox = TRUE)
      } else {
        jsonlite::write_json(x = r, path = file, pretty = TRUE, auto_unbox = TRUE)
      }
    },
    loadJSONDef = function(strJSON = NULL, file = NULL) {
      if (!is.null(str)) {
        l <- jsonlite::fromJSON(strJSON, simplifyVector = TRUE)
      } else {
        l <- jsonlite::read_json(path = file, simplifyVector = TRUE)
      }
      if(l$class != 'paramComput')
        stop('Wrong "class" attribute')
      private$Pvalues = l$values
      private$Pdate = str2date(l$date)
      private$Peq_function = private$Pstr2func(l$eq_function)
    }
   )
)  

`==.paramComput` = function(a, b){
 a$equal(b)
}

`!=.paramComput` = function(a, b){
 !a$equal(b)
}


utils::globalVariables(names = c('self', 'super'))


# @description
# @examples
# @param
# @return 

# For evaluating a command given as a string in R use eval(parse(text=string))
# For converting an R object to string use: paste(deparse(obj), collapse = '\n')
