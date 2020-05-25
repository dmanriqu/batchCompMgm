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
    initialize = function(file = NULL, parameter_names = NULL, parameter_list = NULL, eq_function = function(a,b){all.equal(a,b)}){
      if (!is.null(file)){
        #load file
        return(self)
      } else if (!is.null(parameter_names)){
        #create list with parameter names and call private$Padd()
      } else if (!is.null(parameter_list)){
        private$Padd(parameter_list)
      } else {
        stop('No initialization parameters provided. Aborting.')
      }
      private$Pdate  <-  date()
      private$Peq_function <- eq_function
      if (names(private$Pvalues)[1] != 'id') {
        stop('First element of the parameter list must be "id"')
      }
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
      cat('Date:', self$date, '\n')
    },
    load_values_from_list = function(lista){
      do.call(self$load_values, lista)
    },
    filename = function(naming_field, rule = '%.rds'){
      gsub(pattern = '%', replacement = private$Pparams[[naming_field]], x = rule)
    },
    #' @description
    #' Calculate sha256 hash of values.
    hash = function(){
      if(!require(digest))stop("package 'digest' required")
      return(digest::sha1(self$values))
    },
    #' @description
    #' Check if another paramComput object has the same values based on hash.
    hash_eq = function(other_obj){
      digest::sha1(self$values) == other_obj$hash()
    }, 
    getDefinition = function(){
      list(values = private$Pvalues, 
           date = private$Pdate, 
           eq_function =  paste(deparse(private$Peq_function), collapse = '\n')
      )
    },
    loadDefinition = function(def){
      private$Pvalues = def$values
      private$Pdate = def$date
      private$Peq_function = eval(parse(text=def$eq_function))
    },
    getJSONDef = function(){
      r <- self$getDefinition()
      jsonlite::toJSON(x = r, pretty = TRUE)
    },
    loadJSONDef = function(x){
      l <- jsonlite::fromJSON(x)
      private$Pvalues = l$values
      private$Peq_function = eval(parse(text=l$eq_function))
      private$Pdate = l$date
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
