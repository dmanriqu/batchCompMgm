#' R6 class for tracking batch computation jobs
trialDescriptionCG <- R6::R6Class(
  'trialDescription',
  private = list(
    params = NULL,
    date_data = NULL,
    add = function(...){
      list_params <- list(...)
      if(!is.list(list_params)) stop('Input must be a named list of parameters')
      if(sum(unique(names(list_params)) != '') != length(list_params)){
        stop('Every parameter must have a different name')
      } 
      if( length(intersect(names(self$params), names(list_params))) > 0)
        stop('Cannot have duplicate parameter names')
      private$params <- append(private$params, list_params)
    },
    LOADED = FALSE
  ),
  active = list(
    values = function(value){
      if (missing(value)){
        return(private$params)
      } else {
        stop("'values' is read-only. Use (function) instead.")
      }
    },
    date = function(){
      return(private$date_data)
    }
  ),
  public = list(
    #' @description
    #' Load values for computation definition
    load_values = NULL,

    #' @description
    #' Change hair color.
    #' @param val New hair color.
    #' @examples
    #' P <- Person("Ann", "black")
    #' P$hair
    #' P$set_hair("red")
    #' P$hair 
    #' @return test
    initialize = function(...){
      #create fields and a function to load those fields
      self$load_values  <- function(){}
      a <- match.call(expand.dots = FALSE)$`...`
      bod <- paste(paste0('private$params$',paste(names(a), names(a), sep = ' = ')), collapse = '\n')
      bod <- paste0('if(private$LOADED){warning(\'Values were already loaded.\')} else {private$LOADED <- TRUE}\n',bod)
      b <- parse(text = paste('{', bod, '}', sep = '\n')) 
      formals(self$load_values) <- a
      body(self$load_values) <- b 
      private$params <- list()
      private$add(...)
      private$date_data <- date()
      invisible(self)
    },

    #' @description
    #' Produces a yaml definition of values
    #' @examples
    #' @param file
    #' @return string with the yaml defintion
    yaml = function(file = NULL){
      yaml::as.yaml(private$params, line.sep = '\n')
    },

    #' @description
    #' Produces a json definition of values
    #' @examples
    #' @param file
    #' @return string with the json defintion
    json = function(file = NULL){
      jsonlite::toJSON(private$params, pretty = TRUE)
    },
    equal = function(obj){
      #Override this method for altering the equality criterion (e.g. make it invariant to permutations)
      identical(self$values, obj$values)
    },
    print = function(){
      cat( paste(names(self$values), self$values, sep = ' = '),  sep = '\n')
      cat('Date:', self$date, '\n')
    },
    load_values_from_list = function(lista){
      do.call(self$load_values, lista)
    },
    filename = function(naming_field, rule = '%.rds'){
      gsub(pattern = '%', replacement = private$params[[naming_field]], x = rule)
    },
    load_cache = function(cached_params_name = 'params',
                           naming_field ='COMP_ID',   file_pattern = 'tmp/interm/%.rds'){
      #check if a computation file already exists, and read it if does
      fn <- self$filename(naming_field = naming_field, rule = file_pattern)
      if(file.exists(fn)) {
        x <- readRDS(fn)
        if(self$equal(x[[cached_params_name]])){
          return(x)
        }
      }
      return(NULL)
     },
    hash = function(method = c('sha256')){
      if(!require(digest))stop("package 'digest' required")
      return(digest::sha1(self$values))
    },
    hash_eq = function(o){
      digest::sha1(self$values) == self$hash()
    }
   )
)  

`==.trialDescription` = function(a, b){
 a$equal(b)
}

`!=.trialDescription` = function(a, b){
 !a$equal(b)
}

##Examples:
##Example of use
#CBasic_param <- R6::R6Class(
#  'basic_params', inherit = trialDescriptionCG,
#  public = list(
#    initialize = function(){
#      super$initialize(
#        COMP_ID = 'TEST_COMP',
#        agente_col = 'Agente',
#        bi = 10,
#        geo_col = 'stratum_david4',
#        lcmcr_lists = c('in_CICR', 'in_CEDAP', 'in_DP', 'in_COMISEDH', 'in_CNDDHH', 'in_CVR', 'in_MIMDES'),
#        loglin_lists = c('in_CICR', 'in_CEDAP', 'in_DP', 'in_COMISEDH', 'in_CNDDHH', 'in_CVR', 'in_MIMDES'),
#        nsam = 500,
#        th = 50,
#        imp_source = 'src/imputation_3.R',
#      )
#    }
#    # add code to override the equal method for other ways of comparing
#    #equal = function(q){
#    #  super$equal(q)    
#    #}
#  )
#)

utils::globalVariables(names = c('self', 'super'))
specializeTrialMgmtClass <- function(
  classname = 'newClass', 
  title = 'Normal simulation parameters',
  eq_fn = function(a,b){a$hash_eq(b)}, ...){
  R6::R6Class(
    'basic_params', inherit = trialDescriptionCG,
    public = list(
      initialize = function(){
        super$initialize(...)
      },
      # Override the 'equal' method for other ways of comparing
      equal = function(o){
        eq_fn(o, self)
        },
      print = function(){
        cat(title, '\n') 
        cat('------------------------------\n')
        super$print()
      }
    )
  )
}

# @description
# @examples
# @param
# @return 
