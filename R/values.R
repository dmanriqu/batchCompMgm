#' R6 class for tracking batch computation jobs
#'
#' @examples
#' CBasic_param <- R6::R6Class(
#'   'basic_params', inherit = paramComput,
#'   public = list(
#'     initialize = function(){
#'       super$initialize(
#'         COMP_ID = 'TEST_COMP',
#'         agente_col = 'Agente',
#'         bi = 10,
#'         geo_col = 'stratum_david4',
#'         lcmcr_lists = c('in_CICR', 'in_CEDAP', 'in_DP', 'in_COMISEDH', 'in_CNDDHH', 'in_CVR', 'in_MIMDES'),
#'         loglin_lists = c('in_CICR', 'in_CEDAP', 'in_DP', 'in_COMISEDH', 'in_CNDDHH', 'in_CVR', 'in_MIMDES'),
#'         nsam = 500,
#'         th = 50,
#'         imp_source = 'src/imputation_3.R',
#'       )
#'     }
#'     # add code to override the equal method for other ways of comparing
#'     #equal = function(q){
#'     #  super$equal(q)    
#'     #}
#'   )
#' )
paramComput <- R6::R6Class(
  'paramComput',
  private = list(
    Pparams = NULL,
    Pdate = NULL,
    PLOADED = FALSE,
    Padd = function(...){
      list_params <- list(...)
      if(!is.list(list_params)) stop('Input must be a named list of parameters')
      if(sum(unique(names(list_params)) != '') != length(list_params)){
        stop('Every parameter must have a different name')
      } 
      if( length(intersect(names(self$params), names(list_params))) > 0)
        stop('Cannot have duplicate parameter names')
      private$Pparams <- append(private$Pparams, list_params)
    }
  ),
  active = list(
    values = function(value){
      if (missing(value)){
        return(private$Pparams)
      } else {
        stop("'values' is read-only. Use (function) instead.")
      }
    },
    date = function(){
      return(private$Pdate)
    }
  ),
  public = list(
    #' @description
    #' Load values for computation definition
    load_values = NULL,

    #' @description
    #' Change hair color.
    #' @param ... parameters to track.
    #' @examples
    #' x <- paramComput$new(a = 1, b = 2)
    initialize = function(...){
      #create fields and a function to load those fields
      self$load_values  <- function(){}
      a <- match.call(expand.dots = FALSE)$`...`
      bod <- paste(paste0('private$Pparams$',paste(names(a), names(a), sep = ' = ')), collapse = '\n')
      bod <- paste0('if(private$PLOADED){warning(\'Values were already loaded.\')} else {private$PLOADED <- TRUE}\n',bod)
      b <- parse(text = paste('{', bod, '}', sep = '\n')) 
      formals(self$load_values) <- a
      body(self$load_values) <- b 
      private$Pparams <- list()
      private$Padd(...)
      private$Pdate <- date()
      invisible(self)
    },

    ##' @description
    ##' Produces a yaml definition of values
    ##' @param file file name for saving the output. To console if NULL.
    ##' @return string with the yaml defintion
    #yaml = function(file = NULL){
    #  yaml::as.yaml(private$Pparams, line.sep = '\n')
    #},

    #' @description
    #' Produces a json definition of values
    #' @param file file name for saving the output. To console if NULL.
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
      gsub(pattern = '%', replacement = private$Pparams[[naming_field]], x = rule)
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
    }
   )
)  

`==.paramComput` = function(a, b){
 a$equal(b)
}

`!=.paramComput` = function(a, b){
 !a$equal(b)
}

##Examples:
##Example of use
#CBasic_param <- R6::R6Class(
#  'basic_params', inherit = paramComput,
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

#' @param Either a named list indicating names and default values.
createParamComputClass <- function(
  classname = 'newClass', 
  title = 'Title of Class',
  eq_fn = function(a,b){a$hash_eq(b)}, ...){
  R6::R6Class(
    classname, inherit = paramComput,
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
