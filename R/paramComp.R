#' R6 class for parameters of computation
#'
#' @examples
paramComp <- R6::R6Class(
  classname = "paramComp",
  inherit = base_mgmObj,
  lock_objects = FALSE,
  private = list(
    .values = NULL,
    .date = NULL,
    .add = function(list_params) {
      if (is.null(private$.values)) private$.values <- list()
      if (!is.list(list_params)) {
        stop("Input must be a named list of parameters")
      } else if (sum(unique(names(list_params)) != "") != length(list_params)) {
        stop("Every parameter must have a different name")
      } else if (length(intersect(names(self$params), names(list_params))) > 0) {
        stop("Cannot have duplicate parameter names")
      } else if (!'id_comp' %in% names(private$.values) && !'id_comp' %in% names(list_params)) {
        stop("First element of the parameter list must be 'id_comp'")
      } else if (!'id_value_set' %in% names(private$.values) && !'id_value_set' %in% names(list_params)) {
        stop("Second element of the parameter list must be 'id_value_set'")
      }
      private$.values <- modifyList(private$.values, list_params)
    },
    .func2str = function(func_str) {
      paste(deparse(func_str), collapse = "")
    },
    .str2func = function(func) {
      eval(parse(text = func))
    },
    # THis function will be set during initialization
    .eq_function = NULL
  ),
  active = list(
    values = function() {
        return(private$.values)
    },
    date = function() {
      return(private$.date)
    },
    is_loaded = function() {
      return(length(private$.values) > 0)
    }
  ),
  public = list(
    #' @description
    #' Change hair color.
    #' @param ... parameters to track.
    #' @examples
    #' x <- paramComp$new(a = 1, b = 2)
    initialize = function(
      load_from_file = NULL,
      parameter_list = NULL,
      eq_function = function(a, b) {all.equal(a, b)},
      persist_format = c('json','yaml')
    ) {
      super$initialize(persist_format[1])
      if (!is.null(load_from_file)) {
        self$load(load_from_file)
      } else if (!is.null(parameter_list)) {
        private$.add(parameter_list)
      } 
      private$.eq_function <- eq_function
      private$.date <- Sys.time()
      invisible(self)
    },
    equal = function(obj) {
      if (!self$is_loaded) stop("Data not loaded in object paramComp")
      if (private$.values$id_comp != obj$values$id_comp) {
        warning("Comparing objects with different id_comp.")
        return(FALSE)
      }
      private$.eq_function(self$values, obj$values)
    },
    print = function() {
      cat('Object class: [', paste(class(self), collapse = ', '), ']\n')
      cat(paste(names(self$values), self$values, sep = " = "),  sep = "\n")
      cat("Date:", as.character(self$date), "\n")
    },
    get_list_definition = function(str_dates = TRUE) {
      if (!self$is_loaded) warning("Data not loaded in object paramComp")
      list(class = class(self),
           values = private$.values,
           date = (if (str_dates) private$.serializer$date2str(private$.date) else  private$.date),
           eq_function = private$.func2str(private$.eq_function)
      )
    },
    load_list_definition = function(def = NULL, str_dates = TRUE) {
      if (!("paramComp" %in% def$class)) {
        stop("Wrong 'class' attribute")
      }
      private$.values <- def$values
      private$.date <- (if (str_dates) private$.serializer$str2date(def$date) else def$date)
      private$.eq_function <- private$.str2func(def$eq_function)
      invisible(self)
    },
    save = function(file_name_pattern = NULL){
      #overriding method for validation and use of patterned names
      if (!self$is_loaded) {
        warning("Data not loaded in object paramComp")
        return()
      }
      r <- self$get_list_definition(str_dates = TRUE)
      if (!is.null(file_name_pattern)){
        file_name_pattern <- private$.replace_markers(file_name_pattern, data = private$.values)
      }
      super$save(file_name_pattern)
    },
    string_from_fields= function(pattern = "<id_comp>_<id_value_set>_param.json") {
      private$.replace_markers(pattern, data = private$.values)
    },
    update_fields = function(lst_fields_values) {
      if (!is.list(lst_fields_values)){
        stop('Need to provide a named list of fields and values.')
      }
      old <- new <- character()
      for (i in seq_along(lst_fields_values)) {
        f <- lst_fields_values[i]
        n <- names(f)
        if (is.null(n) || n == '') {
          warning('Element ', i, ' in list not named. Skipping.')
          next
        }
        if (n %in% names(private$.values)){
          old <- append(old, n)
        } else {
          new <- append(new,n)
        }
        #private$.values[[n]] <- f[[1]]
        private$.add(f)
      }
      private$.message('Fields updated: ', paste(old, collapse = ', '), '\nNew fields: ', paste(new, collapse = ', '))
      if ( (is.null(new) || length(new) != 0) && !'id_comp' %in% lst_fields_values)
        warning('Fields added, but "id_comp" not updated. This could lead to inconsistencies.')
      invisible(self)
    },
    remove_fields = function(field_names){
      if (!is.character(field_names)){
        stop('Need to provide a vector of character names')
      }
      for (n in field_names){
        if (n == 'id_comp' || n == 'id_value_set'){
          warning('Cannot remove id_comp nor id_value_set fields. Skipping')
          return(invisible(self))
        }
        private$.values[[n]] <- NULL
      }
      invisible(self)
    }
  )
)

`==.paramComp` <- function(a, b) {
 a$equal(b)
}

`!=.paramComp` <- function(a, b) {
 !a$equal(b)
}

#utils::globalVariables(names = c("self", "super"))


# @description
# @examples
# @param
# @return
