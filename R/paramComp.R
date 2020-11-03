#' R6 class for parameters of computation
#'
#' @examples
paramComp <- R6::R6Class(
  classname = "paramComp",
  inherit = base_mgmObj,
  lock_objects = FALSE,
  private = list(
    .values = NULL,
    .date = as.POSIXct(NA),
    .add = function(list_params) {
      if (!is.list(list_params)) {
        stop("Input must be a named list of parameters")
      } else if (sum(unique(names(list_params)) != "") != length(list_params)) {
        stop("Every parameter must have a different name")
      } else if (length(intersect(names(self$params), names(list_params))) > 0) {
        stop("Cannot have duplicate parameter names")
      } else if (names(list_params)[1] != "id") {
        stop("First element of the parameter list must be 'id'")
      }
      private$.values <- append(private$.values, list_params)
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
        self$loadJSON_def(load_from_file)
      } else if (!is.null(parameter_list)) {
        private$.add(parameter_list)
      } else {
        private$.eq_function <- eq_function
        private$.date <- Sys.time()
      }
      invisible(self)
    },
    equal = function(obj) {
      if (!self$is_loaded) stop("Data not loaded in object paramComp")
      if (private$.values$id != obj$values$id) {
        warning("Comparing objects with different ids.")
        return(FALSE)
      }
      private$.eq_function(self$values, obj$values)
    },
    print = function() {
      cat('Object class: [', class(self), ']\n')
      cat(paste(names(self$values), self$values, sep = " = "),  sep = "\n")
      cat("Date:", as.character(self$date), "\n")
    },
    get_list_definition = function(str_dates = TRUE) {
      if (!self$is_loaded) warning("Data not loaded in object paramComp")
      list(class = class(self),
           values = private$.values,
           date = (if (str_dates) date2str(private$.date) else  private$.date),
           eq_function = private$.func2str(private$.eq_function)
      )
    },
    load_list_definition = function(def = NULL, str_dates = TRUE) {
      if (!("paramComp" %in% def$class)) {
        stop("Wrong 'class' attribute")
      }
      private$.values <- def$values
      private$.date <- (if (str_dates) str2date(def$date) else def$date)
      private$.eq_function <- private$.str2func(def$eq_function)
      invisible(self)
    },
    writeJSON_def = function(file_name_pattern = NULL) {
      if (!self$is_loaded) {
        warning("Data not loaded in object paramComp")
        return()
      }
      r <- self$get_list_definition(str_dates = TRUE)
      if (is.null(file_name_pattern) || file_name_pattern == "") {
        jsonlite::toJSON(
          x = r, pretty = TRUE, null = "null",
          na = "null", auto_unbox = TRUE
        )
      } else {
        file_name <- replace_markers(file_name_pattern, data = private$.values)
        jsonlite::write_json(
          x = r, path = file_name,
          pretty = TRUE,
          auto_unbox = TRUE
        )
      }
    },
    loadJSON_def = function(strJSON = NULL, file_name = NULL) {
      if (!is.null(strJSON)) {
        l <- jsonlite::fromJSON(strJSON)
      } else {
        l <- jsonlite::read_json(path = file_name)
      }
      if (!("paramComp" %in% l$class)) stop("Wrong 'class' attribute")
      self$load_list_definition(l, str_dates = TRUE)
    },
    string_from_fields= function(pattern = "<id>_param.json") {
      replace_markers(pattern, data = private$.values)
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
