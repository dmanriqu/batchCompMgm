#TODO: add automatic filename and patterns.
#' R6 class for parameters of computation
#'
#' @examples
paramComput <- R6::R6Class(
  classname = "paramComput",
  private = list(
    .values = NULL,
    .date = NULL,
    .add = function(list_params) {
      if (!is.list(list_params)) {
        stop("Input must be a named list of parameters")
      } else if (sum(unique(names(list_params)) != "") != length(list_params)) {
        stop("Every parameter must have a different name")
      } else if (length(intersect(names(self$params), names(list_params))) > 0) {
        stop("Cannot have duplicate parameter names")
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
    }
  ),
  public = list(
    #' @description
    #' Change hair color.
    #' @param ... parameters to track.
    #' @examples
    #' x <- paramComput$new(a = 1, b = 2)
    initialize = function(file = NULL, strJSON  = NULL, parameter_list = NULL, eq_function = function(a, b) {all.equal(a, b)}) {
      if (!is.null(file)) {
        self$loadJSON_def(file)
        return(self)
      } else if (!is.null(strJSON)) {
        self$loadJSON_def(strJSON)
        return(self)
      } else if (!is.null(parameter_list)) {
        private$.add(parameter_list)
      } else {
        stop("No initialization parameters provided. Aborting.")
      }
      if (names(parameter_list)[1] != "id") {
        stop("First element of the parameter list must be 'id'")
      }
      private$.date <- Sys.time()
      private$.eq_function <- eq_function
    },
    equal = function(obj) {
      if (private$.values$id != obj$values$id) {
        warning("Comparing objects with different ids.")
        return(FALSE)
      }
      private$.eq_function(self$values, obj$values)
    },
    print = function() {
      cat(paste(names(self$values), self$values, sep = " = "),  sep = "\n")
      cat("Date:", as.character(self$date), "\n")
    },
    get_list_definition = function(str_dates = TRUE) {
      list(class = "paramComput", values = private$.values,
           date = ifelse(str_dates, date2str(private$.date), private$.date),
           eq_function = private$.func2str(private$.eq_function)
      )
    },
    load_list_definition = function(def, str_dates = TRUE) {
      if (def$class != "paramComput") {
        stop("Wrong 'class' attribute")
      }
      private$.values <- def$values
      private$.date <- ifelse(str_dates, date2str(def$date), def$date)
      private$.eq_function <- private$.str2func(def$eq_function)
    },
    writeJSON_def = function(file = NULL) {
      r <- self$get_list_definition(str_dates = TRUE)
      if (is.null(file)) {
        jsonlite::toJSON(x = r, pretty = TRUE, null = "null", na = "null", auto_unbox = TRUE)
      } else {
        jsonlite::write_json(x = r, path = file, pretty = TRUE, auto_unbox = TRUE)
      }
    },
    loadJSON_def = function(strJSON = NULL, file = NULL) {
      if (!is.null(str)) {
        l <- jsonlite::fromJSON(strJSON, simplifyVector = TRUE)
      } else {
        l <- jsonlite::read_json(path = file, simplifyVector = TRUE)
      }
      if (l$class != "paramComput")
        stop("Wrong 'class' attribute")
      private$.values <- l$values
      private$.date <- str2date(l$date)
      private$.eq_function <- private$.str2func(l$eq_function)
    }
  )
)

`==.paramComput` <- function(a, b) {
 a$equal(b)
}

`!=.paramComput` <- function(a, b) {
 !a$equal(b)
}

utils::globalVariables(names = c("self", "super"))


# @description
# @examples
# @param
# @return 

# For evaluating a command given as a string in R use eval(parse(text=string))
# For converting an R object to string use: paste(deparse(obj), collapse = "\n")
