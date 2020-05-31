#TODO: add methods for determining status of tasks
#TODO: add methods for comparison with external paramComp objects

#Object for storing computaitons

batchComp  <- R6::R6Class ( classname = "batchComp",
  private = list(
    .obj_log =  taskLog$new(),
    .obj_parameters = NULL,
    .closed = FALSE,
    .concurrent = FALSE,
    .loaded = FALSE,
    .file_name = NULL,
    .get_list_definition = function(str_dates = TRUE) {
      list(
         class      = "batchComp",
         log        = private$.obj_log$get_list_definition(str_dates = TRUE),
         parameters = private$.obj_parameters$get_list_definition(str_dates = TRUE),
         closed     = private$.closed,
         concurrent = private$.concurrent
      )
    },
    .load_list_definition = function(def, str_dates = TRUE) {
      if (def$class != "batchComp") {
        stop("Wrong 'class' attribute")
      }
      private$.obj_parameters$load_list_definition(def$parameters, str_dates = TRUE)
      private$.obj_log$load_list_definition(def$log)
      private$.closed <- def$closed
      private$.concurrent <- def$concurrent
      invisible(self)
    }
  ),
  active = list(
    params = function() {
      private$.obj_parameters
    },
    log = function() {
      private$.obj_log
    },
    filename = function() {
      private$.file_name
    }
  ),
  public = list(
    initialize  = function(
      parameters = NULL,
      file_name = ifelse(!is.null(parameters),
                         "<id>_batch.json", NULL),
      concurrent = FALSE
    ) {
      if (!is.null(parameters)) {
        if (!("paramComp" %in% class(parameters))) {
            stop('Argument "parameters" needs to be of class paramComp.')
        }
        private$.obj_parameters <- parameters$clone()
        private$.closed <- FALSE
        private$.concurrent <- concurrent
        private$.file_name <- replace_markers(file_name, parameters$values)
      } else if (!is.null(file_name)) {
        i <- grep(pattern = "<[^\\]]+>", x = file_name, perl = TRUE)
        if (length(i) != 0) {
          stop("Cannot use patterned file name if not providing parameters")
        }
        private$.obj_parameters <- paramComp$new()
        private$.file_name <- file_name
        self$read()
      } else {
        stop("No construction parameters.")
      }
      invisible(self)
    },
    print = function() {
      cat("------------------------------\n")
      cat("Parameters:\n")
      private$.obj_parameters$print()
      cat("Scheduling mode:", ifelse(private$.concurrent, "Concurrent", "Sequential"),"\n")
      cat("------------------------------\n")
      cat("log:\n")
      print(private$.obj_log$log_tabular(str_dates = TRUE), row.names = FALSE)
    },
    finish = function() {
      # check that the tasks are completed
      private$.closed  <- TRUE
      invisible(self)
    },
    create_task = function(id="", name = "", descr = "", 
                       notes = "", file_name = "", 
                       Robject_names = list(), depends = list()) {
      if (private$.closed) {
        stop("Cannot add more tasks after finishing")
      }
      private$.obj_log$create_task(id=id, name = name, descr = descr, notes = notes, file_name = file_name, Robject_names = Robject_names, depends = depends)
      invisible(self)
    },
    complete_task = function(id = NULL) {
      #TODO: Fix this.
      if (!private$.concurrent && is.null(id)) {
        i <- length(private$.obj_log)
        if (i == 0) {
          warning("Task doesn't exist. Ignoring command.")
          invisible(self)
        }
        id <- names(private$.obj_log$log)[i]
      } else if (is.null(id)) {
        stop("Mode is concurrent. Need to specify a task Id")
      }
      private$.obj_log$finish_task(id)
      invisible(self)
    },
    getJSON = function() {
      x  <- private$.get_list_definition(str_dates = TRUE)
      jsonlite::toJSON(
        x, pretty = TRUE, null = "null",
        na = "null", auto_unbox = TRUE
      )
    },
    loadJSON = function(string) {
      x <- jsonlite::fromJSON(txt = string)
      private$.load_list_definition(x, str_dates = TRUE)
    },
    write = function(file) {
      #acquire exclusive access to the file
      fnlck <- paste0(private$.file_name, ".lock")
      for (i in 1:5) {
        flock <- filelock::lock(
          path = fnlck, exclusive = TRUE, timeout = 1000 + runif(n = 1, 0, 1000)
        )
        if (!is.null(flock)) {
          break
        } else {
          if (i == 5) stop("Could not accquire file lock")
        }
      }
      x  <- private$.get_list_definition(str_dates = TRUE)
      jsonlite::write_json(
        x, path = self$filename, pretty = TRUE,
        null = "null", na = "null", auto_unbox = TRUE
      )
      filelock::unlock(lock = flock)
    },
    read = function() {
      #acquire exclusive access to the file
      fnlck <- paste0(private$.file_name, ".lock")
      for (i in 1:5) {
        flock <- filelock::lock(
          path = fnlck, exclusive = TRUE, timeout = 1000 + runif(n = 1, 0, 1000)
        )
        if (!is.null(flock)) {
          break
        } else {
          if (i == 5) stop("Could not accquire file lock")
        }
      }
      tryCatch({
          x <- jsonlite::read_json(path = self$filename)
        },
        finally = function() {
          filelock::unlock(flock)
        }
      )
      filelock::unlock(lock = flock)
      private$.load_list_definition(x, str_dates = TRUE)
      invisible(self)
    },
    get_log_object = function() {
      return(private$.obj_log)
    },
    same_parameters = function(parameters) {
      if (is.null(private$.obj_parameters)) {
        stop("Parameters not loaded")
      }
      parameters$equal(private$.obj_parameters)
    }
  )
)

