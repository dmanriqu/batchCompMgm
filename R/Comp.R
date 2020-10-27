#Object for storing computations

Comp  <- R6::R6Class ( classname = "Comp",
  private = list(
    .obj_log = NULL, 
    .auto_update = FALSE,
    .obj_parameters = NULL,
    .closed = FALSE,
    .loaded = FALSE,
    .file_name = NULL,
    .read_parameters = function (param_list_def, str_dates = TRUE){
      if (!("paramComp" %in% param_list_def$class)){
        stop("Parameters must be of class paramComp")
      } else if ("paramBatchComp" %in% param_list_def$class) {
        private$.obj_parameters = paramBatchComp$new()
      } else {
        private$.obj_parameters = paramComp$new()
      }
      #DEBUG
      private$.obj_parameters$load_list_definition(def = param_list_def, str_dates = TRUE)
    },
    .get_list_definition = function(str_dates = TRUE) {
      list(
         class      = class(self),
         parameters = private$.obj_parameters$get_list_definition(str_dates = TRUE),
         closed     = private$.closed,
         log        = private$.obj_log$get_list_definition(str_dates = TRUE)
      )
    },
    .load_list_definition = function(def, str_dates = TRUE) {
      if (!("Comp" %in% def$class)) {
        stop("Wrong 'class' attribute")
      } 
      #private$.obj_parameters$load_list_definition(def$parameters, str_dates = TRUE)
      private$.read_parameters(def$parameters, str_dates = TRUE)
      private$.obj_log$load_list_definition(def$log)

      private$.closed <- def$closed
      invisible(self)
    },
    .file_lock_name = function(file_name){
      d <- dirname(file_name)
      b <- basename(file_name)
      fnlck <-  paste0(d,'/~',b , '.lock')
      return(fnlck)
    },
    .acquire_file_lock = function(file_name, timeout = 1000, retries = 5){
      fnlck <-  private$.file_lock_name(file_name)
      for (i in 1:retries) {
        flock <- filelock::lock(
          path = fnlck, exclusive = TRUE, timeout = timeout + runif(n = 1, 0, 1000)
        )
        if (!is.null(flock)) {
          break
        } else {
          if (i == retries) stop("Could not accquire file lock")
        }
      }
      return(flock)
    },
    .release_file_lock = function(flock){
      filelock::unlock(lock = flock)
    },
    .write = function(file_name) {
      x  <- private$.get_list_definition(str_dates = TRUE)
      jsonlite::write_json(
        x, path = file_name, pretty = TRUE,
        null = "null", na = "null", auto_unbox = TRUE
      )
    },
    .read = function(file_name) {
      x <- jsonlite::read_json(path = file_name)
      private$.load_list_definition(x, str_dates = TRUE)
      invisible(self)
    },
    .validate_id = function(id){
      #requirements for id.
      if(!is.character(id)) return('Task id must be of type character')
      return('OK')
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
      file_name = NULL, 
        # ifelse(!is.null(parameters), "<id>_batch.json", NULL),
      concurrent = FALSE,
      overwrite_file = FALSE,
      auto_update = FALSE
    ) {
      if (!is.null(parameters)) {
        # Case: construct new object from list of parameters.
        if (!("paramComp" %in% class(parameters))) {
          stop('Argument "parameters" needs to be of class paramComp.')
        } 
        private$.obj_parameters <- parameters$clone()
        private$.obj_log <- taskLog$new(concurrent = concurrent)
        private$.closed <- FALSE
        if (!is.null(file_name)) {
          private$.file_name <- replace_markers(file_name, parameters$values)
          if(!overwrite_file && file.exists(private$.file_name)) {
            stop("File '", private$.file_name, "' already exists. Use 'overwrite_file = TRUE' to overwrite")
          }
          l <- private$.acquire_file_lock(file_name = private$.file_name)
          private$.write(private$.file_name)
          private$.release_file_lock(l)
        } else {
          warning("No file attached. Use 'save_as' to set attached file")
        }
      } else if (!is.null(file_name)) {
        # no params. Load data from file
        i <- grep(pattern = "<[^\\]]+>", x = file_name, perl = TRUE)
        if (length(i) != 0) {
          stop("Cannot use patterned file name if not providing parameters")
        }
        #private$.obj_parameters <- paramComp$new()
        private$.obj_log <- taskLog$new()
        private$.file_name <- file_name
        l <- private$.acquire_file_lock(file_name = file_name)
        private$.read(file_name)
        private$.release_file_lock(l)
      } else {
        stop("No construction parameters. Cannot create object")
      }
      private$.auto_update = auto_update
      invisible(self)
    },
    print = function() {
      cat("------------------------------\n")
      cat("Parameters:\n")
      private$.obj_parameters$print()
      cat("Scheduling mode:", ifelse(private$.obj_log$concurrent, "Concurrent", "Sequential"),"\n")
      cat("------------------------------\n")
      cat("log:\n")
      print(private$.obj_log$log_tabular(str_dates = TRUE), row.names = FALSE)
    },
  # task control wrappers
    create_task = function(
      id = NULL, name = "", descr = "", notes = "", file_name = "",
      Robject_names = c(), depends = c(), task_prefix = "T", auto_start = TRUE
    ) {
      if (private$.closed) {
        stop("Cannot add more tasks after finishing")
      }
      v <- private$.validate_id(id)
      if (v != 'OK') stop (v)
      private$.obj_log$create_task(
        id = id, name = name, descr = descr, notes = notes,
        file_name = file_name, Robject_names = Robject_names,
        depends = depends, auto_start = auto_start 
      )
      invisible(self)
    },
    finish_task = function(id = NULL) {
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      if (!self$is_task_defined(id)) stop (paste('Task', id, 'not defined. Cannot finish it.'))
      if (!self$is_task_started(id)) stop (paste('Task', id, 'not started. Cannot finish it.'))
      private$.obj_log$finish_task(id)
      invisible(self)
    },
    get_log_object = function() {
      return(private$.obj_log)
    },
    start_task = function(id = NULL) {
      private$.obj_log$start_task(id)
      invisible(self)
    },
    is_task_defined = function(id) {
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      private$.obj_log$is_task_defined(id)
    },
    is_task_started = function(id) {
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      private$.obj_log$is_task_started(id)
    },
    is_task_finished = function(id) {
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      private$.obj_log$is_task_finished(id)
    },
    is_task_cleared = function(id) {
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      private$.obj_log$is_task_cleared(id)
    },
    is_batch_of_trials = function(){
     return('paramBatchComp' %in% class(private$.obj_parameters))
    },
    start_when_ready = function(id, poll_interval = 10, timeout = Inf) {
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      if (!self$is_task_defined(id)) stop(paste('Start task', id, 'not defined. Cannot start.'))
      if (!self$is_task_started(id)) warning(paste('Task', id, 'was already started.'))
      p1 <- Sys.time()
      while (!private$.obj_log$is_task_cleared(id)) {
        if (Sys.time() - p1 > timeout) {
          stop("Timeout reached while waiting for task '", id, "' to clear")
        }
        Sys.sleep(poll_interval)
        self$update()
      }
      private$.obj_log$start_task(id)
      invisible(self)
    },

  # Close for good
    finish = function() {
      # check that the tasks are completed
      private$.closed  <- TRUE
      invisible(self)
    },
 # Compare to another parameter object
    same_parameters = function(parameters) {
      if (is.null(private$.obj_parameters)) {
        stop("Parameters not loaded")
      }
      parameters$equal(private$.obj_parameters)
    },
    getJSON = function() {
      x  <- private$.get_list_definition(str_dates = TRUE)
      jsonlite::toJSON(
        x, pretty = TRUE, null = "null",
        na = "null", auto_unbox = TRUE
      )
    },
  # In/Out
    loadJSON = function(string) {
      x <- jsonlite::fromJSON(txt = string)
      private$.load_list_definition(x, str_dates = TRUE)
    },
    save_as = function(file_name, overwrite_file = FALSE) {
      if (!overwrite_file && file.exists(file_name)) {
        stop("file '", file_name, "' already exists ", "use 'overwrite_file = TRUE' to overwrite")
      }
      l <- private$.acquire_file_lock(file_name)
      private$.write(file_name)
      private$.release_file_lock(l)
      private$.file_name <- file_name
      if (!is.null(private$.file_name)) {
        warning("Changed attached file from ", private$.file_name, " to ", file_name)
      } 
    },
    update = function(overwrite = FALSE) {
      if (is.null(private$.file_name)) {
        stop("No attached file set. Use save_as() for setting one")
      }
      if (overwrite){
        self$save_as(self$filename, overwrite_file = TRUE)
        invisible(self)
      }
      l <- private$.obj_log$get_list_definition()
      o <- taskLog$new()
      o$load_list_definition(l)
      flock <- private$.acquire_file_lock(private$.file_name)
      tryCatch(
        {
          private$.read(private$.file_name)
          private$.obj_log$log_merge(o)
          private$.write(private$.file_name)
        }, 
        finally = function() {
          private$.release_file_lock(flock)
        }
      )
      private$.release_file_lock(flock)
      invisible(self)
    },
    clean_lockfile = function(){
      fn <- private$.file_lock_name(self$filename)
      if (file.exists(fn)){ file.remove(fn) }
    },
    set_auto_update_on = function(){
      private$.auto_update <- TRUE
      message('Auto-update on.')
    },
    set_auto_update_off = function(){
      private$.auto_update <- FALSE
      message('Auto-update off.')
    },
   task_unfinish = function(id, I_AM_SURE = FALSE){
     private$.obj_log$task_unfinish(id, I_AM_SURE)
     invisible(self)
   },
   task_unstart= function(id, I_AM_SURE = FALSE){
     private$.obj_log$task_unstart(id, I_AM_SURE)
     invisible(self)
   },
   set_concurrent_on = function(){
     private$.obj_log$change_scheduling_mode(concurrent = TRUE)
   },
   set_concurrent_off = function(){
     private$.obj_log$change_scheduling_mode(concurrent = FALSE)
   },
  # Functions for batch of trials. Requires parameter of class paramBatchComp
   generate_params_for_trials = function(){
     if (!self$is_batch_of_trials())
       stop('Parameters are not a batch definition')
     self$params$get_params_for_trials() 
   },
   generate_tasks_from_trials = function(){
     if (!self$is_batch_of_trials())
       stop('Parameters are not a batch definition')
     for (t in private$.obj_parameters$values$trials){
       self$create_task(
         id = as.character(t),
         notes = paste('Created automatically from trial', t),
         auto_start = FALSE
       )
     }
   }
  )
)
