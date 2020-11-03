# New Object for storing computations

CompMgm  <- R6::R6Class ( 
  classname = "CompMgm", inherit = base_mgmObj, 
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
        private$.obj_parameters = paramComp$new(persist_format = private$.serializer$format)
      }
      #DEBUG
      private$.obj_parameters$load_list_definition(def = param_list_def)
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
    # .write = function(file_name) {
    #   x  <- self$get_list_definition(str_dates = TRUE)
    #   jsonlite::write_json(
    #     x, path = file_name, pretty = TRUE,
    #     null = "null", na = "null", auto_unbox = TRUE
    #   )
    # },
    # .read = function(file_name) {
    #   x <- jsonlite::read_json(path = file_name)
    #   self$load_list_definition(x, str_dates = TRUE)
    #   invisible(self)
    # },
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
      concurrent = FALSE,
      overwrite_file = FALSE,
      auto_update = FALSE,
      persist_format = c('json', 'yaml')
    ) {
      super$initialize(persist_format = persist_format[1])
      fn <- !is.null(file_name)
      p <- !is.null(parameters)
      ow <- overwrite_file
      private$.file_name <- replace_markers(file_name, parameters$values)
      action <- NULL
      if (!fn){
        if (p) 
          action <- 'create'
        else
          stop('Need either parameters or a file to create object.')
      } else {
        fe <- file.exists(private$.file_name)
        if (fe){
          if (p && ow){
            action <- 'create'
          } else {
            action <- 'read'
          }
        } else {
          if (p){
            action <- 'create'
          } else {
            stop('Filename', private$.file_name, "doesn't exist. Need parameters to create object")
          }
        }
      }
      if(action == 'create'){
        private$.obj_parameters <- parameters$clone()
        private$.obj_log <- taskLog$new(concurrent = concurrent, persist_format = private$.serializer$format)
        private$.closed <- FALSE
        if (fn){
          l <- private$.acquire_file_lock(file_name = private$.file_name)
          e <- tryCatch(
            {
              self$save(private$.file_name)
              NULL
            },
            error = function(e) e,
            finally =  private$.release_file_lock(l)
          )
          if (!is.null(e)) stop(e)
        } else {
          warning('Filename not attached. Use "save_as to create one".')
        }
      } else if (action == 'read'){
        private$.obj_log <- taskLog$new(persist_format = private$.serializer$format)
        l <- private$.acquire_file_lock(file_name = private$.file_name)
        e <- tryCatch(
          {
            self$load(private$.file_name)
            NULL
          },
          error = function(e) e,
          finally =  { private$.release_file_lock(l) }
        )
        if(!is.null(e)) stop(e)
        message('Data read from ', private$.file_name)
      } else {
        stop("uh oh... there's a bug")
      }
      private$.auto_update = auto_update
      invisible(self)
    },
    print = function() {
      cat("------------------------------\n")
      cat("Parameters:\n")
      private$.obj_parameters$print()
      cat("Scheduling mode:", ifelse(private$.obj_log$concurrent, "Concurrent", "Sequential"),"\n")
      cat("Attached file:", private$.file_name, '\n')
      cat("------------------------------\n")
      cat("log:\n")
      private$.obj_log$print()
    },
    # task control wrappers
    create_task = function(
      id = NULL, description = NULL, comments = NULL, filenames = NULL, params = NULL,
      objects = NULL, requisites = NULL
    ) {
      if (private$.closed) {
        stop("Cannot add more tasks after finishing")
      }
      v <- private$.validate_id(id)
      if (v != 'OK') stop (v)
      private$.obj_log$create_task(
        id = id, description = description, 
        requisites = requisites, params = params, comments = comments, filenames = filenames, objects = objects)  
      invisible(self)
    },
    finish_task = function(id = NULL) {
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      private$.obj_log$finish_task(id)
      invisible(self)
    },
    get_log_object = function() {
      return(private$.obj_log)
    },
    start_task = function(id) {
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
      self$update()
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
    close = function() {
      # check that the tasks are completed
      if (!self$are_all_task_finished()){
        stop('Cannot close. Tasks', paste(.private$.obj_log$get_all_unfinished_tasks(), collapse = ', '), 'are incomplete.')
      } 
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
    # getJSON = function() {
    #   x  <- self$get_list_definition(str_dates = TRUE)
    #   jsonlite::toJSON(
    #     x, pretty = TRUE, null = "null",
    #     na = "null", auto_unbox = TRUE
    #   )
    # },
    # loadJSON = function(string) {
    #   x <- jsonlite::fromJSON(txt = string)
    #   self$load_list_definition(x, str_dates = TRUE)
    # },
    save_as = function(file_name, overwrite_file = FALSE) {
      if (!overwrite_file && file.exists(file_name)) {
        stop("file '", file_name, "' already exists ", "use 'overwrite_file = TRUE' to overwrite")
      }
      l <- private$.acquire_file_lock(file_name)
      e <- tryCatch(
        {
          self$save(file_name)
          NULL
        },
        error = function(e)e,
        finally =  private$.release_file_lock(l)
      )
      if (!is.null(e)) stop(e)
      if (!is.null(private$.file_name) && file_name != private$.file_name) {
        warning("Changed attached file from ", private$.file_name, " to ", file_name)
      } 
      private$.file_name <- file_name
    },
    update = function(overwrite = FALSE) {
      if (is.null(private$.file_name)) {
        warning("Nothing done because no attached file set. Use save_as() for setting one")
        return()
      }
      if (overwrite){
        self$save_as(self$filename, overwrite_file = TRUE)
        invisible(self)
      }
      l <- private$.obj_log$get_list_definition()
      o <- taskLog$new(persist_format = private$.serializer$format)
      o$load_list_definition(l)
      flock <- private$.acquire_file_lock(private$.file_name)
      e <- tryCatch(
        {
          self$load(private$.file_name)
          private$.obj_log$log_merge(o)
          self$save(private$.file_name)
          NULL
        },
        error = function(e){e},
        finally = { private$.release_file_lock(flock) }
      )
      if (!is.null(e)) stop(e)
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
    generate_tasks_from_trials = function(requisites = NULL){
      if (!self$is_batch_of_trials())
        stop('Parameters are not a batch definition')
      l <- private$.obj_parameters$get_params_for_trials()
      for (p in l){
        self$create_task(
          id = as.character(p$values$trial),
          comments = paste('Created automatically from trial', p$values$id),
          params = p,
          requisites = requisites
        )
      }
    },
    get_list_definition = function() {
      list(
        class      = class(self),
        parameters = private$.obj_parameters$get_list_definition(),
        closed     = private$.closed,
        log        = private$.obj_log$get_list_definition()
      )
    },
    load_list_definition = function(def) {
      if (!("CompMgm" %in% def$class)) {
        stop("Wrong 'class' when reading definition of 'CompMgm' object attribute")
      } 
      private$.read_parameters(def$parameters)
      private$.obj_log$load_list_definition(def$log)
      private$.closed <- def$closed
      invisible(self)
    }
  )
)

