# New Object for storing computations

CompMgm  <- R6::R6Class ( 
  classname = "CompMgm", inherit = base_mgmObj, 
  private = list(
    .obj_log = NULL, 
    .obj_parameters = NULL,
    .auto_update = NULL,
    .closed = NULL,
    .loaded = NULL,
    .file_name = NULL,
    .file_lock_name = function(file_name){
      d <- dirname(file_name)
      b <- basename(file_name)
      fnlck <-  paste0(d,'/~',b , '.lock')
      return(fnlck)
    },
    .name_param = function(p){
      paste(p$values$id_comp, p$values$id_value_set, sep = ':')
    },
    .add_1_param = function(input){
      o <- mgmObjFactory$new()
      if (o$is_list_def(input)){
        p <- o$list_def_2_obj(input)
      } else if (is(input, 'paramComp')){
        p <- input$clone()
      } else {
        stop('Wrong input. Must be a list definition or a "paramComp" object.')
      }
      n <- private$.name_param(p)
      private$.obj_parameters[[n]] <- p
    },
    .load_list_parameters = function(list_input){
      if(!is.list(list_input)) stop ('Input must be a list.')
      for (p in list_input){
        private$.add_1_param(p)
      }
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
          if (i == retries) stop("Could not acquire file lock")
        }
      }
      return(flock)
    },
    .release_file_lock = function(flock){
      filelock::unlock(lock = flock)
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
    #General Class Stuff -----
    initialize  = function(
      parameters = NULL,
      file_name = NULL, 
      concurrent = FALSE,
      overwrite_file = FALSE,
      auto_update = FALSE,
      persist_format = c('json', 'yaml')
    ) {
      super$initialize(persist_format = persist_format[1])
      private$.auto_update = FALSE
      private$.closed = FALSE
      private$.loaded = FALSE
      private$.obj_parameters <- list()
      fn <- !is.null(file_name)
      p <- !is.null(parameters)
      ow <- overwrite_file
      private$.file_name <- private$.replace_markers(file_name, parameters$values)
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
        private$.obj_log <- taskLog$new(concurrent = concurrent, persist_format = self$persist_format)
        if (p){
          private$.add_1_param(parameters)
        }
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
        private$.obj_log <- taskLog$new(persist_format = self$persist_format)
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
    get_log_object = function() {
      return(private$.obj_log)
    },
    get_list_definition = function() {
      list(
        class      = class(self),
        parameters = lapply(private$.obj_parameters, FUN = function(x)x$get_list_definition()),
        closed     = private$.closed,
        log        = private$.obj_log$get_list_definition()
      )
    },
    load_list_definition = function(def) {
      f <- mgmObjFactory$new()
      if (!f$is_list_def(def)) stop ('Wrong input. Not a list definition.')
      if (!("CompMgm" %in% def$class)) {
        stop("Wrong 'class' when reading definition of 'CompMgm' object attribute")
      } 
      private$.load_list_parameters(def$parameters)
      private$.obj_log$load_list_definition(def$log)
      private$.closed <- def$closed
      invisible(self)
    },
    # Task log control wrappers ----
    # 1) Task actions ----
    get_task = function(id){
      private$.obj_log$get_task(id)
    },
    get_task_parameters = function(task_id){
      private$.obj_log$get_task(task_id)$get_parameters()
    },
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
        requisites = requisites, params = params, 
        comments = comments, filenames = filenames, 
        objects = objects)  
      invisible(self)
    },
    finish_task = function(id = NULL) {
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      private$.obj_log$finish_task(id)
      invisible(self)
    },
    start_task = function(id) {
      private$.obj_log$start_task(id)
      invisible(self)
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
          stop("Timeout reached while waiting for requisites of task '", id, "' to clear")
        }
        Sys.sleep(poll_interval)
        self$update()
      }
      private$.obj_log$start_task(id)
      invisible(self)
    },
    task_unfinish = function(id, I_AM_SURE = FALSE, unstart_dependent = TRUE){
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      private$.obj_log$task_unfinish(id, I_AM_SURE)
      if (unstart_dependent){
        dep <- private$.obj_log$get_all_dependent_tasks(id)
        for (d in dep){
          private$.obj_log$task_unfinish(d, I_AM_SURE = I_AM_SURE)
          warning('Dependet task ', d, ' unstarted.')
        }
      }
      invisible(self)
    },
    task_unstart = function(id, I_AM_SURE = FALSE, unstart_dependent = TRUE){
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      private$.obj_log$task_unstart(id, I_AM_SURE)
      if (unstart_dependent){
        dep <- private$.obj_log$get_all_dependent_tasks(id)
        for (d in dep){
          private$.obj_log$task_unfinish(d, I_AM_SURE = I_AM_SURE)
          warning('Dependet task ', d, ' unstarted.')
        }
      }
      invisible(self)
    },
    # 2) Task status ----
    #  2.1) Task in isolation
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
    #  2.2) Task in context
    is_task_cleared = function(id) {
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      private$.obj_log$is_task_cleared(id)
    },
    get_all_unfinished_tasks = function(id){
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      private$.obj_log$get_all_unfinished_tasks(id)
    },
    get_all_finished_tasks = function(id){
      v <- private$.validate_id(id)
      if (v != 'OK') stop(v)
      private$.obj_log$get_all_finished_tasks(id)
    },
    
    # Parameter object control ----
    get_params = function(elem) {
      return(private$.obj_parameters[[elem]])
    },
    load_parameter_obj = function(paramComp_obj){
      if (!is(paramComp_obj, 'paramComp')) {
        stop('Need a paramComp object.')
      }
      private$.add_1_param(paramComp_obj)
    },
    generate_params_for_trials = function(elem = 1){
      if (!is(private$.obj_parameters[[elem]], class2  = 'paramBatchComp'))
        stop('Parameters are not a batch definition')
      private$.obj_parameters[[elem]]$get_params_for_trials() 
    },
    generate_tasks_from_params = function(elem = 1, requisites = NULL){
      if (is(object = private$.obj_parameters[[elem]],  class2 = 'paramBatchComp')){
        l <- self$generate_params_for_trials(elem = elem)
        for (p in l) {
          self$create_task (
            id = as.character(p$values$trial),
            comments = paste('Autogenerated from', private$.name_param(p)),
            params = p,
            requisites = requisites
          )
        }
      } else if (is(private$.obj_parameters[[elem]], class2  = 'paramComp')){
        p <- private$.obj_parameters[[elem]]
        self$create_task(
          id = private$.name_param(p),
          comments = paste('Generated from', private$.name_param(p)),
          params = p,
          requisites = requisites
        )
      }
    },
    # Class actions ----
    close = function() {
      # check that the tasks are completed
      if (!self$are_all_task_finished()){
        stop('Cannot close. Tasks', paste(.private$.obj_log$get_all_unfinished_tasks(), collapse = ', '), 'are incomplete.')
      } 
      private$.closed  <- TRUE
      invisible(self)
    },
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
        return(invisible())
      }
      if (overwrite){
        self$save_as(self$filename, overwrite_file = TRUE)
        return(invisible(self))
      }
      l <- private$.obj_log$get_list_definition()
      o <- taskLog$new(persist_format = self$persist_format)
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
    set_concurrent_on = function(){
      private$.obj_log$change_scheduling_mode(concurrent = TRUE)
    },
    set_concurrent_off = function(){
      private$.obj_log$change_scheduling_mode(concurrent = FALSE)
    },
    print = function() {
      for (i in seq_along(private$.obj_parameters)) {
      cat("------------------------------\n")
      p <- private$.obj_parameters[i]
      cat('> Parameters: "', names(p), '"\n', sep = '')
        p[[1]]$print()
      }
      cat("------------------------------\n")
      cat("Scheduling mode:", ifelse(private$.obj_log$concurrent, "Concurrent", "Sequential"),"\n")
      cat("Attached file:", private$.file_name, '\n')
      cat("log:\n")
      private$.obj_log$print()
    }
  )
)

