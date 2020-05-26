#Object for storing computaitons

batchComp  <- R6::R6Class(
  classname = 'batchComp',
  lock_objects = FALSE,
  private   = list(
    Id = NULL,
    Plog  = list(), 
    Pclosed = FALSE,
    Pparameters   = NULL,
    Pconcurrent = FALSE,
    Ploaded = FALSE,
    PcreateTask = 
      function(id, name, descr = NULL, time_start = NULL, 
               time_end = NULL, notes = NULL, fileName = NULL, 
               depends = list())
      {
        list(id = id, 
             name =name,
             descr = descr, 
             time_start= time_start,
             time_end = time_end,
             notes = notes,
             fileName = fileName, 
             depends = depends
        )
      }
  ), 
  active = list(
    closed      = function(){private$Pclosed},
    log         = function(){private$Plog},
    parameters       = function(){private$Pparameters},
    nTasks     = function(){length(Plog)},
    latestTask = function(){
      private$Plog[[length(plog)]]$id
    }
  ),
  public = list(
    initialize  = function(parameters, concurrent = FALSE){
      if(!('paramComput' %in% class(parameters))){
        stop('Need object "paramComput" for initialization')
      }
      private$Pparameters  <- parameters
      private$Pid  <- parameters$id
      private$Pclosed <- FALSE
      private$Pconcurrent<- concurrent
    },
    print       = function(){
      cat('Results for computation','\n')
      print(private$Pparameters)
      cat('Closed:', private$Pclosed, '\n')
      cat('Task log:\n')
      #TODO: format the log
      print(private$Plog)
    },
    save = function(file){
    },
    load = function(file){
    },
    finish = function(){
      private$closed  <- TRUE
    },
    addTask = function(id = NULL , name, descr = NULL, notes = NULL, fileName = NULL){
      if(private$Pclosed){
        stop('Cannot add more tasks after finishing')
      }
      if (is.null(id)) {
        id <- paste0('TSK', length(private$Plog)+1)
      }
      while (id %in% names(private$Plog)){
        id <-paste0(id, 'x')
        warning("Duplicated task Id. Appending 'x'")
      }
      # function(id, name, descr = NULL, time_start = NULL, 
      #          time_end = NULL, notes = NULL, fileName = NULL, 
      #          depends = list())
      x <- private$PcreateTask(id, name, descr, time_start = date(),time_end = NULL, notes = notes, 
        fileName = fileName)
      private$Plog[[id]] <- x
    },
    completeTask = function(id = NULL){
      if (!private$Pconcurrent){
        i  <- length(private$Plog)
      } else if (is.null(id)){
        stop('Mode is concurrent. Need to specify a task Id.')
      } else if (!is.null(private$Plog[[id]]$time_end)) {
        warning('Task was already completed. Ignoring command')
      } else if(i == 0) {
        warning('No tasks. Ignoring command')
      } else {
        private$Plog$time_end = date()
      }
    },
    export = function(file = NULL){
      x  <-  list(
         Id = private$Id,
         parameters = private$Pparameters$parameters,   
         closed = private$Pclosed,
         concurrent = private$Pconcurrent,
         log  = private$Plog
      )
      jsonlite::toJSON(x, pretty = TRUE)
    },
    import = function(file = NULL, string = NULL){
      # if file is not NULL read it and set "string"
      x <- jsonlite::fromJSON(txt = string)
      private$Id = x$Id
      private$Pparameters$parameters = x$parameters   
      private$Pclosed = x$closed
      private$Pconcurrent = x$concurrent
      private$Plog = x$log
    },
  )
)

