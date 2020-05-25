#Object for storing computaitons

batchComp  <- R6::R6Class(
  classname = 'batchComp',
  lock_objects = FALSE,
  private = list (
    parameters = NULL,
    log = NULL, 
    closed = FALSE,
    concurrent = FALSE,
    loaded = FALSE,
    fileName = NULL
  ), 
  active = list (),
  public = list (
    initialize  = function(parameters, concurrent = FALSE) {
      if(!('paramComput' %in% class(parameters))) {
        stop('Need object "paramComput" for initialization')
      }
      #this is for when reading from file
      if (is.null(private$log)) {
        private$log <- taskLog$new()
      }
      private$parameters  <- parameters
      private$closed <- FALSE
      private$concurrent<- concurrent
    },
    print = function() {
      private$parameters$print()
      cat('Computation log:\n------------------------------\n')
      print(private$log$log_tabular(), row.names = FALSE)
    },
    save = function(file) {
    },
    load = function(file) {
    },
    finish = function() {
      # check that the tasks are completed
      private$closed  <- TRUE
    },
    addTask = function(id=NULL, name = NULL, descr = NULL, 
                       notes = NULL, fileName = NULL, 
                       RobjectNames = list(), depends = list()) {
      if (private$closed) {
        stop("Cannot add more tasks after finishing")
      }
      #TODO: invoke the task from the object
      private$log$addEntry(id=id, name = name, descr = descr, notes = notes, fileName = NULL, RobjectNames = RobjectNames, depends = depends)
    },
    completeTask = function(id = NULL){
      if (!private$concurrent){
        id  <- length(private$log$log)
      } else if (is.null(id)){
        stop('Mode is concurrent. Need to specify a task Id')
      }  else if(id == 0) {
        warning('No tasks. Ignoring command')
        return()
      } 
      private$log$closeEntry(id)
    },
    export = function(file = NULL){
      x <- list(
         parameters = private$parameters$getDefinition(),   
         closed = private$closed,
         concurrent = private$Pconcurrent,
         log  = private$log$getLog()
      )
      jsonlite::toJSON(x, pretty = TRUE)
    },
    import = function(file = NULL, string = NULL){
      # if file is not NULL read it and set "string"
      x <- jsonlite::fromJSON(txt = string)
      private$parameters$loadDefinition(x$parameters)
      private$closed = x$closed
      private$concurrent = x$concurrent
      private$log$setLog(x$log)
    }
  )
)

