#Object for storing computaitons

batchComp  <- R6::R6Class(
  classname = 'batchComp',
  lock_objects = FALSE,
  private = list (
    parameters = NULL,
    Plog = NULL, 
    closed = FALSE,
    concurrent = FALSE,
    loaded = FALSE,
    fileName = NULL
  ), 
  active = list (
    params = function(){private$parameters},
    log = function(){private$Plog}
  ),
  public = list (
    initialize  = function(parameters, concurrent = FALSE) {
      if(!('paramComput' %in% class(parameters))) {
        stop('Need object "paramComput" for initialization')
      }
      #this is for when reading from file
      if (is.null(private$Plog)) {
        private$Plog <- taskLog$new()
      }
      #Copy the object
      private$parameters <-paramComput$new(strJSON = parameters$writeJSONDef())
      private$closed <- FALSE
      private$concurrent<- concurrent
    },
    print = function() {
      private$parameters$print()
      cat('Computation log:\n------------------------------\n')
      print(private$Plog$log_tabular(), row.names = FALSE)
    },
    save = function(file) {
    },
    load = function(file) {
    },
    finish = function() {
      # check that the tasks are completed
      private$closed  <- TRUE
    },
    addTask = function(id="", name = "", descr = "", 
                       notes = "", fileName = "", 
                       RobjectNames = list(), depends = list()) {
      if (private$closed) {
        stop("Cannot add more tasks after finishing")
      }
      private$Plog$addEntry(id=id, name = name, descr = descr, notes = notes, fileName = fileName, RobjectNames = RobjectNames, depends = depends)
    },
    completeTask = function(id = NULL){
      if (!private$concurrent){
        id  <- length(private$Plog$log)
      } else if (is.null(id)){
        stop('Mode is concurrent. Need to specify a task Id')
      }  else if(id == 0) {
        warning('No tasks. Ignoring command')
        return()
      } 
      private$Plog$closeEntry(id)
    },
    export = function(file = NULL){
      x <- list(
         class = 'batchComp',
         parameters = private$parameters$getDefinition(),   
         closed = private$closed,
         concurrent = private$Pconcurrent,
         log  = private$Plog$getLog()
      )
      if (!is.null(file)) {
        x <- jsonlite::write_json(x, path = file, pretty = TRUE, null = 'null', na = 'null', auto_unbox = TRUE)
      } else {
        x <- jsonlite::toJSON(x, pretty = TRUE, null = 'null', na = 'null', auto_unbox = TRUE)
      }
      invisible(x)
    },
    import = function(file = NULL, string = NULL){
      #TODO: make sure that the data types match in x
      # if file is not NULL read it and set "string"
      if (!is.null(file)) {
        x <- jsonlite::read_json(path = file)
      } else {
        x <- jsonlite::fromJSON(txt = string)
      }
      if (x$class != 'batchComp') {
        stop('Wrong "class" attribute')
      }
      private$parameters$loadDefinition(x$parameters)
      private$closed = x$closed
      private$concurrent = x$concurrent
      private$Plog$setLog(x$log)
    }
  )
)

