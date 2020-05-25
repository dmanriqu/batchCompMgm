# tasklog object

taskLog <- R6::R6Class(
  classname = 'taskLog',
  private = list (log = list()),
  active = list(),
  public = list
  (
   initialize = function() {},
   addEntry = function(id=NULL, name = NULL, descr = NULL,
                        notes = NULL, fileName = NULL,
                        RobjectNames = c(), depends = c()) {
     if (is.null(id)) {
       id <- paste0("TSK", length(private$log) + 1)
     }
     while (id %in% names(private$log)) {
       id <-paste0(id, "x")
       warning("Duplicated task Id. Appending 'x'")
     }
     #TODO: add code to check dependencies
     x <- list(id = id, name = name, descr = descr, 
                               time_init = date(), time_end = NULL, 
                               notes = notes, fileName = fileName,
                               RobjectNames = RobjectNames, 
                               depends = depends) 
     private$log[[id]] <- lapply(x, FUN = function(y)ifelse(is.null(y), character(0), y))
   },
   closeEntry = function(id){
     if (!(id %in% names(private$log))){
       stop("event doesn't exist")
     }
     private$log[[id]]$time_end  <-  date()
   },
   log_tabular = function(){
     y <- lapply(private$log, 
            FUN = function(x){
              x <- lapply(x, FUN = function(y)ifelse(is.null(y), '-', y))
              data.frame(
                         id =x$id,
                         name =x$name,
                         descr =x$descr,
                         time_init =x$time_init,
                         time_end =x$time_end,
                         notes =x$notes,
                         fileName =x$fileName,
                         RobjectNames = paste(x$RobjectNames, collapse = ', '),
                         depends = paste(x$depends, collapse = ', '))
            }
     )
     do.call(rbind, y)
   },
   getLog = function(){ private$log },
   setLog= function(x){
     private$log <- x
   }
  )
     
)
