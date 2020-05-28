# tasklog object

taskLog <- R6::R6Class(
  classname = 'taskLog',
  private = list (log = list()),
  active = list(),
  public = list
  (
   initialize = function() {},
   addEntry = function(id=NULL, name = '', descr = '',
                        notes = '', fileName = '',
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
               time_init = Sys.time(), time_end = NA, 
               notes = notes, fileName = fileName,
               RobjectNames = RobjectNames, 
               depends = depends) 
     private$log[[id]] <- x
   },
   closeEntry = function(id){
     if (!(id %in% names(private$log))){
       stop("event doesn't exist")
     }
     private$log[[id]]$time_end  <-  Sys.time()
   },
   log_tabular = function(){
     y <- lapply(private$log, 
            FUN = function(x){
              data.frame(
                         id =x$id,
                         name =x$name,
                         descr =x$descr,
                         time_init = date2str(x$time_init),
                         time_end  = date2str(x$time_end),
                         notes =x$notes,
                         fileName =x$fileName,
                         RobjectNames = paste(x$RobjectNames, collapse = ', '),
                         depends = paste(x$depends, collapse = ', '))
            }
     )
     do.call(rbind, y)
   },
   getLog = function(){ 
     # construct a version where the dates are in
     # string format
     x <- lapply(private$log, 
                FUN = function(y){
                  y$time_init  <- date2str(y$time_init)
                  y$time_end  <- date2str(y$time_end) 
                  return(y)
                }
     )
     return(x)
   },
   setLog= function(x){
     #convert string dates to POSIX
     x <- lapply(x, 
                FUN = function(y){
                  y$time_init  <- str2date(y$time_init)
                  y$time_end  <-  str2date(y$time_end) 
                  return(y)
                }
     )
     private$log <- x
   },
   test = function()private$log
  )
)
