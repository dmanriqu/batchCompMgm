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
               time_init = Sys.time(), time_end = as.POSIXct(NA), 
               notes = notes, fileName = fileName,
               RobjectNames = RobjectNames, 
               depends = depends) 
     private$log[[id]] <- x
   },
   closeEntry = function(id){
     if (is.null(private$log[[id]])){
       stop("event doesn't exist")
     }
     private$log[[id]]$time_end  <-  Sys.time()
   },
   log_tabular_raw = function(){
     y <- lapply(private$log, 
            FUN = function(x){
              data.frame(
                         id =x$id,
                         name =x$name,
                         descr =x$descr,
                         time_init = x$time_init,
                         time_end  = x$time_end,
                         notes =x$notes,
                         fileName =x$fileName,
                         RobjectNames = paste(x$RobjectNames, collapse = ', '),
                         depends = paste(x$depends, collapse = ', '),
                         stringsAsFactors = FALSE
                         )
            }
     )
     do.call(rbind, y)
   },
   log_tabular = function(){
     y <- self$log_tabular_raw()
     y$time_init <- date2str(y$time_init)
     y$time_end <- date2str(y$time_end)
     y
   },
   getLogRaw = function(){private$log},
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
  logMerge = function(otherLogObject){
#common and different events.
    o <- self$log_tabular_raw()[, c('id', 'time_init', 'time_end') ]
    n <- otherLogObject$log_tabular_raw()[, c('id', 'time_init', 'time_end') ]
    common <- intersect(o$id, n$id)
    n_minus_o <- setdiff(n$id, o$id)
#Update events:
    o$time_end[is.na(o$time_end)] <- as.POSIXct('1789-07-01 01:00:00 UTC')
    n$time_end[is.na(n$time_end)] <- as.POSIXct('1789-07-01 01:00:00 UTC')

    harm <- pmax(n[common,'time_end'], o[common,'time_end' ])
    o[common,'time_end'] <- n[common,'time_end'] <- harm
    o[o$time_end < as.POSIXct('1789-12-12'),'time_end'] <- as.POSIXct(NA)
    n[n$time_end < as.POSIXct('1789-12-12'),'time_end'] <- as.POSIXct(NA)
# Now add the events in the other log that are missing in the present
    if (length(o_minus_n)> 0){
      o <- rbind(o, n[n_minus_o, ])
    }
    all <- o$id
# Now update the log data
    private$log <- append(private$log, otherLogObject$getLogRaw()[n_minus_o])
    for (i in all){
      private$log[[i]]$time_end <- o$time_end[o$id == i]
    }
  },
  test = function()private$log
  )
)
