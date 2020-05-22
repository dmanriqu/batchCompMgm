#Object for storing computaitons

batchComp  <- R6::R6Class(
  classname = 'batchComp',
  lock_objects = FALSE,
  private   = list(
    Plog  = data.frame(id = character(0), descr = character(0), notes = character(0), timestamp = character(0)),
    Pstatus = factor('INIT', levels = c('INIT', 'DONE')),
    Pinput   = NULL
  ), 
  active = list(
    status = function(){private$Pstatus},
      log = function(){private$Plog},
    input  = function(){private$Pinput},
    nEvents = function(){NROW(Plog)},
    latestEvent = function(){

    }
  ),
  public = list(
    initialize  = function(input_values){
      if(!('paramComput' %in% class(input_values))){
        stop('Need object "paramComput" for initialization')
      }
      private$Pinput  <- input_values
      private$Pstatus[1] <- 'INIT'
    },
    results     = list(),
    print       = function(){
      cat('Results for computation','\n')
      print(private$Pinput)
      cat('Status:', as.character(private$Pstatus), '\n')
      cat('Event log:\n')
      print(private$Plog)
    },
    save = function(file){
    },
    load = function(file){
    },
    finish = function(){
      private$Pstatus[1] = 'DONE'
    },
    addEvent = function(descr, id = NULL ){
      if(private$Pstatus == 'FINISH'){
        stop('Cannot add more events after finishing')
      }
      if (is.null(id)) {
        id <- paste0('EV', NROW(private$Plog)+1)
      }
      while (id %in% private$Plog$id){
        id <-paste0(id, 'x')
        warning("Duplicated event Id. Appending 'x'")
      }
      private$Plog <- rbind(private$Plog, data.frame(id = id, descr = descr, timestamp = date()))
    }
  )
)
