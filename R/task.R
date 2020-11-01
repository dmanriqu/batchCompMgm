#Task object
CTask <- R6::R6Class ( 
  classname = "CTask",
  private = list(
    .data = list(
      id = character(),
      description = NULL,
      params = NULL,
      time_init = NULL,
      time_start = NULL,
      time_end = NULL,
      comments = NULL,
      events = list(),
      filenames = NULL,
      objects = NULL,
      requisites = NULL
    )
    
  ),
  public = list(
    initialize = function(
      id,
      description = NULL,
      requisites =  NULL,
      params = NULL,
      comments = NULL,
      filenames = NULL,
      objects = NULL
    ){
      if (!is.character(id)) stop('Id must be of type character.')
      private$.data$id =id
      private$.data$description = description
      private$.data$params =params
      private$.data$comments =comments
      private$.data$filenames =filenames
      private$.data$objects =objects
      private$.data$requisites = requisites
      
      private$.data$time_init = Sys.time()
      private$.data$time_end= as.POSIXct(NA)
      private$.data$time_start = as.POSIXct(NA)
      self$register_event('Object created.')
      j <- self$toJSON()
      self$fromJSON(j)
    },
    flatten2df = function(){
      
    },
    get_requisites = function(){
      return(private$.data$requisites)
    },
    set_requisites = function(requisites){
      if(self$get_status() !=  'defined'){
        stop('Task "',private$.data$id, '": Cannot add requisites if the task is either started or finished')
      }
      private$.data$requisites  <- append(private$.data$requisites, requisites)
    },
    get_list_definition = function(){
      x <- list(
        id = as.character(private$.data$id),
        class = class(self),
        description = private$.data$description,
        time_init = date2str(private$.data$time_init),
        time_start = date2str(private$.data$time_start),
        time_end = date2str(private$.data$time_end),
        comments = private$.data$comments,
        events = private$.data$events,
        filenames = private$.data$filenames,
        objects = private$.data$objects,
        requisites = private$.data$requisites
      )
      if(!is.null(private$.data$params)) {
        x$params <- private$.data$params$get_list_definition()
      }
      return(x)
    },
    load_list_definition = function(x){
      if (!("CTask" %in% x$class)) stop ('Not a definition of CTask class. Aborting.')
        private$.data$id = x$id
        private$.data$description = x$description
        private$.data$time_init = str2date(x$time_init)
        private$.data$time_start = str2date(x$time_start)
        private$.data$time_end = str2date(x$time_end)
        private$.data$comments = x$comments
        private$.data$events = x$events
        private$.data$filenames = x$filenames
        private$.data$objects = x$objects
        private$.data$requisites = x$requisites
      if(!is.null(x$params)) {
        #it'd be better to allow to construct empty paramComp objects and then load the list...
        private$.data$params <-paramComp$new(strJSON = jsonlite::toJSON(x$params, auto_unbox = TRUE, na = 'null'))
      }
    },
    toJSON = function(){
      x <- self$get_list_definition()
      jsonlite::toJSON(
        x, pretty = TRUE, null = "null",
        na = "null", auto_unbox = TRUE
      )
    },
    fromJSON = function(json_str){
      x <- jsonlite::fromJSON(txt = json_str)
      self$load_list_definition(x)
    },
    finish = function(){
      if(!self$is_started()){
        private$.data$time_start <- private$.data$time_init
        warning('Task "', private$.data$id,  '": finishing before registering starting time". Registering date of creation as starting time.')
        self$register_event('Starting date copied from creation date.')
      }
      private$.data$time_end = Sys.time()
      self$register_event('Task finished.')
    },
    start = function(){
      self$register_event('Task started')
      private$.data$time_start = Sys.time()
    },
    unfinish = function(){
      self$register_event('Task unfinished')
      private$.data$time_end <- NA
    },
    unstart = function(){
      self$unfinish()
      self$register_event('Task unstarted')
      private$.data$time_start<- NA
    },
    is_finished = function(){
      !is.na(private$.data$time_end)
    },
    get_time_end = function(as_str = TRUE){
      if(as_str){
        return(date2str(private$.data$time_end))
      }
      return(private$.data$time_end)
      
    },
    is_started = function(){
      !is.na(private$.data$time_start)
    },
    get_time_start = function(as_str = TRUE){
      if(as_str){
        return(date2str(private$.data$time_start))
      }
      return(private$.data$time_start)
    },
    get_status = function(){
      if (self$is_finished()) return('finished')
      if (self$is_started()) return('started')
      return('defined')
    },
    get_time_status = function(){
      if (self$get_status() == 'defined') return(date2str(private$.data$time_init))
      if (self$get_status() == 'started') return(date2str(private$.data$time_start))
      if (self$get_status() == 'finished') return(date2str(private$.data$time_end))
    },
    get_time_completion_sec = function(){
      if (self$get_status() != 'finished') return(NA)
      return(private$.data$time_end - private$.data$time_start)
    },
    get_parameters = function(){
      return(private$.data$params)
    },
    register_event = function(message){
      t <- date2str(Sys.time())
      lab <- t; cc <- 1
      while (lab %in% names(private$.data$events)){
        lab <- paste0(t, '_', cc)
        cc <- cc + 1
      }
      private$.data$events[[lab]] <- as.character(message)
    },
    get_events = function(){
      private$.data$events
    },
    get_id = function(){
      private$.data$id
    },
    clear_events = function(){
      private$.data$events = list()
    },
    print = function(){
      cat('Task "', private$.data$id, '"\n')
      cat('created: ', date2str(private$.data$time_init), '\n', sep = '')
      cat('Status:', self$get_status(), '\n')
      if(self$is_started()) cat('Started at:', date2str(private$.data$time_start), '\n')
      if(self$is_finished()) cat('Finished at:', date2str(private$.data$time_end), '\n')
      if(!is.null(private$.data$comments)) cat('Comments:', private$.data$comments, '\n')
      if(!is.null(private$.data$requisites)) cat('Requires:', paste(private$.data$requisites, collapse = ', '), '\n')
      if(!is.null(private$.data$params)){
        cat('*Associated parameters:\n')  
        cat('---------------------\n')  
        private$.data$params$print()
      }
      if(length(private$.data$events) > 0){
        cat('*Registered Events:\n')
        cat('---------------------\n')  
        print(private$.data$events)
      }
    },
    get_oneline_summary = function(){
      s = paste0('[',self$get_id(),':', private$.data$comments, '] -> ', 
                 self$get_status(), ' at ', 
                 self$get_time_status(), '. Last event: ', 
                 private$.data$events[[length(private$.data$events)]], ' at ',
                 names(private$.data$events[length(private$.data$events)]))
      return(s)
    },
    peek = function(){
      return(private$.data)
    },
    update_with_another = function(another){
      if (self$get_id() != another$get_id()){
        stop('Not same task id. Aborting update (this: ', private$.data$id, ' | other: ', another$get_id(), ')')
      }
      if (!self$is_finished() && another$is_finished()) private$.data$time_end <- another$get_time_end(as_str = FALSE)
      if (!self$is_started() && another$is_started()) private$.data$time_start <- another$get_time_start(as_str = FALSE)
      get <- setdiff(names(another$get_events()), names(self$get_events()))
      for (i in seq_along(get)){
        private$.data$events[[get[i]]] <- another$get_events()[[get[i]]]
      }
      private$.data$events[ sort(names(private$.data$events))  ]
    }
  )
)
