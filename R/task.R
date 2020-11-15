#Task object
CTask <- R6::R6Class ( 
  classname = "CTask",
  inherit = base_mgmObj,
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
      persist_format = c('json', 'yaml')
    ){
      super$initialize(persist_format[1])
      if (!is.character(id)) stop('Id must be of type character.')
      private$.data$id =id
      private$.data$description = description
      private$.data$params =params
      private$.data$comments =comments
      private$.data$requisites = requisites
      
      private$.data$time_init = private$.get_time()
      private$.data$time_end= NULL
      private$.data$time_start = NULL
      self$register_event('Object created.')
      j <- self$save()
      self$load(string = j)
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
        time_init  = private$.serializer$date2str(private$.data$time_init),
        time_start = private$.serializer$date2str(private$.data$time_start),
        time_end   = private$.serializer$date2str(private$.data$time_end),
        comments = private$.data$comments,
        events = private$.data$events,
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
        private$.data$time_init  = private$.serializer$str2date(x$time_init)
        private$.data$time_start = private$.serializer$str2date(x$time_start)
        private$.data$time_end   = private$.serializer$str2date(x$time_end)
        private$.data$comments = x$comments
        private$.data$events = x$events
        private$.data$requisites = x$requisites
      if(!is.null(x$params)) {
        private$.data$params <-paramComp$new(persist_format = private$.serializer$format)
        private$.data$params$load_list_definition(x$params)
      }
    },
    finish = function(){
      if(!self$is_started()){
        private$.data$time_start <- private$.data$time_init
        warning('Task "', private$.data$id,  '": finishing before registering starting time". Registering date of creation as starting time.')
        self$register_event('Starting date copied from creation date.')
      }
      private$.data$time_end = private$.get_time()
      self$register_event('Task finished.')
    },
    start = function(){
      self$register_event('Task started')
      private$.data$time_start = private$.get_time()
    },
    unfinish = function(){
      self$register_event('Task unfinished')
      private$.data$time_end <- NULL
    },
    unstart = function(){
      self$unfinish()
      self$register_event('Task unstarted')
      private$.data$time_start<- NULL
    },
    is_finished = function(){
      !is.null(private$.data$time_end)
    },
    get_time_end = function(as_str = TRUE){
      if(as_str){
        return(private$.serializer$date2str(private$.data$time_end))
      }
      return(private$.data$time_end)
    },
    is_started = function(){
      !is.null(private$.data$time_start)
    },
    get_time_start = function(as_str = TRUE){
      if(as_str){
        return(private$.serializer$date2str(private$.data$time_start))
      }
      return(private$.data$time_start)
    },
    get_status = function(){
      if (self$is_finished()) return('finished')
      if (self$is_started()) return('started')
      return('defined')
    },
    get_time_status = function(){
      if (self$get_status() == 'defined')  return(private$.serializer$date2str(private$.data$time_init))
      if (self$get_status() == 'started')  return(private$.serializer$date2str(private$.data$time_start))
      if (self$get_status() == 'finished') return(private$.serializer$date2str(private$.data$time_end))
    },
    get_time_completion_sec = function(){
      if (self$get_status() != 'finished') return(NA)
      return(private$.data$time_end - private$.data$time_start)
    },
    get_parameters = function(){
      return(private$.data$params)
    },
    register_event = function(message){
      t <- private$.serializer$date2str(private$.get_time())
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
      cat('Task "', private$.data$id, '"\n', sep = '')
      cat('created: ', private$.serializer$date2str(private$.data$time_init), '\n', sep = '')
      cat('Status:', self$get_status(), '\n')
      if(self$is_started()) cat('Started at:', private$.serializer$date2str(private$.data$time_start), '\n')
      if(self$is_finished()) cat('Finished at:', private$.serializer$date2str(private$.data$time_end), '\n')
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
      id <- private$.data$id
      if (self$is_finished()) {
        id <- paste0('[X] ', id)
      } else {
        id <- paste0('[ ] ', id)
      }
      ev <- tail(self$get_events(), 1)
      s = paste0(id, ' (', private$.data$comments, ') -> ', 
                 self$get_status(), ' at ', 
                 self$get_time_status(),
                 ' last upd.: ', names(ev), ' (', ev, ')'
      )
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
