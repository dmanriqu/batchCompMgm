# New version of TaskLog

taskLog <- R6::R6Class(
  classname = "taskLog",
  inherit = base_mgmObj,
  private = list(
   .data = list(),
   .concurrent = FALSE,
   .uptheline= function(l, node, check = node){
     #Determine all the nodes up the line from this one
     # Also evaluates if there's a circular reference
     a <- l[[node]]
     if (check %in% a){
       stop('circular reference!')
     }
     li <- c()
     if (is.null(a)) return(NULL)
     for (n in a){
       #recursive call!
       r <- private$.uptheline(l, n, check)
       li <- append(li,r)
     }
     return(unique(append(li, a)))
   },
   .downtheline = function(l, node, check = node){
     #Determine all the nodes in a tree down the line from node.
     asc1 <- function(lst, node){
       #WHich nodes list 'node' as a requisite
       res <- as.character()
       for (i in seq_along(lst)){
         if (node %in% lst[[i]]) {
           res <- append(names(lst[i]), res)
         }
       }
       return(res)
     }
     a <- asc1(l, node)
     if (check %in% a){
       stop('circular reference detected!')
     }
     li <- c()
     if (is.null(a)) return(NULL)
     for (n in a){
       #Recursive call!
       r <- private$.downtheline(l, n, node)
       li <- append(li,r)
     }
     return(unique(append(li, a)))
    }
  ),
  active = list(
    concurrent = function() {
      return(private$.concurrent)
    }
  ),
  public = list(
    initialize = function(concurrent = FALSE, persist_format = c('json', 'yaml')) {
      super$initialize(persist_format = persist_format[1])
      private$.concurrent = concurrent
    },
    get_task = function(id){
      return(private$.data[[id]])
    },
    get_tasks = function(ids){
      return(private$.data[ids])
    },
    #--- These are wrappers for methods from task object in private$.data ----
    #   --- 1) task actions ----
    create_task = function(
      id,
      description = NULL,
      requisites =  NULL,
      params = NULL,
      comments = NULL,
      filenames = NULL,
      objects = NULL,
      error_if_exists = FALSE
    ) {
      if (id %in% names(private$.data)) {
        msg <- paste0("Task name '", id, "' already in use. Did not create.")
        if (error_if_exists)
          stop(msg)
        else{
          warning(msg)
          return()
        }
      }
      # check if dependencies are correct.
      #  this will throw an error if it detects a circular reference
      if (!is.null(requisites)){
        all_deps <- lapply(private$.data, FUN = function(x) x$get_requisites())
        all_deps[[id]] <- requisites
        private$.uptheline(l = all_deps, node = id, check = id)
      }
      private$.data[[id]] <-  CTask$new( 
        id = id, description = description, requisites = requisites, 
        params = params, comments = comments,
        persist_format = private$.serializer$format
      )
      invisible(self)
    },
    import_task = function(task){
      if (!('CTask' %in% class(task))){
        stop('Object is not a task.')
      }
      if (self$is_task_defined(task$get_id())){
        stop('Task ', task$get_id(), ' already defined.')
      }
      private$.data[[task$get_id()]] <- task$clone()
      invisible(self)
    },
    start_task = function(id) {
      if (!self$is_task_defined(id)) {
        stop ("Cannot start. Task ", id,  " doesn't exist")
      } else if (self$is_task_started(id)) {
        warning ("Task ", id,  " already started")
      } else if (self$is_task_finished(id)) {
        warning ("Task ", id,  " already finished") 
      } else if (!self$is_task_cleared(id)) {
        stop ("Requisites for task ", id, "not completed")
      } else {
        private$.data[[id]]$start()
      }
      invisible(self)
    },
    finish_task = function(id) {
      if (!self$is_task_defined(id)) {
        stop ("Cannot finish. Task ", id,  " doesn't exist")
      } else if (!self$is_task_started(id)) {
        stop ("Task ", id,  " not started")
      } else if (self$is_task_finished(id)) {
        warning ("Task ", id,  " already finished")
      } 
      else {
        private$.data[[id]]$finish()
      }
      invisible(self)
    },
    task_unstart = function(id, I_AM_SURE = FALSE){
      if (!I_AM_SURE) stop("Need to explicitly declare that you know what you by setting argument I_AM_SURE = TRUE")
      else if (private$.concurrent) warning('Cannot unstart while in concurrent mode. Nothing done.')
      else self$get_task(id)$unstart()
    },
    task_unfinish = function(id, I_AM_SURE = FALSE){ 
      if (!I_AM_SURE) stop("Need to explicitly declare that you know what you by setting argument I_AM_SURE = TRUE")
      else if (private$.concurrent) warning('Cannot unfinish while in concurrent mode. Nothing done.')
      else self$get_task(id)$unfinish()
    },
    task_undefine = function(id, I_AM_SURE = FALSE){
      if (!I_AM_SURE) stop("Need to explicitly declare that you know what you by setting argument I_AM_SURE = TRUE")
      else if (private$.concurrent) warning('Cannot undefine while in concurrent mode. Nothing done.')
      private[[id]] <- NULL
    },
    # 2) Individual Task status -----
    is_task_defined = function(id){
      return(id %in% names(private$.data))
    },
    is_task_started = function(id) {
      return(private$.data[[id]]$is_started())
    },
    is_task_finished = function(id) {
      return(private$.data[[id]]$is_finished())
    },

    # 3) Task status w/ respect to other tasks ----
    is_task_cleared = function(id) {
      x <- private$.data[[id]]$get_requisites()
      for (t in x){
        if (!self$get_task(t)$is_finished()) return (FALSE)
      }
      return(TRUE)
    },
    get_all_requisite_tasks = function(id){
      x <- self$get_dependency_tree()
      private$.uptheline(x, id, id)
    },
    get_all_dependent_tasks = function(id){
      x <- self$get_dependency_tree()
      private$.downtheline(x, id, id)
    },
    get_unmet_requisites = function(id){
      x <- self$get_all_requisite_tasks(id)
      return (intersect(x, self$get_all_unfinished_tasks()))
    },
   # 4) Status of all tasks ----
    get_dependency_tree  = function(){
      dep_tree <- lapply(private$.data, FUN = function(x)x$get_requisites())
      return(dep_tree)
    },
    get_all_tasks = function(){
      l <- character()
      for (task in private$.data){
        l <- append(l, task$get_id())
      }
      return(l)
    },
    get_all_unfinished_tasks = function(){
      x <- character()
      for (t in private$.data){
        if (!t$is_finished()){
          x <- c(x, t$get_id())
        }
      }
      return(x)
    },
    get_all_finished_tasks = function(){
      x <- character()
      for (t in private$.data){
        if (t$is_finished()){
          x <- c(x, t$get_id())
        }
      }
      return(x)
    },
    are_all_tasks_finished = function(){
      x <- self$get_all_unfinished_tasks()
      return(length(x) == 0)
    },
    #Class communication ----
    get_list_definition = function() {
      x <- list(
        class = class(self),
        concurrent = private$.concurrent
      )
      x$data <- list()
      for (t in private$.data){
        x$data[[t$get_id()]] <- t$get_list_definition()
      }
      return(x)
    },
    load_list_definition = function(def) {
      if(!('taskLog' %in% def$class)) stop ('Not a definition of a taskLog object. Aborting.')
      data <- list()
      for (t in def$data){
        data[[t$id]] <- CTask$new(id = 'xx', persist_format = private$.serializer$format)
        data[[t$id]]$load_list_definition(t)
      }
      private$.concurrent <- def$concurrent 
      private$.data <- data
    },
   
   #General object stuff ----
    log_merge = function(other_log) {
      #First check if any of the logs are empty
      if(!self$is_log_in_use()){
        self$load_list_definition( other_log$get_list_definition() )
        return()
      } else if (!other_log$is_log_in_use()) {
        return()
      }
      #common and different tasks.
      me <- self$get_all_tasks()
      notme <- other_log$get_all_tasks()
      common <- intersect(me, notme)
      newones <- setdiff(notme, me);
      # Now add the events in the other log that are missing in the present
      for (id in newones) {
        self$import_task(other_log$get_task(id))
      }
      # For the common ones, we need to update the fields to the latest versions
      for (id in common) {
        private$.data[[id]]$update_with_another(other_log$get_task(id))
      }
      invisible(self)
    },
    is_log_in_use = function(id) {
      return(length(private$.data) > 0)
    },
    change_scheduling_mode = function(concurrent = !private$.concurrent){
      private$.concurrent <- concurrent
      message('Scheduling mode is now: ', ifelse(concurrent, 'concurrent', 'sequential'))
    },
    print = function(){
      i <- 1
      for (t in private$.data){
        id <- t$get_id()
        r <- t$get_requisites()
        if (!is.null(r) && length(r) > 0){
          u <- self$get_unmet_requisites(id)
          s <- sapply(r, FUN = function(x) ifelse(!(x %in% u), paste0('[X] ', x) , paste0('[ ] ', x) ))
          req <- paste0(' | Requires: ', paste(s, collapse = ', '))
        } else {
          req <- ''
        }
        cat('(', i, ') ', t$get_oneline_summary(), req, '\n', sep = '')
        i <- i + 1
      }
    },
    peek = function(){
      return(private$.data)
    }
  )
)
#l <- taskLog$new()
#l$create_task(id = 'hola', comments = 'Created manually.')
#l$create_task(id = 'adios', requisites = 'hola')
#l$create_task(id = 'x1', requisites = 'x2')
#l$create_task(id = 'x2', requisites = 'x3')
#a <- l$get_list_definition()
#l2 <- taskLog$new()
#(l2$load_list_definition(a))
#l2$start_task('hola')
#l2$finish_task('hola')
#l
#l2
#l$log_merge(l2)
#l
#
#a <- l$get_task('x2')
#l$import_task(a)
#b <- CTask$new(id = 'externa' )
#l$import_task(b)
#l$task_unfinish('hola', I_AM_SURE = TRUE)
#l$get_all_tasks()
#l$get_all_finished_tasks()
#l$get_all_unfinished_tasks()
#l$are_all_tasks_finished()
#l$get_all_requisite_tasks('x1')
#l$get_all_unfinished_tasks()
#l$get_unmet_requisites('hola')
#l$get_unmet_requisites('x1')
#l$get_dependency_tree()
#l$get_all_dependent_tasks('hola')
#l$change_scheduling_mode()
#l$print()
#l$getJSON()
#x <- l$get_list_definition()
#l$load_list_definition(x)
#l$get_task('adios')$get_list_definition()
#l$get_task('adios')$start()
#
#lapply(l$peek(), FUN = function(x)x$get_requisites())
