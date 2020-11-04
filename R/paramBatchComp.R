paramBatchComp <- R6::R6Class(
  classname = 'paramBatchComp',
  inherit = paramComp,
  private = list(
    .get_params_for_trial_single = function(trial){
      pt <- p <- self$values
      pos <- which(p$trials == trial)
      for (f in p$by_trial){
        pt$id <- paste0(self$values$id, '_TRIAL')
        pt$trials <- NULL
        pt$trial <- trial
        pt$by_trial <- NULL
        pt[[f]] <- p[[f]][[pos]]
      }
      f <- names(pt)
      r <- gregexpr(pattern = '_pattern$', f, perl = TRUE)
      indx_pat <- which(r > 0)
      for (i in indx_pat){
        if (!is.null(pt[[i]])){
          nam <- names(pt)[i]
          new_cont <- replace_markers( pt[[nam]] , pt)
          new_name <- gsub(pattern = '_pattern$', x = nam, replacement = '')  
          pt[[nam]] <- NULL
          pt[[new_name]] <- new_cont
        }
      }
      a <- paramComp$new(parameter_list = pt)
      return(a) 
    },
    .validate_subclass = function(other_list = NULL){
      if (!is.null(other_list)){
        l <- other_list
      } else {
        l <- self$values
      }
      if (is.null(l) || is.null(l$trials)){
        stop('This class requires a parameter named "trials" with the list of trial names')
      }
      if (is.null(l$by_trial)){
        stop ('This class requires a parameter named "by_trial" with the list of parameters that will vary from trial to trial')
      }
      for (t in l$by_trial){
        if (is.null(l[[t]])) {
          stop('Parameter "', t, '" referred in field "by_trial" does not exist')
        }
        if (length(l[[t]]) != length(l$trials)) {
          stop('Parameter "', t, '" contains a different number of elements than number of trials in "trials"')
        }
      }
    }
  ),
  public = list(
    initialize = function(
      load_from_file = NULL,
      parameter_list = NULL,
      eq_function = function(a, b) {all.equal(a, b)},
      persist_format = c('json', 'yaml')
    ) {
      if(!is.null(parameter_list))  parameter_list$trials <- as.character(parameter_list$trials)
      super$initialize(
        load_from_file = load_from_file,
        parameter_list = parameter_list,
        eq_function = eq_function,
        persist_format = persist_format[1]
      )
      if (self$is_loaded){
        private$.validate_subclass()
      }
    },
    get_params_for_trials  = function(trials = self$values$trials){
      r <- list()
      for (t in trials){
        p <- private$.get_params_for_trial_single(t)
        r[[as.character(t)]] <- p
      }
      return(r)
    },
    string_from_fields = function(pattern = '<id>'){
      res <- character()
      for (t in self$get_params_for_trials()){
        res <- c(res, t$string_from_fields(pattern))
      }
      res
    },
    load_list_definition = function(def = NULL, str_dates = TRUE){
      super$load_list_definition(def = def, str_dates = str_dates)
      private$.validate_subclass()
    },
    load = function(file_name = NULL, string = NULL){
      super$load(file_name = file_name, string = string)
      private$.validate_subclass()
    },
    remove_fields = function(field_names){
      l <- private$.values
      super$remove_fields(field_names)
      e <- tryCatch(
        {
          private$.validate_subclass()
          NULL
        },
        error = function(e){
          private$.values <- l
          return(e)
        },
      )
      if(!is.null(e)) stop(e)
    },
    update_fields = function(lst_fields_values) {
      l <- self$values
      super$update_fields(lst_fields_values)
      e <- tryCatch(
        {
          private$.validate_subclass()
          NULL
        },
        error = function(e){
          private$.values <- l
          return(e)
        }
      )
      if(!is.null(e)) stop(e)
    }
  )
)
#a <- paramBatchComp$new()
#a <- paramBatchComp$new(
#  parameter_list = list(
#    id = 'TEST_COMP', 
#    description = 'mean of <mean>',
#    fn_pattern = './<id>_<trial>.j',
#    mean = c(1.0, 2.0), 
#    sd = c(1.0,10), 
#    trials =1:2,
#    by_trial = c('mean', 'sd')
#  )
#)
#a$get_params_for_trials()
