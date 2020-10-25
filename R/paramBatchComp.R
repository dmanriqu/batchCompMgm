paramBatchComp <- R6::R6Class(
  classname = 'paramBatchComp',
  inherit = paramComp,
  private = list(
    .get_params_for_trial_single = function(trial){
      pt <- p <- self$values
      pos <- which(p$trials == trial)
      for (f in p$by_trial){
        pt$trials <- NULL
        pt$trial <- trial
        pt$by_trial <- NULL
        pt$id <- paste0(self$values$id, '_TRIAL')
        pt[[f]] <- p[[f]][[pos]]
      }
      a <- paramComp$new(parameter_list = pt)
      return(a) 
    }
  ),
  public = list(
    initialize = function(
      load_from_file = NULL,
      strJSON  = NULL, 
      parameter_list = NULL,
      eq_function = function(a, b) {all.equal(a, b)}
    ) {
    if (is.null(parameter_list$trials)) 
      stop ('This class requires a parameter named "trials" with the list of trial names')
     parameter_list$trials <- as.character(parameter_list$trials)
      super$initialize(
        load_from_file = load_from_file,
        strJSON  = strJSON,
        parameter_list = parameter_list,
        eq_function = eq_function
      )
      if (is.null(self$values$by_trial)) 
        stop ('This class requires a parameter named "by_trial" with the list of parameters that will vary from trial to trial')
      for (t in self$values$by_trial){
        if (is.null(self$values[[t]])) {
          stop('Parameter "', t, '" referred in field "by_trial" does not exist')
        }
        if (length(self$values[[t]]) != length(self$values$trials)) {
          stop('Parameter "', t, '" contains less elements than number of trials in "trials"')
        }
      }
    },
    get_params_for_trials  = function(trials = self$values$trials){
      r <- list()
      for (t in trials){
        p <- private$.get_params_for_trial_single(t)
        r[[as.character(t)]] <- p
      }
      return(r)
    }
  )
)

a <- paramBatchComp$new(
  parameter_list = list(
    id = 'TEST_COMP', 
    mean = c(1.0, 2.0), 
    sd = c(1.0,10), 
    trials =1:2, 
    by_trial = c('mean', 'sd')
  )
)
a$get_params_for_trials()
