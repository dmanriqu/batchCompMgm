# tasklog object

taskLog <- R6::R6Class(
  classname = "taskLog",
  private = list(
   .data = list(),
   .concurrent = FALSE,
   .exists = function(id) {
     return(!is.null(private$.data[[id]]))
   }
  ),
  active = list(
    concurrent = function() {
      return(private$.concurrent)
    }
  ),
  public = list(
    initialize = function(concurrent = FALSE) {
      private$.concurrent = concurrent
    },
    create_task = function(
      id = NULL, name = "", descr = "", notes = "", file_name = "",
      Robject_names = c(), depends = c(), task_prefix = "T",
      auto_start = (!private$.concurrent)
    ) {
      if (is.null(id) && !private$.concurrent) {
        id <- paste0(task_prefix, length(private$.data) + 1)
      } else if (is.null(id)) {
        stop("Concurrent mode set. Need to provide task id.")
      } else if (id %in% names(private$.data)) {
        stop("Task name '", id, "'already in use")
      }
      # while (id %in% names(private$.data)) {
      #   id <- paste0(id, "x")
      #   warning("Duplicated task Id. Appending 'x'")
      # }
      # allow to depend on undefined events.
      # nex <- sapply(depends, FUN = function(x)!private$.exists(x))
      # if (any(nex)){
      #   stop("Dependency event(s) ", paste(depends[nex], collapse = ', '), "not defined")
      # }
      x <- list(
        id = id, name = name, descr = descr,
        time_init = Sys.time(),
        time_start = as.POSIXct(NA),
        time_end = as.POSIXct(NA),
        notes = notes, file_name = file_name,
        Robject_names = Robject_names,
        depends = depends
      )
      if (auto_start) {
        x$time_start <- x$time_init
      }
      private$.data[[id]] <- x
      invisible(self)
    },
    start_task = function(id) {
      if (!private$.exists(id)) {
        stop ("Cannot start. Task ", id,  " doesn't exist")
      } else if (self$is_task_started(id)) {
        warning ("Task ", id,  " already started")
      } else if (self$is_task_finished(id)) {
        warning ("Task ", id,  " already finished")
      } else if (!self$is_task_cleared(id)) {
        stop ("Requisites for task ", id, "not completed")
      } else {
        private$.data[[id]]$time_start  <-  Sys.time()
      }
      invisible(self)
    },
    finish_task = function(id) {
      if (!private$.exists(id)) {
        stop ("Cannot finish. Task ", id,  " doesn't exist")
      } else if (!self$is_task_started(id)) {
        stop ("Task ", id,  " not started")
      } else if (self$is_task_finished(id)) {
        warning ("Task ", id,  " already finished")
      } else {
        private$.data[[id]]$time_end  <-  Sys.time()
      }
      invisible(self)
    },
    log_tabular = function(str_dates = TRUE, na = NA) {
      if (!self$is_log_in_use()) {
        return(NULL)
      } else {
        y <- lapply(
           private$.data,
           FUN = function(x) {
             data.frame(
              id = x$id, name = x$name, descr = x$descr,
              time_init = x$time_init,
              time_start = x$time_start,
              time_end = x$time_end,
              notes = x$notes,
              file_name = x$file_name,
              Robject_names = paste(x$Robject_names, collapse = ", "),
              depends = paste(x$depends, collapse = ", "),
              stringsAsFactors = FALSE
             )
           }
        )
        y <- do.call(rbind, y)
      }
      if (str_dates) {
        y$time_init <- date2str(y$time_init)
        y$time_start <- date2str(y$time_start)
        y$time_end  <- date2str(y$time_end)
      }
      return(y)
    },
    get_list_definition = function(str_dates = TRUE) {
      x <- private$.data
      if (str_dates) {
        x <- lapply(
          private$.data,
          FUN = function(y) {
            y$time_init  <- date2str(y$time_init)
            y$time_start  <- date2str(y$time_start)
            y$time_end  <- date2str(y$time_end)
            return(y)
          }
        )
      }
      return(list(concurrent = private$.concurrent, log = x))
    },
    load_list_definition = function(def) {
      #convert string dates to POSIX
      def$log <- lapply(
        def$log,
        FUN = function(y) {
          y$time_init  <- str2date(y$time_init)
          y$time_start <- str2date(y$time_start)
          y$time_end   <- str2date(y$time_end)
          return(y)
        }
      )
      private$.concurrent <- def$concurrent
      private$.data <- def$log 
    },
    log_merge = function(other_log_object) {
       #First check if any of the logs are empty
       if (!other_log_object$is_log_in_use()) {
         return(self)
       } else if (!self$is_log_in_use()) {
         l <- other_log_object$get_list_definition()
         self$load_list_definition(l)
         return(self)
       }
      #common and different events.
      o <- self$log_tabular(str_dates = FALSE)[, c("id", "time_init", "time_start", "time_end")]
      n <- other_log_object$log_tabular(str_dates = FALSE)[, c("id", "time_init", "time_start", "time_end")]
      common <- intersect(o$id, n$id)
      n_minus_o <- setdiff(n$id, o$id)
      #Update events:
      o$time_start[is.na(o$time_start)] <- as.POSIXct("1789-07-01 01:00:00 UTC")
      n$time_start[is.na(n$time_start)] <- as.POSIXct("1789-07-01 01:00:00 UTC")
      o$time_end[is.na(o$time_end)] <- as.POSIXct("1789-07-01 01:00:00 UTC")
      n$time_end[is.na(n$time_end)] <- as.POSIXct("1789-07-01 01:00:00 UTC")
      harm_start <- pmax(n[common, "time_start"], o[common, "time_start"])
      harm_end <- pmax(n[common, "time_end"], o[common, "time_end"])
      o[common, "time_start"] <- n[common, "time_start"] <- harm_start
      o[common, "time_end"] <- n[common, "time_end"] <- harm_end
      o[o$time_end < as.POSIXct("1789-12-12"), "time_end"] <- as.POSIXct(NA)
      n[n$time_end < as.POSIXct("1789-12-12"), "time_end"] <- as.POSIXct(NA)
      o[o$time_start < as.POSIXct("1789-12-12"), "time_start"] <- as.POSIXct(NA)
      n[n$time_start < as.POSIXct("1789-12-12"), "time_start"] <- as.POSIXct(NA)
      # Now add the events in the other log that are missing in the present
      if (length(n_minus_o) > 0) {
        o <- rbind(o, n[n_minus_o, ])
      }
      all <- o$id
      # Now update the log data
      private$.data <- append(
        private$.data, other_log_object$get_list_definition(str_dates = FALSE)$log[n_minus_o]
      )
      for (i in all) {
        private$.data[[i]]$time_end <- o$time_end[o$id == i]
        private$.data[[i]]$time_start <- o$time_start[o$id == i]
      }
      invisible(self)
    },
    dependencies = function(id) {
      private$.data[[id]]$depends
    },
    unmet_dependencies = function(id) {
      d <- as.character(self$dependencies(id))
      if (length(d) == 0) {
        return(character(0))
      }
      falta <-  as.logical(sapply(d, FUN = function(x)!self$is_task_started(x), simplify = TRUE))
      if (length(falta) == 0) {
        return(character(0))
      }
      return(d[falta])
    },
    is_task_started = function(id) {
      e <- private$.data[[id]]$time_start
      return(!is.null(e) & !is.na(e) & ('POSIXct' %in% class(e)))
    },
    is_task_finished = function(id) {
      e <- private$.data[[id]]$time_end
      return(!is.null(e) & !is.na(e))
    },
    is_task_cleared = function(id) {
      u <- self$unmet_dependencies(id)
      return(length(u) == 0)
    },
    is_log_in_use = function(id) {
      return(length(private$.data) > 0)
    }
  )
)
