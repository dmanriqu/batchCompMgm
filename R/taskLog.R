# tasklog object

taskLog <- R6::R6Class(
  classname = "taskLog",
  private = list(
   .log = list()
  ),
  active = list(),
  public = list(
    add_entry = function(
      id = NULL, name = "", descr = "", notes = "", file_name = "",
      Robject_names = c(), depends = c(), task_prefix = "T"
    ) {
      if (is.null(id)) {
        id <- paste0(task_prefix, length(private$.log) + 1)
      }
      while (id %in% names(private$.log)) {
        id <- paste0(id, "x")
        warning("Duplicated task Id. Appending 'x'")
      }
     #TODO: add code to check dependencies
     x <- list(
       id = id, name = name, descr = descr,
       time_init = Sys.time(), 
       time_end = as.POSIXct(NA),
       notes = notes, file_name = file_name,
       Robject_names = Robject_names,
       depends = depends
     )
     private$.log[[id]] <- x
     invisible(self)
    },
    close_entry = function(id) {
      if (is.null(private$.log[[id]])) {
        stop("event doesn't exist")
      }
      private$.log[[id]]$time_end  <-  Sys.time()
      invisible(self)
    },
    log_tabular = function(str_dates = TRUE) {
      y <- lapply(
         private$.log,
         FUN = function(x) {
           data.frame(
            id = x$id, name = x$name, descr = x$descr,
            time_init = x$time_init, time_end = x$time_end,
            notes = x$notes,
            file_name = x$file_name,
            Robject_names = paste(x$Robject_names, collapse = ", "),
            depends = paste(x$depends, collapse = ", "),
            stringsAsFactors = FALSE
           )
         }
      )
      y <- do.call(rbind, y)
      if (str_dates) {
        y$time_init <- date2str(y$time_init)
        y$time_end  <- date2str(y$time_end)
      }
      return(y)
    },
    get_list_definition = function(str_dates = TRUE) {
      x <- private$.log
      if (str_dates) {
        x <- lapply(
          private$.log,
          FUN = function(y) {
            y$time_init  <- date2str(y$time_init)
            y$time_end  <- date2str(y$time_end)
            return(y)
          }
        )
      }
      return(x)
    },
    set_log = function(x) {
      #convert string dates to POSIX
      x <- lapply(
        x,
        FUN = function(y) {
          y$time_init  <- str2date(y$time_init)
          y$time_end   <- str2date(y$time_end)
          return(y)
        }
      )
      private$.log <- x
    },
    log_merge = function(other_log_object) {
      #common and different events.
      o <- self$log_tabular(str_dates = FALSE)[, c("id", "time_init", "time_end")]
      n <- other_log_object$log_tabular(str_dates = FALSE)[, c("id", "time_init", "time_end")]
      common <- intersect(o$id, n$id)
      n_minus_o <- setdiff(n$id, o$id)
      #Update events:
      o$time_end[is.na(o$time_end)] <- as.POSIXct("1789-07-01 01:00:00 UTC")
      n$time_end[is.na(n$time_end)] <- as.POSIXct("1789-07-01 01:00:00 UTC")
      harm <- pmax(n[common, "time_end"], o[common, "time_end"])
      o[common, "time_end"] <- n[common, "time_end"] <- harm
      o[o$time_end < as.POSIXct("1789-12-12"), "time_end"] <- as.POSIXct(NA)
      n[n$time_end < as.POSIXct("1789-12-12"), "time_end"] <- as.POSIXct(NA)
      # Now add the events in the other log that are missing in the present
      if (length(n_minus_o) > 0) {
        o <- rbind(o, n[n_minus_o, ])
      }
      all <- o$id
      # Now update the log data
      private$.log <- append(
        private$.log, other_log_object$get_list_definition(str_dates = FALSE)[n_minus_o]
      )
      for (i in all) {
        private$.log[[i]]$time_end <- o$time_end[o$id == i]
      }
      invisible(self)
    },
    dependencies = function(id) {
      private$.log[[id]]$depends
    },
    completed = function(id) {
      #maybe check that the date is greater than time_init
      e <- private$.log[[id]]$time_end
      return(!is.null(e) & !is.na(e))
    },
    cleared = function(id) {
      d <- self$dependencies(id)
      if (length(d) == 0) {
        return(TRUE)
      }
      private$.log[d]
      self$log_tabular(str_dates = TRUE)[d, "time_end"]
    }
  )
)
