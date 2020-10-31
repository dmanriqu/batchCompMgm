#test of library. Build and instal library with make all install
# install.packages(c("R6", "jsonlite", "magrittr", "filelock"))
library(R6)
library(yaml)
library(jsonlite)
library(magrittr)
library(filelock)

source(file = 'R/paramComp.R')
source(file = 'R/paramBatchComp.R')
source(file = 'R/taskLog.R')
source(file = 'R/Comp.R')
source(file = 'R/general.R')

(a <- paramComp$new(parameter_list = list(id = 'BASE_NOSTRAT', mean = 1.0, sd = 1.0, trials =1000)))
(b <- paramComp$new(parameter_list = list(id = 'COMP', mean = 1, sd = 1, trials =1000)))
a$writeJSON_def(file = './tmp/<id>_param.json')
(a$loadJSON_def(a$writeJSON_def()))
b$generate_file_name()
a$generate_file_name()
a == b
a == a
cc <- a$clone()
a == cc

(js  <- a$writeJSON_def())
b$loadJSON_def(js)
b$writeJSON_def()
b
(jsb  <- b$writeJSON_def())
b$loadJSON_def(js)
b$get_list_definition()
b$writeJSON_def()
a == b
dput(a$values)
dput(b$values)


class(a)
b$writeJSON_def()

x <- taskLog$new(concurrent = T)
x$log_tabular()
x$create_task(id = "e2", name = "segunda")
x$create_task(id = "e3", name = "segunda", Robject_names = c("uno", "dos"))
x$get_list_definition(str_dates = FALSE)
x$log_tabular(str_dates = TRUE)
x$finish_task('e2')
x$start_task('e2')
x$log_tabular(str_dates = TRUE)
x$is_task_started('e2')
x$is_task_finished('e2')
x$is_task_cleared('e2')
x$create_task(id = 't2', name = 'task2', descr = 'dos', notes = "not sure what's going on")
x$create_task(id = 't1', name = 'task1', descr = 'uno')
x$create_task(id = 't3', name = 'task3', descr = 'dos', notes = "not sure what's going on", depends = c('t1', 't2'))
x$unmet_dependencies('e1')
x$unmet_dependencies('t3')
x$start_task('t2')
x$finish_task('t2')
x$is_task_cleared('t3')
x$unmet_dependencies('t3')
x$start_task('t1')
x$finish_task('t1')
x$unmet_dependencies('t3')
x$is_task_cleared('t3')
x$log_tabular(str_dates = FALSE)
x$is_log_in_use()


a
batch <- batchComp$new(a, file_name = 'batch_<id>_mean_<mean>.json', overwrite_file = TRUE)
batch2 <- batchComp$new(file_name = 'batch_BASE_NOSTRAT_mean_1.json')
batch2$filename
batch3 <- batchComp$new(a)
batch3$filename
batch$update()
batch$create_task(id = 't1', name = 'task1', descr = 'uno')
batch$create_task(id = 't2', name = 'task2', descr = 'dos', notes = "not sure what's going on")
batch$create_task(id = 't3', name = 'task3', descr = 'dos', notes = "not sure what's going on", depends = c('t1', 't2'))

batch$finish_task('t2')
batch$filename
batch$getJSON()
batch$update()
batch$log$get_list_definition(str_dates = TRUE)
batch


#log merging
self <- taskLog$new()
self$create_task(id = "e1", name = "segunda")
self$create_task(id = "e2", name = "segunda", Robject_names = c("uno", "dos"))
def <- self$get_list_definition(str_dates = TRUE)
self$create_task(id = "e4", name = "segunda", Robject_names = c("uno", "dos"))
self$get_list_definition(str_dates = FALSE)
self$log_tabular(str_dates = FALSE)

other_log_object <- taskLog$new()
other_log_object$load_list_definition(def)
other_log_object$finish_task('e2')
other_log_object$create_task('e5', name = 'cinco' )
other_log_object$log_tabular()

self$log_merge(other)
self$get_list_definition(str_dates = FALSE)
self$log_tabular(str_dates = FALSE)

# writing with file lock
batch$write(); batch$write()
library(snowfall)
snowfall::sfInit(parallel = TRUE, cpus = 10)
sfSource(file = 'R/paramComp.R'); sfSource(file = 'R/taskLog.R'); sfSource(file = 'R/batchComp.R'); sfSource(file = 'R/general.R')
sfLibrary(R6); sfLibrary(yaml); sfLibrary(jsonlite); sfLibrary(magrittr); sfLibrary(filelock)

a <- paramComp$new(parameter_list = list(id = 'TEST_COMP', mean = 1.0, sd = 1.0, trials =1000))
batch <- batchComp$new(a, file_name = 'batch_<id>_mean_<mean>.json', concurrent = TRUE)
batch$create_task(id = 't1', name = 'task1', descr = 'uno')
batch$create_task(id = 't2', name = 'task2', descr = 'dos')
batch
(batch$complete_task('t1'))
batch$log$completed('t1')
batch$log$dependencies('t1')
batch$write()
bb <- batchComp$new(file_name = batch$filename)
x <- snowfall::sfSapply(1:2, 
   fun = function() {
      batch <- batchComp$new(file = a)
      batch$create_task(id = paste0('t2', x, collapse  = '.'), name = 'task2', descr = 'dos', notes = "not sure what's going on")
      batch$create_task(id = paste0('t3', x, collapse  = '.'), name = 'task3', descr = 'dos', notes = "not sure what's going on", depends = c('t1', 't2'))
      batch$write()
   },
   a = batch$filename 
)
batch$read()
batch
r <- paramComp$new()
batch$same_parameters(b)
