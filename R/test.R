#test of library. Build and instal library with make all install
# install.packages(c("R6", "jsonlite", "magrittr", "filelock"))
library(R6)
library(yaml)
library(jsonlite)
library(magrittr)
library(filelock)

source(file = 'R/paramComp.R')
source(file = 'R/taskLog.R')
source(file = 'R/batchComp.R')
source(file = 'R/general.R')

a <- paramComp$new(parameter_list = list(id = 'TEST_COMP', mean = 1.0, sd = 1.0, trials =1000))
b <- paramComp$new(parameter_list = list(id = 'COMP', mean = 1, sd = 1, trials =1000))
b$writeJSON_def()
fromJSON(b$writeJSON_def())
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
class
a
b$writeJSON_def()

class(a)
x <- taskLog$new()
x$add_entry(id = "e2", name = "segunda")
x$add_entry(id = "e3", name = "segunda", Robject_names = c("uno", "dos"))
x$get_list_definition(str_dates = FALSE)

x$get_list_definition(str_dates = TRUE)
x$log_tabular(str_dates = TRUE)
x$close_entry('e2')
x$log_tabular(str_dates = TRUE)

batch <- batchComp$new(a, file_name = 'batch_<id>_mean_<mean>.json')
batch$add_task(id = 't1', name = 'task1', descr = 'uno')
batch$add_task(id = 't2', name = 'task2', descr = 'dos', notes = "not sure what's going on")
batch$add_task(id = 't3', name = 'task3', descr = 'dos', notes = "not sure what's going on", depends = c('t1', 't2'))
batch$complete_task('t1')
batch$complete_task('t2')
batch$filename
batch$getJSON()
batch$write()
batch$read()
batch$log$get_list_definition(str_dates = TRUE)
batch

aaa <- a$clone()  

#log merging
self <- taskLog$new()
self$add_entry(id = "e1", name = "segunda")
self$add_entry(id = "e2", name = "segunda", Robject_names = c("uno", "dos"))
def <- self$get_list_definition(str_dates = TRUE)
self$add_entry(id = "e4", name = "segunda", Robject_names = c("uno", "dos"))
self$get_list_definition(str_dates = FALSE)
self$log_tabular(str_dates = FALSE)

other <- taskLog$new()
other$set_log(def)
other$close_entry('e2')
other$add_entry('e5', name = 'cinco' )

self$log_merge(other)
self$get_list_definition(str_dates = FALSE)
self$log_tabular(str_dates = FALSE)

# writing with file lock
batch$write(); batch$write()
library(snowfall)
snowfall::sfInit(parallel = TRUE, cpus = 10)

a <- paramComp$new(parameter_list = list(id = 'TEST_COMP', mean = 1.0, sd = 1.0, trials =1000))
batch <- batchComp$new(a, file_name = 'batch_<id>_mean_<mean>.json', concurrent = TRUE)
batch$add_task(id = 't1', name = 'task1', descr = 'uno')
batch$add_task(id = 't2', name = 'task2', descr = 'dos')
batch
(batch$complete_task('t1'))
batch$log$completed('t1')
batch$log$dependencies('t1')
batch$write()
bb <- batchComp$new(file_name = batch$filename)
x <- snowfall::sfSapply(1:2, 
   fun = function(x, a){ 
      library(R6)
      library(yaml)
      library(jsonlite)
      library(magrittr)
      library(filelock)
      source(file = 'R/paramComp.R')
      source(file = 'R/taskLog.R')
      source(file = 'R/batchComp.R')
      source(file = 'R/general.R')
      batch <- batchComp$new(file = a)
      batch$add_task(id = paste0('t2', x, collapse  = '.'), name = 'task2', descr = 'dos', notes = "not sure what's going on")
      batch$add_task(id = paste0('t3', x, collapse  = '.'), name = 'task3', descr = 'dos', notes = "not sure what's going on", depends = c('t1', 't2'))
      batch$write()
   },
   a = batch$filename 
)
batch$read()
batch
r <- paramComp$new()
batch$same_parameters(b)
