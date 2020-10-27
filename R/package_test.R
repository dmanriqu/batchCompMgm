rm(list = ls())
library(R6)
library(yaml)
library(jsonlite)
library(magrittr)
library(filelock)
library(snowfall)

source(file = 'R/paramComp.R')
source(file = 'R/paramBatchComp.R')
source(file = 'R/taskLog.R')
source(file = 'R/Comp.R')
source(file = 'R/general.R')
a <- paramComp$new(parameter_list = list(id = 'TEST_COMP', mean = 1.0, sd = 1.0, trials =1000))
batch <- Comp$new(a, file_name = 'batch_<id>_mean_<mean>.json', concurrent = TRUE, overwrite_file = TRUE)
batch$create_task(id = 't1', name = 'task1', descr = 'uno', auto_start = TRUE)
batch$update()
batch$create_task(id = 't2', name = 'task2', descr = 'dos', auto_start = TRUE)
batch
batch$update()
(batch$finish_task('t1'))
batch$log$is_task_finished('t1')
batch$log$dependencies('t1')
batch$update()
x <- Comp$new(file_name = 'batch_TEST_COMP_mean_1.json')
x$update()
#create a tree:
batch$create_task(id = 'uno')
batch$create_task(id = '1.1', depends = 'uno')
batch$create_task(id = '1.3', depends = 'uno')
batch$create_task(id = '1.2', depends = 'uno')
batch$create_task(id = '1.3.1', depends = c('1.3', '1.2', '1.3.2', 'hola'))
batch$create_task(id = '1.3.2', depends = '1.3')
batch$create_task(id = '1.3.3', depends = '1.3')
batch$create_task(id = '1.3.2.1', depends = '1.3.2')
batch$create_task(id = 'hola', depends = 'x')
batch$create_task(id = 'x', depends = c('y', 'z'))
batch$create_task(id = 'z', depends = 'uno')
batch$create_task(id = 'w', depends = 'x')
batch$create_task(id = 'XX', depends = 'XX')
batch$update()
batch
batch$get_log_object()$get_all_requisite_tasks('1.3.2.1')
batch$get_log_object()$get_all_dependent_tasks('uno')
batch$get_log_object()$task_undefine('1.3.2.1')
batch$get_log_object()$task_unstart('1.3.2.1')
snowfall::sfInit(parallel = TRUE, cpus = 12)
snowfall::sfSource(file = 'R/paramComp.R')
snowfall::sfSource(file = 'R/paramBatchComp.R')
snowfall::sfSource(file = 'R/taskLog.R')
snowfall::sfSource(file = 'R/Comp.R')
snowfall::sfSource(file = 'R/general.R')
batch$clean_lockfile()
tasks <- c( '1.1', '1.3', '1.2', '1.3.1', '1.3.2', '1.3.3', '1.3.2.1')
x <- snowfall::sfSapply(tasks, 
   fun = function(id, a){ 
      batch <- Comp$new(file_name = a)
      if(batch$is_task_finished(id)) return("task already finished")
      batch$start_when_ready(id = id, poll_interval = .01, timeout = 20)
      if (!batch$is_task_started(id)) {
         warning(paste('task ', id, 'timed out.'))
         return('failed')
      }
      Sys.sleep(runif(1,2, 10))
      batch$finish_task(id)
      batch$update()
      return(paste('finished', id))
   },
   a = batch$filename 
)
   x
batch$update()
batch$set_concurrent_off()
batch$get_log_object()$task_unfinish('1.3', I_AM_SURE = FALSE)
batch$get_log_object()$task_unstart('1.3', I_AM_SURE = FALSE)
batch
batch$save_as(batch$filename, overwrite_file = TRUE)
batch$update()
batch
r <- paramComp$new()
r$writeJSON_def()
r$is_loaded




#detach('package:batchCompMgm', unload = TRUE)
