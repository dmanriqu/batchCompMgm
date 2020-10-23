rm(list = ls())
library(R6)
library(yaml)
library(jsonlite)
library(magrittr)
library(filelock)
library(snowfall)

source(file = 'R/paramComp.R')
source(file = 'R/taskLog.R')
source(file = 'R/batchComp.R')
source(file = 'R/general.R')
a <- paramComp$new(parameter_list = list(id = 'TEST_COMP', mean = 1.0, sd = 1.0, trials =1000))
batch <- batchComp$new(a, file_name = 'batch_<id>_mean_<mean>.json', concurrent = TRUE, overwrite_file = TRUE)
batch$create_task(id = 't1', name = 'task1', descr = 'uno', auto_start = TRUE)
batch$update()
batch$create_task(id = 't2', name = 'task2', descr = 'dos', auto_start = TRUE)
batch
batch$update()
(batch$finish_task('t1'))
batch$log$is_task_finished('t1')
batch$log$dependencies('t1')
batch$update()
x <- batchComp$new(file = 'batch_TEST_COMP_mean_1.json')
x$update()
#create a tree:
batch$create_task(id = 'uno')
batch$create_task(id = '1.1', depends = 'uno')
batch$create_task(id = '1.3', depends = 'uno')
batch$create_task(id = '1.2', depends = 'uno')
batch$create_task(id = '1.3.1', depends = '1.3')
batch$create_task(id = '1.3.2', depends = '1.3')
batch$create_task(id = '1.3.3', depends = '1.3')
batch$create_task(id = '1.3.2.1', depends = '1.3.2')

snowfall::sfInit(parallel = TRUE, cpus = 12)
snowfall::sfSource(file = 'R/paramComp.R')
snowfall::sfSource(file = 'R/taskLog.R')
snowfall::sfSource(file = 'R/batchComp.R')
snowfall::sfSource(file = 'R/general.R')
tasks <- c( '1.1', '1.3', '1.2', '1.3.1', '1.3.2', '1.3.3', '1.3.2.1' )
x <- snowfall::sfSapply(tasks, 
   fun = function(id, a){ 
      batch <- batchComp$new(file_name = a)
      batch$start_when_ready(id = id, poll_interval = .01, timeout = 20)
      if (!batch$is_task_started(id)) {
         warning(paste('task ', id, 'timed out.'))
         return('failed')
      }
      #Sys.sleep(runif(1,2, 10))
      batch$finish_task(id)
      batch$update()
      return(paste('finished', id))
   },
   a = batch$filename 
)
batch$update()
batch
batch$task_unstart('35', I_AM_SURE = T)
batch$update()
batch
r <- paramComp$new()
r$writeJSON_def()
r$is_loaded




#detach('package:batchCompMgm', unload = TRUE)
