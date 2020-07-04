library(R6)
library(yaml)
library(jsonlite)
library(magrittr)
library(filelock)

source(file = 'R/paramComp.R')
source(file = 'R/taskLog.R')
source(file = 'R/batchComp.R')
source(file = 'R/general.R')
library(snowfall)
snowfall::sfInit(parallel = TRUE, cpus = 10)

a <- paramComp$new(parameter_list = list(id = 'TEST_COMP', mean = 1.0, sd = 1.0, trials =1000))
batch <- batchComp$new(a, file_name = 'batch_<id>_mean_<mean>.json', concurrent = TRUE)
batch$create_task(id = 't1', name = 'task1', descr = 'uno', auto_start = TRUE)
batch$create_task(id = 't2', name = 'task2', descr = 'dos', auto_start = TRUE)
batch
(batch$finish_task('t1'))
batch$write()
bb <- batchComp$new(file_name = batch$filename)


sfSource(file = 'R/paramComp.R'); sfSource(file = 'R/taskLog.R'); sfSource(file = 'R/batchComp.R'); sfSource(file = 'R/general.R')

sfLapply(
  list(
    expression({
      b$create_task('sub1', depends = 'sub3', auto_start = FALSE)
      b$start_when_ready('sub1')
      b$start_task('sub1')
      Sys.sleep(time = 12)
      b$finish_task('sub1')
    }),
    expression   ({
         b$create_task('sub2', depends = 'sub3', auto_start = FALSE)
         b$start_when_ready('sub2')
         b$start_task('sub2')
         Sys.sleep(time = 2)
         b$finish_task('sub2')
         b$write()
       }),
     expression({
       b$create_task('sub3', depends = 't1', auto_start = FALSE)
       b$start_when_ready('sub3')
       Sys.sleep(time = 1)
       b$finish_task('sub3')
       b$write()
     }),
     expression({
       b$create_task('sub4', depends = c('sub2', 'sub1'), auto_start = FALSE)
       b$start_when_ready('sub2')
       Sys.sleep(time = 1)
       b$finish_task('sub4')
       b$write()
     })
  ),
  fun = function(a, b){
    eval(a)
    return()
  },
  b = batch$clone()
)
