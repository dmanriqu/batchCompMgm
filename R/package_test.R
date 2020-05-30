rm(list = ls())
library('batchCompMgm', lib.loc = 'cache')
a <- paramComput$new(parameter_list = list(id = 'TEST_COMP', mean = 1.0, sd = 1.0, trials =1000))
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
      batch <- batchComp$new(file = a)
      batch$add_task(id = paste0('TTT2', x, collapse  = '.'), name = 'task2', descr = 'dos', notes = "not sure what's going on")
      batch$add_task(id = paste0('XXX3', x, collapse  = '.'), name = 'task3', descr = 'dos', notes = "not sure what's going on", depends = c('t1', 't2'))
      batch$write()
   },
   a = batch$filename 
)
batch$read()
batch
r <- paramComput$new()
r$writeJSON_def(file = '')
r$is_loaded





a <- paramComput$new()
i <- batchCompMgm::batchComp$new(a)
i$write()
detach('package:batchCompMgm', unload = TRUE)
