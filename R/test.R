#test of library. Build and instal library with make all install
# install.packages(c("R6", "jsonlite", "magrittr", "filelock"))
library(R6)
library(yaml)
library(jsonlite)
library(magrittr)
library(filelock)

source(file = 'R/paramComput.R')
source(file = 'R/taskLog.R')
source(file = 'R/batchComp.R')
source(file = 'R/general.R')

a <- paramComput$new(parameter_list = list(id = 'TEST_COMP', mean = 1.0, sd = 1.0, trials =1000))
b <- paramComput$new(parameter_list = list(id = 'COMP', mean = 1, sd = 1, trials =1000))
b$writeJSONDef()
fromJSON(b$writeJSONDef())
a == b
a == a

(js  <- a$writeJSONDef())
b$loadJSONDef(js)
b$writeJSONDef()
b
(jsb  <- b$writeJSONDef())
b$loadJSONDef(js)
b$getListDefinition()
b$writeJSONDef()
a == b
dput(a$values) 
dput(b$values)


class(a)
class
a
b$writeJSONDef()

class(a)
x <- taskLog$new()
x$addEntry(id = "e2", name = "segunda")
x$addEntry(id = "e3", name = "segunda", RobjectNames = c("uno", "dos"))
x$getLog()

jsonlite::toJSON(x = x$getLog(), pretty = TRUE) 
x$getLog()
x$log_tabular()
x$closeEntry('e3')
x$log_tabular()

batch <- batchComp$new(a, file_name_rule = 'batch_<id>_mean_<mean>.json')
batch$addTask(id = 't1', name = 'task1', descr = 'uno')
batch$addTask(id = 't2', name = 'task2', descr = 'dos', notes = "not sure what's going on")
batch$addTask(id = 't3', name = 'task3', descr = 'dos', notes = "not sure what's going on", depends = c('t1', 't2'))
batch$completeTask('t1')
batch$filename
(y  <- batch$getJSON())

a

(x <- jsonlite::fromJSON(y))
x
batch$getJSON()
(batch$write(file = 'tmp/batch1.json'))
batch$read(file = 'tmp/batch1.json')
batch$log$test()  
batch$log$getLog()
batch

#log merging
self <- taskLog$new()
self$addEntry(id = "e1", name = "segunda")
self$addEntry(id = "e2", name = "segunda", RobjectNames = c("uno", "dos"))
def <- self$getLog()
self$addEntry(id = "e4", name = "segunda", RobjectNames = c("uno", "dos"))

other <- taskLog$new()
other$setLog(def)
other$closeEntry('e2')
other$addEntry('e5', name = 'cinco' )
other$log_tabular_raw()

self$logMerge(other)
self$log_tabular_raw()
o <- other$log_tabular_raw()
l <- other$getLogRaw()
l[['e2']]$time_end <-  o[o$id == 'e2', 'time_end']


