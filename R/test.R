#test of library. Build and instal library with make all install
library(R6)
library(yaml)
library(jsonlite)
library(magrittr)

source(file = 'R/values.R')
source(file = 'R/taskLog.R')
source(file = 'R/compObj.R')


a <- paramComput$new(parameter_list = list(id = 'TEST_COMP', mean = 1.0, sd = 1.0, trials =1000))
b <- paramComput$new(parameter_list = list(id = 'COMP', mean = 1, sd = 1, trials =1000))

a == b
a == a

(js  <- a$getJSONDef())
b$loadJSONDef(js)
(jsb  <- b$getJSONDef())
b$loadJSONDef(js)
b$getDefinition()
a == b
dput(a$values) 
dput(b$values)
all.equal(a$values, b$values)


class(a)
class
a

class(a)
x <- taskLog$new()
x$addEntry(id = "e2", name = "segunda", RobjectNames = c("uno", "dos"))
x$addEntry(id = "e3", name = "segunda", RobjectNames = c("uno", "dos"))
x$getLog()
jsonlite::toJSON(x = x$getLog(), pretty = TRUE) 
x$getLog()
x$log_tabular()
x$closeEntry('e3')


batch <- batchComp$new(a)
batch$addTask(name = 'task1', descr = 'uno')
batch$addTask(name = 'task2', descr = 'uno', notes = NULL)
(y  <- batch$export())

batch
x <- jsonlite::fromJSON(y)
eval(parse(text = x$parameters$eq_function))

batch$import(string = y)
batch
