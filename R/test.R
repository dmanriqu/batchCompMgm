#test of library. Build and instal library with make all install
library(R6)
library(yaml)
library(jsonlite)
library(magrittr)

source('R/values.R')
source(file = 'R/compObj.R')
#library(trialMgmnt, lib.loc="cache/")
normalParameters <-  createParamComputClass(classname = 'normalSimulation', title = 'Normal Simulation Parameters',
mean = 1, sd = 1, trials =1)
a <- normalParameters$new()
b <- normalParameters$new()

(a$load_values(mean = 1, sd = 2, trials = 1000))
a$print()

batch <- batchComp$new(a)
batch$log
batch$addTask(name = 'task1', descr = 'uno')
batch$addTask(name = 'task2', descr = 'uno', notes = NULL)
batch$completeTask('TSK2')
batch$log
x <- batch$export()
batch$import(string = x)
