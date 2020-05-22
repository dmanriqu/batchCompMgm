library(R6)
library(yaml)
library(jsonlite)
library(roxygen2)
source('R/values.R')
source(file = 'R/compObj.R')

roxygen2::roxygenize(package.dir = 'output')
new_trial <- specializeTrialMgmtClass( 
  title = 'Normal simulation',
  eq_fn = function(a,b){
    (a$values$sigma == b$values$sigma) &
    (a$values$sd    == b$values$sd   )
  },
  id=NULL, sigma=NULL, sd=NULL, notes=NULL
)

x <- new_trial$new()
y <- new_trial$new()
x$load_values(id = 'll', sigma = 1, sd = 2, notes ='hola')
y$load_values(id = 'll', sigma = 1, sd = 2, notes ='hola')

(com <- compObj$new(x))
com
com$status
com$log
com$addEvent('cmp1', 'Calculate 1')
com$addEvent('cmp2', 'Calculate 2')
com$addEvent(descr = 'Calculate 3')
com$addEvent(id = 'EV3', descr = 'Calculate 3')
com$log
com
com$log = 3
com$finish()
