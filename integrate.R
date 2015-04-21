library(knitr)
library(xtable)
project_dir <- getwd()


source("r/functions.R")

#===============Import and reshape data=======

source("R/KeyStats-TableCreation.r")

#==============compile the latex document=============
setwd("knitr")

knit2pdf(input="KeyStats.rnw", 
         compiler = 'xelatex', 
         quiet=TRUE, 
         clean = TRUE)

setwd(project_dir)