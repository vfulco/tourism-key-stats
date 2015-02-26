library(knitr)
library(xtable)
project_dir <- getwd()


#===============Import and reshape data=======

# source("R/whatever.R")

MyCars <- mtcars[1:4 , 1:5]


#==============compile the latex document=============
setwd("knitr")

knit2pdf(input="keystats_onepager.Rnw", 
         compiler = 'xelatex', 
         quiet=TRUE, 
         clean = TRUE)

setwd(project_dir)