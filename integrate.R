library(knitr)
library(xtable)
project_dir <- getwd()


<<<<<<< HEAD
#===============Import Stats tables and Plots =======
=======
source("r/functions.R")

#===============Import and reshape data=======
>>>>>>> 0eb56673b1797b3009b5fce18f6083f76c0eb04a

source("R/KeyStats-TableCreation.r")

source("R/KTS_Plots.r")

#==============compile the latex document=============
setwd("knitr")

knit2pdf(input="KeyStats.rnw", 
         compiler = 'xelatex', 
         quiet=TRUE, 
         clean = TRUE)

setwd(project_dir)