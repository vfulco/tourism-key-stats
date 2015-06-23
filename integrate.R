library(knitr)
library(xtable)
project_dir <- getwd()



#===============Import Stats tables and Plots =======

source("r/functions.R")

#===============Import and reshape data=======

source("R/KeyStats-TableCreation.r")

source("R/KTS_Plots.r")

#==============compile the latex document=============
setwd("knitr")

knit2pdf(input = "KeyStats.rnw", 
         output = paste0("KeyStats_", today(), ".tex"),
         compiler = 'xelatex', 
         quiet = TRUE, 
         clean = TRUE)

setwd(project_dir)
