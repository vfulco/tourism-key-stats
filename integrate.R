library(knitr)

project_dir <- getwd()

setwd("knitr")

knit2pdf(input="keystats_onepager.Rnw", 
         compiler = 'xelatex', 
         quiet=TRUE, 
         clean = TRUE)

setwd(project_dir)