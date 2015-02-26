setwd("P:r/ellisp/latex examples/nested tables")
source(".rprofile")

library(knitr)
library(xtable)

cars_table <- mtcars[1:5, 1:5]

knit2pdf("tablesEG.rnw")