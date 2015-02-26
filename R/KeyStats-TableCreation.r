
# Preparation -------------------------------------------------------------
library(lubridate)
library(Defaults)
library(utils)
library(xtable)
library(data.table)
library(RODBC)
library(reshape)
library(mbie)
library(mbiedata)
library(mbieDBmisc)



# table1 ------------------------------------------------------------------



# ================================== Reporting Period Parameters ==================================

Report_start_date <- as.Date("2013-12-01")
Report_end_date <- as.Date("2014-11-30")

TRED_Conn <- odbcConnect("TRED_Prod")


# ================================== International Visitor Arrival Analysis ==================================
# --------- Key International Markets ---------

# iv_pc <- ImportTS(TRED_Conn, "Visitor arrivals by EVERY country of residence and purpose (Monthly)")
# save(iv_pc, file = "data/iv_pc.rda")
load("data/iv_pc.rda")

iv_pc_tot_1 <- subset(iv_pc, !(ClassificationValue %in% c("ASIA", "AMERICAS", "EUROPE", "OCEANIA", "TOTAL ALL COUNTRIES OF RESIDENCE")) & 
                        ClassificationValue.1 =="TOTAL ALL TRAVEL PURPOSES" & 
                        TimePeriod > Report_start_date & TimePeriod < Report_end_date + days(1))

iv_pc_tot_sum_1 <- aggregate(Value ~ ClassificationValue, data=iv_pc_tot_1, FUN= "sum")
iv_tot_1 <- sum(iv_pc_tot_sum_1$Value)
iv_pc_tot_sum_1 <- data.table(iv_pc_tot_sum_1[order(iv_pc_tot_sum_1$Value, decreasing = TRUE),])[1:6]

iv_pc_tot_0 <- subset(iv_pc, !(ClassificationValue %in% c("ASIA", "AMERICAS", "EUROPE", "OCEANIA", "TOTAL ALL COUNTRIES OF RESIDENCE", "NOT STATED")) & 
                        ClassificationValue.1 =="TOTAL ALL TRAVEL PURPOSES" & TimePeriod > Report_start_date - years(1) &
                        TimePeriod < Report_end_date - years(1) + days(1))

iv_pc_tot_sum_0 <- aggregate(Value ~ ClassificationValue, data=iv_pc_tot_0, FUN= "sum")
iv_pc_tot_sum <- merge(iv_pc_tot_sum_1, iv_pc_tot_sum_0, by = "ClassificationValue")
iv_pc_tot_sum <- iv_pc_tot_sum[order(iv_pc_tot_sum$Value.x, decreasing = TRUE), ]

iv_pc_tot_sum[ClassificationValue=="China, People's Republic of", ClassificationValue := "China"]
iv_pc_tot_sum[ClassificationValue=="United States of America", ClassificationValue := "USA"]
iv_pc_tot_sum[ClassificationValue=="United Kingdom", ClassificationValue := "UK"]

iv_pc_tot_sum <- iv_pc_tot_sum[, list ("Key International Markets" = iv_pc_tot_sum$ClassificationValue, 
                                       "Market Share" = percent(round(iv_pc_tot_sum$Value.x/iv_tot_1, digits =2)), 
                                       "No_of_Visit" =format(round(iv_pc_tot_sum$Value.x/1000, 0)*1000, big.mark = ","), 
                                       "Growth(pa)" = percent(round((iv_pc_tot_sum$Value.x-iv_pc_tot_sum$Value.y)/iv_pc_tot_sum$Value.y, digits=2)))]


  #       IVA_Sum_tbl <- xtable(iv_pc_tot_sum, hline.after=c(1), caption= NULL, digits = 0,label=NULL, floating=FALSE, latex.environments = "")
  #       align(IVA_Sum_tbl) <- "|l{0.5cm}|l{3.5cm}|r{1.5cm}||r{1.5cm}|r{1.5cm}|"
  #       align(IVA_Sum_tbl) <- "llrrr"
tab1 <- print(xtable(iv_pc_tot_sum, align = "|l|p{2.5cm}|p{1cm}|p{1.5cm}|p{1.5cm}|", 
             hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
      floating=FALSE, 
      include.rownames=FALSE)
tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",tab1, fixed=T)
tab1 <- gsub("\n\\end{tabular}","\n\\multicolumn{4}{p{6.5cm}}{{\\tiny Some notes on table};Some notes on table;Some notes on table;Some notes on table;Some notes on table;Some notes on table;Some notes on table;Some notes on table;}\\\\ \n\\end{tabular}",tab1, fixed=T)

sink("tables/table1.tex")
cat(tab1)
sink()