
.libPaths("P:/R/libraries/3.1.2")

# Preparation -------------------------------------------------------------
library(lubridate)
library(Defaults)
library(utils)
library(xtable)
library(data.table)
library(RODBC)
library(reshape)
library(stringr)
library(mbie)
library(mbiedata)
library(mbieDBmisc)



# ====================================create summary tables ============================================================

# ================================== Reporting Period Parameters ==================================

Report_start_date <- as.Date("2013-12-01")
Report_end_date <- as.Date("2014-11-30")

TRED_Conn <- odbcConnect("TRED_Prod")


# ================================== International Visitor Arrival Analysis ==================================

# --------- Key International Markets ---------

iv_pc <- ImportTS(TRED_Conn, "Visitor arrivals by EVERY country of residence and purpose (Monthly)")
save(iv_pc, file = "data/iv_pc.rda")
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

Market_share <- percent(round(sum(iv_pc_tot_sum$Value.x)/iv_tot_1,digits =2))


iv_pc_tot_sum[ClassificationValue=="China, People's Republic of", ClassificationValue := "China"]
iv_pc_tot_sum[ClassificationValue=="United States of America", ClassificationValue := "USA"]
iv_pc_tot_sum[ClassificationValue=="United Kingdom", ClassificationValue := "UK"]

iv_pc_tot_sum <- iv_pc_tot_sum[, list ("Key International Markets" = iv_pc_tot_sum$ClassificationValue, 
                                       "Market %" = percent(round(iv_pc_tot_sum$Value.x/iv_tot_1, digits =2)), 
                                       "No_Visit" =format(round(iv_pc_tot_sum$Value.x/1000, 0)*1000, big.mark = ","), 
                                       "Growth(pa)" = percent(round((iv_pc_tot_sum$Value.x-iv_pc_tot_sum$Value.y)/iv_pc_tot_sum$Value.y, digits=2)))]

IVA_tab1 <- print(xtable(iv_pc_tot_sum, align = "|l|p{3cm}|p{1.3cm}|p{1.25cm}|p{1.3cm}|",
             hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                  floating=FALSE, 
                  include.rownames=FALSE,
#                   scalebox ="0.8"
                  )
IVA_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",IVA_tab1, fixed=T)
# IVA_tab1 <- gsub("\n\\end{tabular}","\n\\multicolumn{4}{p{7cm}}{Combined, these markets provided $\Sexpr{Market_share}$ of international visitors to New Zealand for the year ended December 2014}\\\\ \n\\end{tabular}",IVA_tab1, fixed=T)

# IVA_tab1 <- gsub("\n\\end{tabular}","\n\\multicolumn{4}{p{6.5cm}}{{\\tiny Some notes on table};Some notes on table;Some notes on table;Some notes on table;}\\\\ \n\\end{tabular}",IVA_tab1, fixed=T)


sink("tables/IVA_tab1.tex")
cat(IVA_tab1)
sink()

note_1 <- paste("Combined, these markets provided", Market_share, "of international visitors to New Zealand for the year ended December 2014.")

note_1 <- gsub("%"," percent",note_1)
sink("outputs/note_1.txt")
cat(note_1)
sink()




#----------------------------- International Visitor Arrival Purpose of Visiting ---------------------------

iv <- ImportTS(TRED_Conn, "Visitor arrivals by country of residence, purpose and length of stay (Monthly)")

save(iv, file = "data/iv_pov.rda")
load("data/iv_pov.rda")

iv_pov_1 <- subset(iv, ClassificationValue.2 %in% c("TOTAL ALL LENGTHS OF STAY") & 
                     TimePeriod > Report_start_date & TimePeriod < Report_end_date + days(1) & 
                     ClassificationValue.1 %in% c("Business", "Holiday/Vacation", "Visit Friends/Relatives"))

iv_pov_sum_1 <- aggregate(Value ~ ClassificationValue.1, data=iv_pov_1, FUN= "sum")
iv_pov_0 <- subset(iv, ClassificationValue.2 %in% c("TOTAL ALL LENGTHS OF STAY") & 
                     TimePeriod > Report_start_date - years(1) & TimePeriod < Report_end_date - years(1) + days(1) & 
                     ClassificationValue.1 %in% c("Business", "Holiday/Vacation", "Visit Friends/Relatives"))

iv_pov_sum_0 <- aggregate(Value ~ ClassificationValue.1, data=iv_pov_0, FUN= "sum")
iv_pov_tot_sum <- merge(iv_pov_sum_1, iv_pov_sum_0, by = "ClassificationValue.1")
iv_pov_tot_sum <- iv_pov_tot_sum[order(iv_pov_tot_sum $Value.x, decreasing = TRUE), ]
iv_pov_tot_sum <- data.table(iv_pov_tot_sum)

iv_pov_tot_sum <- iv_pov_tot_sum[, list ("Purpose of Visiting" = iv_pov_tot_sum$ClassificationValue.1,
                                         "No_Nights" = format(round(iv_pov_tot_sum$Value.x/1000, 0)*1000, big.mark = ","), 
                                         "Growth(pa)" = percent(round((iv_pov_tot_sum$Value.x-iv_pov_tot_sum$Value.y)/iv_pov_tot_sum$Value.y, digits=3)))]

IVA_tab2 <- print(xtable(iv_pov_tot_sum, align = "lp{4.7cm}p{1.25cm}p{1.3cm}",
                         hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                  floating=FALSE, 
                  include.rownames=FALSE,
#                   scalebox ="0.8"
                  )
IVA_tab2 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",IVA_tab2, fixed=T)
# IVA_tab2 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{7.5cm}}{{\\tiny Some notes on table};Some notes on table;Some notes on table;}\\\\ \n\\end{tabular}",IVA_tab2, fixed=T)

sink("tables/IVA_tab2.tex")
cat(IVA_tab2)
sink()


# -------------------------- International Visitor Arrival Calculate LOS -----------------------------


iv_los_1 <- subset(iv, ClassificationValue.2 %in% c("Total stay days", "TOTAL ALL LENGTHS OF STAY") & 
                     TimePeriod > Report_start_date & TimePeriod < Report_end_date + days(1) & 
                     ClassificationValue.1 %in% c("TOTAL ALL TRAVEL PURPOSES"))

iv_los_sum_1 <- aggregate(Value ~ ClassificationValue.2, data=iv_los_1, FUN= "sum")
iv_los_1 <- dcast(iv_los_1, TimePeriod ~ ClassificationValue.2, sum, value.var="Value")
iv_los_1 <- rename(iv_los_1, c("TOTAL ALL LENGTHS OF STAY" = "Tot_LOS" , "Total stay days" ="Tot_day"))
iv_los_sum_1 <- sum(iv_los_1$Tot_day)/sum(iv_los_1$Tot_LOS)
iv_los_0 <- subset(iv, ClassificationValue.2 %in% c("Total stay days", "TOTAL ALL LENGTHS OF STAY") & 
                     TimePeriod > Report_start_date - years(1) & TimePeriod < Report_end_date - years(1) + days(1) & 
                     ClassificationValue.1 %in% c("TOTAL ALL TRAVEL PURPOSES"))

iv_los_sum_0 <- aggregate(Value ~ ClassificationValue.2, data=iv_los_0, FUN= "sum")
iv_los_0 <- dcast(iv_los_0, TimePeriod ~ ClassificationValue.2, sum, value.var="Value")
iv_los_0 <- rename(iv_los_0, c("TOTAL ALL LENGTHS OF STAY" = "Tot_LOS", "Total stay days" ="Tot_day"))

iv_los_sum_0 <- sum(iv_los_0$Tot_day)/sum(iv_los_0$Tot_LOS)
los_growth <- (iv_los_sum_1-iv_los_sum_0)/iv_los_sum_0



LOS <-data.frame('Average Intended length of stay:', iv_los_sum_1, percent(round(los_growth, digits=3)))

# \begin{tabular}{ |l|p{4cm}|p{1.5cm}|p{1.5cm}| }
# "Average Intended length of stay:" & iv_los_sum_1 & los_growth \\ 
# \end{tabular}

IVA_los <- print(xtable(LOS, align = "|l|p{4.7cm}|p{1.25cm}|p{1.3cm}|",
                         hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                  floating=FALSE, 
                  include.rownames=FALSE,
                  include.colnames =FALSE                 
                  )
IVA_los <- gsub("\\begin{tabular}","\\begin{tabular}[t]",IVA_los, fixed=T)
# IVA_tab2 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{6.5cm}}{{\\tiny Some notes on table};Some notes on table;Some notes on table;Some notes on table;}\\\\ \n\\end{tabular}",IVA_tab2, fixed=T)

sink("tables/IVA_los.tex")
cat(IVA_los)
sink()






# ================================== International Visitor Expenditure ==================================

# SQL_query <- "SELECT * FROM Production.vw_IVSSurveyMainHeader where TotalSpendInNZ = 0 or TotalSpendInNZ is null"
# IVS_main <- sqlQuery(TRED_Conn, SQL_query)
# IVS_main <- IVS_main[, list(Tot_Exp = sum(TotalSpendInNZ))]

# --------- calculate the total expenditure ---------

qry_exp_tot <- "SELECT '$'+convert(varchar(50),round((sum(TotalSpendInNZ*SpendSmoothingFactor*PopulationWeight)/1000000000), 2)) as total_sp,
'$'+convert(varchar(50),round((sum(TotalSpendInNZ*SpendSmoothingFactor*PopulationWeight)/sum(PopulationWeight)), 0)) as avg_sp 
FROM Production.IVSSurveyMainHeader WHERE Qtr in ( '2013 4','2014 1','2014 2','2014 3')"

IVS_exp <- sqlQuery(TRED_Conn, qry_exp_tot)
save(IVS_exp, file = "data/IVS_exp.rda")
load("data/IVS_exp.rda")

# --------- calculate the annual expenditure for the top market ---------

qry_exp_top <- "SELECT top 5 aa.country, aa.Annual_Spending_1, bb.Annual_Spending_0 FROM

(
SELECT CORNextYr as country, round(sum(TotalSpendInNZ*SpendSmoothingFactor*PopulationWeight)/1000000, 0) as Annual_Spending_1
FROM Production.IVSSurveyMainHeader
WHERE Qtr in ( '2013 4','2014 1','2014 2','2014 3')
Group by CORNextYr
) aa

LEFT JOIN

(
SELECT CORNextYr as country, round(sum(TotalSpendInNZ*SpendSmoothingFactor*PopulationWeight)/1000000, 0) as Annual_Spending_0
FROM Production.IVSSurveyMainHeader
WHERE Qtr in ( '2012 4','2013 1','2013 2','2013 3')
Group by CORNextYr

) bb

on aa.country = bb.country
order by aa.Annual_Spending_1 desc"

exp_top <- sqlQuery(TRED_Conn, qry_exp_top)
exp_top <- data.table(exp_top)

exp_top[country=="China, People's Republic of", country := "China"]
exp_top[country=="United States of America", country := "USA"]
exp_top[country=="United Kingdom", country := "UK"]


exp_top_sum <- exp_top[, list("Key International Markets" = exp_top$country,
                              "Current_Yr ($mil)" = format(exp_top$Annual_Spending_1,big.mark = ","),
                              "Growth(pa)" = percent(round((exp_top$Annual_Spending_1-exp_top$Annual_Spending_0)/exp_top$Annual_Spending_0, digits=2)))]



IVE_tab1 <- print(xtable(exp_top_sum, align = "|l|p{4cm}|p{2cm}|p{1.3cm}|", 
                         hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                  floating=FALSE, 
                  include.rownames=FALSE)
IVE_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",IVE_tab1, fixed=T)
# IVE_tab1 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{6.5cm}}{{\\tiny Some notes on table};Some notes on table;\\Some notes on table;}\\\\ \n\\end{tabular}",IVE_tab1, fixed=T)

sink("tables/IVE_tab1.tex")
cat(IVE_tab1)
sink()

# --------- calculate total expenditure by purpose of visit ---------

sql_pov_1 <- "SELECT bb.POV, bb.total_sp as total_sp_curr FROM
(
SELECT top 3 POV, round(sum(TotalSpendInNZ*SpendSmoothingFactor*PopulationWeight)/1000000, 0) as total_sp
FROM [TRED].[Production].[IVSSurveyMainHeader]
WHERE (POV like '%Holiday%' or POV like '%Visiting Friends%Relatives%' or POV = 'Business') and Qtr in ( '2013 4','2014 1','2014 2','2014 3')
group by POV
order by total_sp desc
) bb "

pov_1 <- sqlQuery(TRED_Conn, sql_pov_1)

sql_pov_0 <- "SELECT bb.POV, bb.total_sp as total_sp_prev FROM
(
SELECT top 3 POV, round(sum(TotalSpendInNZ*SpendSmoothingFactor*PopulationWeight)/1000000, 0) as total_sp
FROM [TRED].[Production].[IVSSurveyMainHeader]
WHERE (POV like '%Holiday%' or POV like '%Visiting Friends%Relatives%' or POV = 'Business') and Qtr in ( '2012 4','2013 1','2013 2','2013 3')
group by POV
order by total_sp desc
) bb "

pov_0 <- sqlQuery(TRED_Conn, sql_pov_0)

ive_pov_sum <- merge(pov_1, pov_0)
ive_pov_sum <- data.table(ive_pov_sum)
ive_pov_sum <- ive_pov_sum[, list("Total Spend by Purpose of Visit" = ive_pov_sum$POV,
                                "Current Year" = format(ive_pov_sum$total_sp_curr, big.mark = ","),
                                "Growth(pa)" = percent(round((ive_pov_sum$total_sp_curr -ive_pov_sum$total_sp_prev)/ive_pov_sum$total_sp_prev, digits= 2)))]

ive_pov_sum <- ive_pov_sum[order(ive_pov_sum$"Current Year", decreasing = TRUE), ]

IVE_tab3 <- print(xtable(ive_pov_sum, align = "|l|p{4cm}|p{2cm}|p{1.2cm}|", 
                         hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                  floating=FALSE, 
                  include.rownames=FALSE)
IVE_tab3 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",IVE_tab3, fixed=T)
IVE_tab3 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{8.25cm}}{$^*$Excludes international airfares and individuals whose purpose of visit to New Zealand was to attend a recognised educational institute, and are foreign-fee paying students.}\\\\ \n\\end{tabular}",IVE_tab3, fixed=T)

sink("tables/IVE_tab3.tex")
cat(IVE_tab3)
sink()





# ================================== Trip aboard by NZers ==================================


NZ_out <- ImportTS(TRED_Conn, "Short-term NZ traveller departures by EVERY country of main dest and purpose (Monthly)")
NZ_out <- rename(NZ_out, c("ClassificationValue" = "Country", "ClassificationValue.1" = "Trip_Type"))
save(iv, file = "data/NZ_out.rda")
load("data/NZ_out.rda")

NZ_out_1 <- subset(NZ_out, !(Country %in% c("ASIA", "AMERICAS", "EUROPE", "OCEANIA", "TOTAL ALL COUNTRIES OF MAIN DESTINATION")) &
                     Trip_Type == "TOTAL ALL TRAVEL PURPOSES" & TimePeriod > Report_start_date & TimePeriod < Report_end_date )
NZ_out_0 <- subset(NZ_out, !(Country %in% c("ASIA", "AMERICAS", "EUROPE", "OCEANIA", "TOTAL ALL COUNTRIES OF MAIN DESTINATION")) &
                     Trip_Type == "TOTAL ALL TRAVEL PURPOSES" & TimePeriod > (Report_start_date-years(1)) & TimePeriod < (Report_end_date - years(1)) )

# --- sumary the total visit by country

NZ_out_total_1 <- aggregate(Value ~ Country, data=NZ_out_1, FUN=sum)

NZ_out_total_0 <- aggregate(Value ~ Country, data=NZ_out_0, FUN=sum)

NZ_out_total <- merge(NZ_out_total_1, NZ_out_total_0, by = "Country")


NZ_out_total <- NZ_out_total[, list (
                                   "Total Departures" =format(sum(NZ_out_total$Value.x), big.mark = ","),
                                   "Growth(pa)" = percent(round((sum(NZ_out_total$Value.x)-sum(NZ_out_total$Value.y))/sum(NZ_out_total$Value.y), digits=3)))]

Annual_total_outbound <- c('Annual Outbound Departures', format(sum(NZ_out_total$Value.x), big.mark = ","),
                           percent(round((sum(NZ_out_total$Value.x)-sum(NZ_out_total$Value.y))/sum(NZ_out_total$Value.y), digits=3)))

Annual_total_outbound <- t(data.table(Annual_total_outbound))

Annual_total_outbound <- data.table(Annual_total_outbound)

Annual_total_outbound <- rename(Annual_total_outbound, c("V1"= " ", "V2" =" ", "V3" = "Growth(pa)"))


NZ_out_tab1 <- print(xtable(Annual_total_outbound, align = "|l|p{4.7cm}|p{1.25cm}|p{1.3cm}|", 
                            hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                     floating=FALSE, 
                     include.rownames=FALSE)
NZ_out_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",NZ_out_tab1, fixed=T)

sink("tables/NZ_out_tab1.tex")
cat(NZ_out_tab1)
sink()


# Annual_total_outbound <- sum(NZ_out_sum_1$Value)
                      

NZ_out_sum_1 <- data.table(NZ_out_sum_1[order(NZ_out_sum_1$Value, decreasing = TRUE), ])[1:5]
NZ_out_sum_0 <- aggregate(Value ~ Country, data=NZ_out_0, FUN=sum)
NZ_out_sum_f <- merge(NZ_out_sum_1, NZ_out_sum_0, by = "Country")
NZ_out_sum_f <- NZ_out_sum_f[order(NZ_out_sum_f$Value.x, decreasing = TRUE), ]

NZ_out_sum_f[Country=="China, People's Republic of", Country := "China"]
NZ_out_sum_f[Country=="United States of America", Country := "USA"]
NZ_out_sum_f[Country=="United Kingdom", Country := "UK"]


NZ_out_sum <- NZ_out_sum_f[, list ("Countries Visited by NZers" = NZ_out_sum_f$Country, 
                                   "No_Visit" =format(NZ_out_sum_f$Value.x, big.mark = ","),
                                   "Growth(pa)" = percent(round((NZ_out_sum_f$Value.x-NZ_out_sum_f$Value.y)/NZ_out_sum_f$Value.y, digits=3)))]

NZ_out_tab2 <- print(xtable(NZ_out_sum, align = "|l|p{4.7cm}|p{1.25cm}|p{1.3cm}|", 
                         hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                  floating=FALSE, 
                  include.rownames=FALSE)
NZ_out_tab2 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",NZ_out_tab2, fixed=T)
# NZ_out_tab2 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{6.5cm}}{{\\tiny Some notes on table};Some notes on table;\\Some notes on table;}\\\\ \n\\end{tabular}",NZ_out_tab2, fixed=T)

sink("tables/NZ_out_tab2.tex")
cat(NZ_out_tab2)
sink()


# ================================== Tourism Forecast ==================================

## This is the path of forecast data: Enterprise  Policy Advice, Regulation...  Tourism Research  Forecasts  Dissemination  2014 

Fcst_query <- "SELECT * FROM Production.NZTFSurveyMainHeader"

Fcst <- sqlQuery(TRED_Conn, Fcst_query)

Fcst_year <- as.numeric(max(Fcst$ForecastYear))
End_year <- as.numeric(max(Fcst$Year))

Fcst_sum <- data.table(subset(Fcst, ForecastYear == Fcst_year & (Year == 2020 | Year == Fcst_year-1)))

Fcst_sum_1 <- aggregate(cbind(TotalVisitorArrivals, TotalVisitorDays, TotalVisitorSpend) ~ Year, data=Fcst_sum, FUN=sum)




Fcst_sum_2 <- t(Fcst_sum_1)
Fcst_sum_2_new <- data.table(Fcst_sum_2)
Fcst_sum_2_rpt <- Fcst_sum_2_new[, list("Visitor Arrivals" = format(Fcst_sum_2_new$V2, big.mark = ","),
                                    "growth_rate" = CAGR((Fcst_sum_2_new$V2-Fcst_sum_2_new$V1)/Fcst_sum_2_new$V1, 
                                                         (End_year-Fcst_year+1)))]
                                    
Fcst_sum_rpt = Fcst_sum_2_rpt[-1, ]

Fcst_sum_rpt <- Fcst_sum_rpt[, list("Visitor Arrivals" = "",
                                    "Forecast(2020)" = format(Fcst_sum_rpt$"Visitor Arrivals", big.mark = ","),
                                    "CAGR" = Fcst_sum_rpt$growth_rate)]

Fcst_tab1 <- print(xtable(Fcst_sum_rpt, align = "|l|p{4cm}|p{2cm}|p{1.25cm}|", 
                            hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                     floating=FALSE, 
                     include.rownames=FALSE)
Fcst_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Fcst_tab1, fixed=T)

sink("tables/Fcst_tab1.tex")
cat(Fcst_tab1)
sink()



# # Fcst_sum_3 <- data.table(t(Fcst_sum_2))
# 
# Fcst_sum_new = Fcst_sum_3[-1, ]
# Fcst_sum_1 <- Fcst_sum_1[, list(Fcst_sum_1$TotalVisitorArrivals,  Fcst_sum_1$TotalVisitorDays,  Fcst_sum_1$TotalVisitorSpend)]
# Fcst_sum_1 <- Fcst_sum_1[, list(TotalVisitorArrivals,  TotalVisitorDays,  TotalVisitorSpend)]

# Here need some more works

Fcst_sum_3 <- Fcst_sum_2[, list(]

# Fcst_sum_3 <- subset(Fcst_sum_2, row.names not in c("Year"))

Fcst_sum_2_rpt <- Fcst_sum_1[, list("Visitor Arrivals" = Fcst_sum_2$row.names, 
                                    "Visitor Arrivals" = Fcst_sum_2$V2, 
                                    "growth_rate" = CAGR((Fcst_sum_1$V2-Fcst_sum_1$V1)/Fcst_sum_1$V1, (as.numeric(max(Fcst$Year))-as.numeric(max(Fcst$ForecastYear))+1)),
                                    "Visit Nights" = Fcst_sum_1$TotalVisitorDays, 
                                    "growth_rate" = CAGR((Fcst_sum_1$V2-Fcst_sum_1$V1)/Fcst_sum_1$V1, (as.numeric(max(Fcst$Year))-as.numeric(max(Fcst$ForecastYear))+1)),
                                    "Expenditure($Mil)" = Fcst_sum_1$V2, 
                                    "growth_rate" = CAGR((Fcst_sum_1$V2-Fcst_sum_1$V1)/Fcst_sum_1$V1, (as.numeric(max(Fcst$Year))-as.numeric(max(Fcst$ForecastYear))+1))
                                    )]

# 
# Fcst_sum_1 <- aggregate(list(TotalVisitorArrivals, TotalVisitorDays) ~ Fcst_sum$Year, data=Fcst_sum, FUN=sum)
# Fcst_sum_1 <- aggregate(Fcst_sum['TotalVisitorArrivals', 'TotalVisitorDays'], by=Fcst_sum[c("Year")], FUN=sum)




# --------- Key International Market Forecast ---------

Fcst_sum_key <- subset(Fcst_sum, Year == 2020 & Country != "Other")
Fcst_sum_key <- Fcst_sum_key[, list("Key International Markets"= Fcst_sum_key$Country,
                                    "Visitors" = format(round(Fcst_sum_key$TotalVisitorArrivals/1000, 0)*1000, big.mark = ","),
                                    "Spend($Mil)" = format(Fcst_sum_key$TotalVisitorSpend, big.mark = ","))]

Fcst_sum_key <- Fcst_sum_key[order(Fcst_sum_key$Visitors, decreasing = TRUE), ][1:5]

Fcst_tab2 <- print(xtable(Fcst_sum_key, align = "|l|p{4cm}|p{2cm}|p{1.3cm}|", 
                         hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                  floating=FALSE, 
                  include.rownames=FALSE)
Fcst_tab2 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Fcst_tab2, fixed=T)
# Fcst_tab2 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{6.5cm}}{{\\tiny Some notes on table};Some notes on table;Some notes on table;}\\\\ \n\\end{tabular}",Fcst_tab2, fixed=T)

sink("tables/Fcst_tab2.tex")
cat(Fcst_tab2)
sink()

# ================================== Economic Contribution ==================================

Annual_Report_date <- as.Date("2013-03-31")

# --------- Tourism Export ---------

EC_exp <- ImportTS(TRED_Conn, "Summary of Tourism Expenditure by type of tourist (ANZSIC06) (Annual-Mar)")

EC_exp_sum <- subset(EC_exp, TimePeriod == max(EC_exp$TimePeriod))


EC_exp_sum <-data.table(EC_exp_sum)

# calculate the percentage of internations tourism expenditure

EC_exp_sum_1 <- subset(EC_exp_sum, ClassificationValue %in% c('International tourism as a percentage of total exports', select=c(3)))
EC_exp_sum_1 <- data.table(EC_exp_sum_1)                             
EC_exp_share <- EC_exp_sum_1[, "Value", with= FALSE]                    

# inter_emp2 <- EC_emp_sum_rpt[, "Value", with= FALSE][2]
# inter_emp2<- format(inter_emp2, big.mark = ",")

EC_exp_total <- subset(EC_exp_sum, ClassificationValue %in% c('Domestic tourism expenditure', 
                                                              'International tourism expenditure', 'Total tourism expenditure'), select=c(2, 3))

EC_exp_total <- EC_exp_total[order(EC_exp_total$Value),]

EC_exp_total_v <- cbind(EC_exp_total,Class = 
                          ifelse(EC_exp_total$ClassificationValue=="Domestic tourism expenditure","Domestic",
                                 
                                 ifelse(EC_exp_total$ClassificationValue=="International tourism expenditure","international", "Total"))) 

EC_exp_growth <- subset(EC_exp_sum, ClassificationValue %in% c('Annual percentage change in international tourism expenditure', 
                                                               'Annual percentage change in domestic tourism expenditure',
                                                               'Annual percentage change in total tourism expenditure'), select=c(2, 3))

EC_exp_growth_v <- cbind(EC_exp_growth,Class = 
                           ifelse(EC_exp_growth$ClassificationValue=="Annual percentage change in domestic tourism expenditure","Domestic",
                                  
                                  ifelse(EC_exp_growth$ClassificationValue=="Annual percentage change in international tourism expenditure","international", "Total"))) 

EC_exp_table <-merge(EC_exp_total_v, EC_exp_growth_v, by = "Class")


EC_exp_table_sum <- subset(EC_exp_table, select = c(Class, Value.x, Value.y))
EC_exp_table_sum <- EC_exp_table_sum[order(EC_exp_table_sum$Value.x),]

EC_exp_table_sum <- EC_exp_table_sum[, list("Tourism Market"= EC_exp_table_sum$Class,
                                            "Expenditure" = format(round(EC_exp_table_sum$Value.x/1000, 1), big.mark = ","),
                                            "Growth(pa)" = percent(EC_exp_table_sum$Value.y/100))]


EC_exp_table_sum <- data.table(EC_exp_table_sum, key ="Tourism Market")
EC_exp_table_sum <- data.table(EC_exp_table_sum[order(EC_exp_table_sum$Expenditure, decreasing = FALSE),])

inter_exp <- EC_exp_table_sum[, "Expenditure", with= FALSE][1]

EC_exp_tab1 <- print(xtable(EC_exp_table_sum, align = "|l|p{4.75cm}|p{1.25cm}|p{1.25cm}|", 
                          hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                   floating=FALSE, 
                   include.rownames=FALSE)

EC_exp_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",EC_exp_tab1, fixed=T)
EC_exp_tab1 <- gsub("international","international$^+$",EC_exp_tab1, fixed=T)
EC_exp_tab1 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{7.5cm}}{$^+$Includes international airfares paid to New Zealand carriers.}\\\\ \n\\end{tabular}",EC_exp_tab1, fixed=T)

sink("tables/EC_exp_tab1.tex")
cat(EC_exp_tab1)
sink()


EC_comment_1 <- paste("International tourist expenditure accounted for ", inter_exp, "billion or ", EC_exp_share, "percent of New Zealand total export earnings.")

# EC_comment_1 <- gsub(" %","$%$",EC_comment_1, fixed=T)
# EC_comment_1 <- gsub("accounted for ","accounted for \ $",EC_comment_1, fixed=T)

sink("outputs/EC_comment_1.txt")
cat(EC_comment_1)
sink()



# --------- Tourism GDP Contribution ---------

EC_GDP <- ImportTS(TRED_Conn, "Tourism expenditure by component (ANZSIC06) (Annual-Mar)")
EC_GDP_sum <- subset(EC_GDP, TimePeriod == max(EC_exp$TimePeriod))

EC_GDP_sum_rpt <- subset(EC_GDP_sum, ClassificationValue %in% c('Direct tourism value added', 
                                                                'Direct tourism value added as a percentage of total industry contribution to GDP'),
                         select=c(2, 3))

EC_GDP_sum_rpt_1 <- subset(EC_GDP_sum, ClassificationValue %in% c('Indirect tourism value added', 
                                                                'Indirect tourism value added as a percentage of total industry contribution to GDP'),
                         select=c(2, 3))


EC_GDP_sum_rpt <- data.table(EC_GDP_sum_rpt)
inter_gdp1 <- EC_GDP_sum_rpt[, "Value", with= FALSE][1]

inter_gdp1 <- round((inter_gdp1$Value/1000), 1)
inter_gdp2 <- EC_GDP_sum_rpt[, "Value", with= FALSE][2]
# inter_gdp2 <- percent((inter_gdp2$Value/100))
inter_gdp2 <- inter_gdp2$Value

EC_GDP_sum_rpt_1 <- data.table(EC_GDP_sum_rpt_1)
inter_gdp3 <- EC_GDP_sum_rpt_1[, "Value", with= FALSE][1]
inter_gdp3<- round((inter_gdp3$Value/1000), 1)

inter_gdp4 <- EC_GDP_sum_rpt[, "Value", with= FALSE][2]
inter_gdp4 <- percent(inter_gdp4$Value/100)

EC_comment_2 <- paste("Tourism directly contributes",inter_gdp1, "billion dollar (or", inter_gdp2,
                      "percent) to New Zealand total GDP. A further", inter_gdp3, "billion (or", inter_gdp3 ,
                      "percent) is indirectly contributed. When comparing tourism to other industries, the direct contribution should be used.")

sink("outputs/EC_comment_2.txt")
cat(EC_comment_2)
sink() 


# --------- Tourism Employment ---------

EC_emp <- ImportTS(TRED_Conn, "Summary of Tourism Employment (ANZSIC06) (Annual-Mar)")

EC_emp_sum <- subset(EC_emp, TimePeriod == max(EC_exp$TimePeriod) )

EC_emp_sum_rpt <- subset(EC_emp_sum, ClassificationValue %in% c('FTE persons directly employed in tourism as a percentage of total employment in New Zealand', 
                                                                'FTE persons directly employed in tourism in New Zealand'), select=c(2, 3))

EC_emp_sum_rpt <- data.table(EC_emp_sum_rpt)
inter_emp1 <- EC_emp_sum_rpt[, "Value", with= FALSE][1]

inter_emp2 <- EC_emp_sum_rpt[, "Value", with= FALSE][2]
inter_emp2<- format(inter_emp2, big.mark = ",")

EC_comment_3 <- paste("Tourism directly supports",inter_emp2, "full-time equivalent jobs (", inter_emp1,
                      "percent of the total workforce in New Zealand).")

sink("outputs/EC_comment_3.txt")
cat(EC_comment_3)
sink() 


# ================================== Commercial Accommodation Stats ==================================

ACCOM <- ImportTS(TRED_Conn, "Actual by Accommodation by Type by Variable (Monthly)")
ACCOM<- data.table(ACCOM)

# --------- Guest Nights Summary ---------

ACCOM_1 <- subset(ACCOM, ClassificationValue %in% c('Holiday parks', 'Backpackers', 'Motels', 'Hotels', 'Total') & 
                    TimePeriod > Report_start_date & TimePeriod < Report_end_date )

ACCOM_0 <- subset(ACCOM, ClassificationValue %in% c('Holiday parks', 'Backpackers', 'Motels', 'Hotels', 'Total') & 
                    TimePeriod > (Report_start_date-years(1)) & TimePeriod < (Report_end_date - years(1)) )

ACCOM_type_1 <- subset(ACCOM_1, ClassificationValue.1 %in% c('Number of guest nights'))
ACCOM_type_0 <- subset(ACCOM_0, ClassificationValue.1 %in% c('Number of guest nights'))
ACCOM_type_sum_1 <- aggregate(Value ~ ClassificationValue, data=ACCOM_type_1, FUN=sum)
ACCOM_type_sum_0 <- aggregate(Value ~ ClassificationValue, data=ACCOM_type_0, FUN=sum)
ACCOM_type_sum_1 <- ACCOM_type_sum_1[order(ACCOM_type_sum_1$Value, decreasing = TRUE), ]
ACCOM_type_sum <- merge(ACCOM_type_sum_1, ACCOM_type_sum_0, by = "ClassificationValue")
ACCOM_type_sum <- data.table(ACCOM_type_sum[order(ACCOM_type_sum$Value.x, decreasing = TRUE), ])

ACCOM_type_sum <- ACCOM_type_sum[, list ("Accommodation Type" = ACCOM_type_sum$ClassificationValue, 
                                         "No_Nights" =format(ACCOM_type_sum$Value.x, big.mark = ","),
                                         "Growth(pa)" = percent(round((ACCOM_type_sum$Value.x-ACCOM_type_sum$Value.y)/ACCOM_type_sum$Value.y, digits=3)))]

ACCOM_type_sum <- data.table(ACCOM_type_sum[order(ACCOM_type_sum$"Accommodation Type", decreasing = FALSE), ])


Accom_tab1 <- print(xtable(ACCOM_type_sum, align = "|l|p{4.7cm}|p{1.25cm}|p{1.3cm}|", 
                            hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                     floating=FALSE, 
                     include.rownames=FALSE)
Accom_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Accom_tab1, fixed=T)
# Accom_tab1 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{6.5cm}}{{\\tiny Some notes on table};Some notes on table;Some notes on table;}\\\\ \n\\end{tabular}",Accom_tab1, fixed=T)

sink("tables/Accom_tab1.tex")
cat(Accom_tab1)
sink()

# --------- Occupancy Rates Summary ---------

ACCOM_p <- subset(ACCOM, ClassificationValue %in% c('Holiday parks', 'Backpackers', 'Motels', 'Hotels', 'Total'), select=c(1,2,3,4), )
ACCOM_p <- rename(ACCOM_p, c("ClassificationValue" = "Accom_Type", "ClassificationValue.1" = "Class", "Value" = "Num_of_Stay"))
ACCOM_p <- subset(ACCOM_p, Class %in% c('Occupancy rate (percent)'))

ACCOM_p_1 <- subset(ACCOM_p, TimePeriod > Report_start_date & TimePeriod < Report_end_date )
ACCOM_p_0 <- subset(ACCOM_p, TimePeriod > (Report_start_date-years(1)) & TimePeriod < (Report_end_date - years(1)) )


ACCOM_p_1 <- data.table(ACCOM_p_1[order(ACCOM_p_1$TimePeriod, decreasing = TRUE), ])[1:5]
ACCOM_p_0 <- data.table(ACCOM_p_0[order(ACCOM_p_0$TimePeriod, decreasing = TRUE), ])[1:5]

ACCOM_p_sum <- merge(ACCOM_p_1, ACCOM_p_0, by = "Accom_Type")

ACCOM_p_sum <- ACCOM_p_sum[, list ("Occupancy Rates" = ACCOM_p_sum$Accom_Type, 
                                         "Current YE" =percent(round(ACCOM_p_sum$Num_of_Stay.x/100, digits=2)),
                                         "Last YE" = percent(round(ACCOM_p_sum$Num_of_Stay.y/100, digits=2)))]


Accom_tab2 <- print(xtable(ACCOM_p_sum, align = "|l|p{4.7cm}|p{1.25cm}|p{1.3cm}|", 
                           hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                    floating=FALSE, 
                    include.rownames=FALSE)
Accom_tab2 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Accom_tab2, fixed=T)
# Accom_tab2 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{6.5cm}}{{\\tiny Some notes on table};Some notes on table;Some notes on table;}\\\\ \n\\end{tabular}",Accom_tab2, fixed=T)

sink("tables/Accom_tab2.tex")
cat(Accom_tab2)
sink()

# ================================== RTE Summary ==================================

## This is the path of forecast data: Enterprise  Policy Advice, Regulation...  Tourism Research  Forecasts  Dissemination  2014 

RTE_query <- "select sp.Type, d.L1Description as RTO, mh.YearEndMarch, sum(sp.Spend) as Spend from 
(
  select * from Production.vw_RTESurveyMainHeader
  where YearEndMarch in 
  (
    select max(YearEndMarch) from Production.vw_RTESurveyMainHeader
  )
) mh
left join Production.vw_RTESpend sp
on mh.SurveyResponseID = sp.SurveyResponseID
left join
(
  Select distinct L1Description, L2Description from Classifications.vw_ClassificationLevels cl
  where cl.L2ClassificationName = 'MBIEInt_TAMOD_1_Geography' and 
  cl.L1ClassificationName = 'MBIEPub_RTO_1_Geography'
) d
on d.L2Description = mh.TA_MOD
group by mh.YearEndMarch, sp.Type, d.L1Description
order by mh.YearEndMarch"

RTE <- sqlQuery(TRED_Conn, RTE_query)

RTE_1 <- reshape(RTE, idvar ='RTO', v.names ='Spend', timevar ='Type', direction ='wide')
RTE_1 <-data.table(RTE_1)

RTE_1[RTO=="Auckland RTO", RTO := "Auckland"]
RTE_1[RTO=="Wellington RTO", RTO := "Wellington"]
RTE_1[RTO=="Queenstown RTO", RTO := "Queenstown"]
RTE_1[RTO=="Waikato RTO", RTO := "Waikato"]
RTE_1[RTO=="Northland RTO", RTO := "Northland"]



sum_spend <- sum(RTE_1$Spend.Domestic, RTE_1$Spend.International)
RTE_sum <-RTE_1[, list ("RTO ($mil)"= RTE_1$RTO, 
#                        "YearEndMarch"= RTE_1$YearEndMarch,
                       "Domestic" = format(round(RTE_1$Spend.Domestic, digits =0),big.mark = ","),
                       "International"= format(round(RTE_1$Spend.International, digits =0),big.mark = ","),
                        "Total" =format(round(RTE_1$Spend.Domestic+RTE_1$Spend.International, 0), big.mark = ","),
                        "%" =percent(round((RTE_1$Spend.Domestic+RTE_1$Spend.International)/sum_spend, digits= 2)))]

RTE_sum_1 <- data.table(RTE_sum[order(RTE_sum$Total, decreasing = TRUE), ])[1:6]

RTE_tab1 <- print(xtable(RTE_sum_1, align = "|l|p{2.95cm}|p{1cm}|p{1.5cm}|p{0.5cm}|p{0.5cm}|", 
                           hline.after=c(1), caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                    floating=FALSE, 
                    include.rownames=FALSE)
RTE_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",RTE_tab1, fixed=T)
# RTE_tab1 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{6.5cm}}{{\\tiny Some notes on table};Some notes on table;Some notes on table;}\\\\ \n\\end{tabular}",RTE_tab1, fixed=T)

sink("tables/RTE_tab1.tex")
cat(RTE_tab1)
sink()


# ==================== Data Sources ==========================

Data_S_1 <- paste("$^1$International Travel and Migration - 'Business' excludes conferences.")
sink("outputs/Data_S_1.txt")
cat(Data_S_1)
sink() 

Data_S_2 <- paste("$^2$International Visitor Survey - Figures in grey are not statistically significant. 'Business' excludes conferences.")
sink("outputs/Data_S_2.txt")
cat(Data_S_2)
sink()

Data_S_3 <- paste("$^3$International Travel and Migration")
sink("outputs/Data_S_3.txt")
cat(Data_S_3)
sink()


Data_S_4 <- paste("$^4$Accommodation Survey")
sink("outputs/Data_S_4.txt")
cat(Data_S_4)
sink()

Data_S_5 <- paste("$^5$Regional Tourism Estimation")
sink("outputs/Data_S_5.txt")
cat(Data_S_5)
sink()

Data_S_6 <- paste("$^6$Tourism Satellite Account")
sink("outputs/Data_S_6.txt")
cat(Data_S_6)
sink()

Data_S_7 <- paste("$^7$New Zealand's Tourism Sector Outlook: Forecasts for 2014-2020")
sink("outputs/Data_S_7.txt")
cat(Data_S_7)
sink()


Data_S_8 <- paste("This paper was prepared by the Ministry of Business, Innovation and Employment. For further information visit the 
                  tourism data section of the Ministry's website url{http://www.med.govt.nz/sectors-industries/tourism}. 
                  Key Tourism Statistics are updated regularly throughout the month as data is released."
                  )

Data_S_8 <- gsub("url","\\url",Data_S_8, fixed=T)

sink("outputs/Data_S_8.txt")

cat(Data_S_8)
sink() 











