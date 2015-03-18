
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
library(survey)
library(scales)


# ====================================create summary tables ============================================================

# ================================== Reporting Period Parameters ==================================

TRED_Conn <- odbcConnect("TRED_Prod")


# ================================== International Visitor Arrival Analysis ==================================

# --------- Title ---------

iv_pc <- ImportTS(TRED_Conn, "Visitor arrival totals (Monthly)")
save(iv_pc, file = "data/iv_pc.rda")
load("data/iv_pc.rda")

iv_report_end_date <- max(iv_pc$TimePeriod)


IVA_title <- paste("\\small Internatonal Visitor Arrivals$^1$ (Yearly ended at", months(iv_report_end_date), year(iv_report_end_date), ")")
sink("outputs/IVA_title.txt")
cat(IVA_title)
sink()

# --------- Total International Markets ---------



iv_pc_tot_1 <- subset(iv_pc, ClassificationValue == "Actual Counts" & 
                        TimePeriod > iv_report_end_date -years(1) & 
                        TimePeriod < iv_report_end_date + days(1))
iv_tot_1 <- sum(iv_pc_tot_1$Value)

iv_pc_tot_0 <- subset(iv_pc, ClassificationValue == "Actual Counts" & 
                        TimePeriod > iv_report_end_date -years(2) & 
                        TimePeriod < iv_report_end_date - years(1) + days(1))
iv_tot_0 <- sum(iv_pc_tot_0$Value)


iva_growth <- (iv_tot_1-iv_tot_0)/iv_tot_0

iva_total <-data.frame('Annual International Arrivals:', format(iv_tot_1, big.mark = ","), percent(round(iva_growth, digits=2)))
names(iva_total) <- c(" ", " ", "Growth (pa)")

iva_tot <- print(xtable(iva_total, align = "lp{5cm}p{1.3cm}p{1.2cm}",
                        caption= NULL, digits = 0,label=NULL, type="latex"), 
                 floating=FALSE, 
                 hline.after= NULL,
#                  hline.after= c(0, nrow(iva_tot)),
                 include.rownames=FALSE
                                 
)

iva_tot <- gsub("\\begin{tabular}","\\begin{tabular}[t]",iva_tot, fixed=T)
iva_tot <- gsub("Annual International Arrivals","\\textbf{Annual International Arrivals}", iva_tot, fixed=T)

sink("tables/iva_tot.tex")
cat(iva_tot)
sink()

# --------- Key International Markets ---------

iv_pc_p2 <- ImportTS(TRED_Conn, "Visitor arrivals by EVERY country of residence and purpose (Monthly)")


total_sum <- subset(iv_pc_p2, ClassificationValue == "TOTAL ALL COUNTRIES OF RESIDENCE" & 
                      ClassificationValue.1 =="TOTAL ALL TRAVEL PURPOSES" &
                    TimePeriod > iv_report_end_date -years(1) & 
                      TimePeriod < iv_report_end_date + days(1)) 

iva_total <- sum(total_sum$Value)

iv_pc_p2_1 <- subset(iv_pc_p2,  ClassificationValue.1 =="TOTAL ALL TRAVEL PURPOSES" & 
                       substr(CountryGrouped, 1, 4) != "Rest" & 
                       CountryGrouped != "Other" &
                       TimePeriod > iv_report_end_date -years(1) & 
                       TimePeriod < iv_report_end_date + days(1))

iv_pc_p2_0 <- subset(iv_pc_p2, ClassificationValue.1 =="TOTAL ALL TRAVEL PURPOSES" & 
                       substr(CountryGrouped, 1, 4) != "Rest" & 
                       CountryGrouped != "Other" &
                       TimePeriod > iv_report_end_date -years(2) & 
                       TimePeriod < iv_report_end_date -years(1) + days(1))

iv_pc_tot_sum_1 <- aggregate(Value ~ ClassificationValue, data=iv_pc_p2_1, FUN= "sum")
iv_pc_tot_sum_0 <- aggregate(Value ~ ClassificationValue, data=iv_pc_p2_0, FUN= "sum")

iv_pc_tot_sum_1 <- data.table(iv_pc_tot_sum_1[order(iv_pc_tot_sum_1$Value, decreasing = TRUE),])[1:6]

iv_pc_tot_sum <- merge(iv_pc_tot_sum_1, iv_pc_tot_sum_0, by = "ClassificationValue")
iv_pc_tot_sum <- iv_pc_tot_sum[order(iv_pc_tot_sum$Value.x, decreasing = TRUE), ]

Market_share <- percent(round(sum(iv_pc_tot_sum$Value.x)/iva_total,digits =2))


iv_pc_tot_sum[ClassificationValue=="China, People's Republic of", ClassificationValue := "China"]
iv_pc_tot_sum[ClassificationValue=="United States of America", ClassificationValue := "USA"]
iv_pc_tot_sum[ClassificationValue=="United Kingdom", ClassificationValue := "UK"]

iv_pc_tot_sum_final <- iv_pc_tot_sum[, list ("Key International Markets" = iv_pc_tot_sum$ClassificationValue, 
                                       "Market %" = percent(round(iv_pc_tot_sum$Value.x/iva_total, digits =2)), 
                                       "Visitors" =format(round(iv_pc_tot_sum$Value.x/1000, 0)*1000, big.mark = ","), 
                                       "Growth (pa)" = percent(round((iv_pc_tot_sum$Value.x-iv_pc_tot_sum$Value.y)/iv_pc_tot_sum$Value.y, digits=2)))]

IVA_tab1 <- print(xtable(iv_pc_tot_sum_final, align = "lp{3.5cm}p{1.1cm}p{1.3cm}p{1.2cm}",
                         caption= NULL, digits = 0,label=NULL,  type="latex"), 
                  floating=FALSE, 
                  hline.after= NULL,
#                   hline.after= c(0, nrow(iv_pc_tot_sum_final)),
                  include.rownames=FALSE,

                  )


IVA_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",IVA_tab1, fixed=T)
iva_tot <- gsub("Annual International Arrivals","\\textbf{Annual International Arrivals}", iva_tot, fixed=T)
sink("tables/IVA_tab1.tex")
cat(IVA_tab1)
sink()

note_1 <- paste("Combined, these markets provided", Market_share, "of international visitors to New Zealand for the year ended December 2014.")
note_1 <- gsub("%"," \\\\%",note_1)
sink("outputs/note_1.txt")
cat(note_1)
sink()

line_space <- paste("     ")
sink("outputs/line_space.txt")
cat(line_space)
sink()




#----------------------------- International Visitor Arrival Purpose of Visiting ---------------------------

iv_pov <- ImportTS(TRED_Conn, "Visitor arrivals by country of residence, purpose and length of stay (Monthly)")

save(iv, file = "data/iv_pov.rda")
load("data/iv_pov.rda")

iv_pov_report_end_date <- max(iv_pov$TimePeriod)

iv_pov_1 <- subset(iv, ClassificationValue.2 %in% c("TOTAL ALL LENGTHS OF STAY") & 
                     TimePeriod > iv_pov_report_end_date-years(1) & TimePeriod < iv_pov_report_end_date + days(1) & 
                     ClassificationValue.1 %in% c("Business", "Holiday/Vacation", "Visit Friends/Relatives"))

iv_pov_sum_1 <- aggregate(Value ~ ClassificationValue.1, data=iv_pov_1, FUN= "sum")
iv_pov_0 <- subset(iv, ClassificationValue.2 %in% c("TOTAL ALL LENGTHS OF STAY") & 
                     TimePeriod > iv_pov_report_end_date - years(2) & TimePeriod < iv_pov_report_end_date - years(1) + days(1) & 
                     ClassificationValue.1 %in% c("Business", "Holiday/Vacation", "Visit Friends/Relatives"))

iv_pov_sum_0 <- aggregate(Value ~ ClassificationValue.1, data=iv_pov_0, FUN= "sum")
iv_pov_tot_sum <- merge(iv_pov_sum_1, iv_pov_sum_0, by = "ClassificationValue.1")
iv_pov_tot_sum <- iv_pov_tot_sum[order(iv_pov_tot_sum $Value.x, decreasing = TRUE), ]
iv_pov_tot_sum <- data.table(iv_pov_tot_sum)

iv_pov_tot_sum <- iv_pov_tot_sum[, list ("Purpose of Visiting" = iv_pov_tot_sum$ClassificationValue.1,
                                         "Nights" = format(round(iv_pov_tot_sum$Value.x/1000, 0)*1000, big.mark = ","), 
                                         "Growth (pa)" = percent(round((iv_pov_tot_sum$Value.x-iv_pov_tot_sum$Value.y)/iv_pov_tot_sum$Value.y, digits=3)))]

IVA_tab2 <- print(xtable(iv_pov_tot_sum, align = "lp{5cm}p{1.3cm}p{1.2cm}",
                         caption= NULL, digits = 0,label=NULL, type="latex"), 
                  floating=FALSE, 
                  hline.after= NULL,
#                   hline.after= c(0, nrow(iv_pov_tot_sum)),
                  include.rownames=FALSE
                  )

IVA_tab2 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",IVA_tab2, fixed=T)
IVA_tab2 <- gsub("Purpose of Visiting","\\textbf{Purpose of Visiting}", IVA_tab2, fixed=T)
sink("tables/IVA_tab2.tex")
cat(IVA_tab2)
sink()


# -------------------------- International Visitor Arrival Calculate LOS -----------------------------


iv_los_1 <- subset(iv_pov, ClassificationValue.2 %in% c("Total stay days", "TOTAL ALL LENGTHS OF STAY") & 
                     TimePeriod > iv_report_end_date-years(1) & TimePeriod < iv_report_end_date + days(1) & 
                     ClassificationValue.1 %in% c("TOTAL ALL TRAVEL PURPOSES"))

iv_los_sum_1 <- aggregate(Value ~ ClassificationValue.2, data=iv_los_1, FUN= "sum")
iv_los_1 <- dcast(iv_los_1, TimePeriod ~ ClassificationValue.2, sum, value.var="Value")
iv_los_1 <- rename(iv_los_1, c("TOTAL ALL LENGTHS OF STAY" = "Tot_LOS" , "Total stay days" ="Tot_day"))
iv_los_sum_1 <- sum(iv_los_1$Tot_day)/sum(iv_los_1$Tot_LOS)
iv_los_0 <- subset(iv_pov, ClassificationValue.2 %in% c("Total stay days", "TOTAL ALL LENGTHS OF STAY") & 
                     TimePeriod > iv_report_end_date - years(2) & TimePeriod < iv_report_end_date - years(1) + days(1) & 
                     ClassificationValue.1 %in% c("TOTAL ALL TRAVEL PURPOSES"))

iv_los_sum_0 <- aggregate(Value ~ ClassificationValue.2, data=iv_los_0, FUN= "sum")
iv_los_0 <- dcast(iv_los_0, TimePeriod ~ ClassificationValue.2, sum, value.var="Value")
iv_los_0 <- rename(iv_los_0, c("TOTAL ALL LENGTHS OF STAY" = "Tot_LOS", "Total stay days" ="Tot_day"))

iv_los_sum_0 <- sum(iv_los_0$Tot_day)/sum(iv_los_0$Tot_LOS)
los_growth <- (iv_los_sum_1-iv_los_sum_0)/iv_los_sum_0



LOS <-data.frame('Average Intended length of stay:', iv_los_sum_1, percent(round(los_growth, digits=3)))
names(LOS) <- c(" ", "Days", "Growth (pa)")


IVA_los <- print(xtable(LOS, align = "lp{5cm}p{1.3cm}p{1.2cm}",
                         caption= NULL, digits = 0,label=NULL, type="latex"), 
                  floating=FALSE, 
                 hline.after= NULL,
#                  hline.after= c(0, nrow(LOS)),
                  include.rownames=FALSE
#                   include.colnames =FALSE                 
                  )


IVA_los <- gsub("\\begin{tabular}","\\begin{tabular}[t]",IVA_los, fixed=T)
IVA_los <- gsub("Average Intended length of stay:","\\textbf{Average Intended length of stay:}", IVA_los, fixed=T)
sink("tables/IVA_los.tex")
cat(IVA_los)
sink()



# ================================== International Visitor Expenditure ==================================


# --------- automatically pick the last 4 quarters ---------

SQL_query <- "select max(Year) as survey_yr, right(max(Qtr), len(max(qtr))-5) as survey_qtr FROM [TRED].[Production].[vw_IVSSurveyMainHeader]"

survey_date <- sqlQuery(TRED_Conn, SQL_query)

survey_qtr <- survey_date$survey_qtr
survey_yr <- survey_date$survey_yr

IVE_title <- paste("\\small Internatonal Visitor Expenditure$^∗^2$ (Yearly ended at", paste0("Q", survey_qtr), survey_yr, ")")
sink("outputs/IVE_title.txt")
cat(IVE_title)
sink()


if (survey_qtr== '1') {
    qry_period <- paste("(", paste0("'", survey_yr, " 1',"), paste0("'", survey_yr-1, " 2',"), paste0("'", survey_yr-1, " 3',"), paste0("'", survey_yr-1, " 4'"), ")");
    qry_period_prev <- paste("(", paste0("'", survey_yr-1, " 1',"), paste0("'", survey_yr-2, " 2',"), paste0("'", survey_yr-2, " 3',"), paste0("'", survey_yr-2, " 4'"), ")")
} else if (survey_qtr== '2') {
    qry_period <- paste("(", paste0("'", survey_yr, " 1',"), paste0("'", survey_yr, " 2',"), paste0("'", survey_yr-1, " 3',"), paste0("'", survey_yr-1, " 4'"), ")");
    qry_period_prev <- paste("(", paste0("'", survey_yr-1, " 1',"), paste0("'", survey_yr-1, " 2',"), paste0("'", survey_yr-2, " 3',"), paste0("'", survey_yr-2, " 4'"), ")")
} else if (survey_qtr== '3') {
    qry_period <- paste("(", paste0("'", survey_yr, " 1',"), paste0("'", survey_yr, " 2',"), paste0("'", survey_yr, " 3',"), paste0("'", survey_yr-1, " 4'"), ")");
    qry_period_prev <- paste("(", paste0("'", survey_yr-1, " 1',"), paste0("'", survey_yr-1, " 2',"), paste0("'", survey_yr-1, " 3',"), paste0("'", survey_yr-2, " 4'"), ")")
} else {
    qry_period <- paste("(", paste0("'", survey_yr, " 1',"), paste0("'", survey_yr, " 2',"), paste0("'", survey_yr, " 3',"), paste0("'", survey_yr, " 4'"), ")");
    qry_period_prev <- paste("(", paste0("'", survey_yr-1, " 1',"), paste0("'", survey_yr-1, " 2',"), paste0("'", survey_yr-1, " 3',"), paste0("'", survey_yr-1, " 4'"), ")")
}

# --------- calculate the total expenditure ---------

SQL_query_1 <- paste("SELECT SurveyResponseID, CORNextYr, Year, Qtr, PopulationWeight, WeightedSpend, PopulationWeight*WeightedSpend as Expenditure FROM Production.vw_IVSSurveyMainHeader where Qtr in ", qry_period)

ive_main_1 <- sqlQuery(TRED_Conn, SQL_query_1)

SQL_query_0 <- paste("SELECT SurveyResponseID, CORNextYr, Year, Qtr, PopulationWeight, WeightedSpend, PopulationWeight*WeightedSpend as Expenditure FROM Production.vw_IVSSurveyMainHeader where Qtr in ", qry_period_prev)

ive_main_0 <- sqlQuery(TRED_Conn, SQL_query_0)

ive_main_1 <- cbind(ive_main_1, "CountryGroup" = CountryGroup(ive_main_1$CORNextYr, shorten=TRUE, type="IVSweights", OneChina_first=FALSE))

ive_main_0 <- cbind(ive_main_0, "CountryGroup" = CountryGroup(ive_main_0$CORNextYr, shorten=TRUE, type="IVSweights", OneChina_first=FALSE))

ive_median_1 <- svydesign(ids=~1, weights= ~PopulationWeight, data = ive_main_1)
median_1 <- svyquantile(~WeightedSpend, design=ive_median_1, quantiles=0.5)

ive_median_0 <- svydesign(ids=~1, weights= ~PopulationWeight, data = ive_main_0)
median_0 <- svyquantile(~WeightedSpend, design=ive_median_0, quantiles=0.5)


ive_exp_1 <- rbind("A" = sum(ive_main_1$PopulationWeight*ive_main_1$WeightedSpend)/1000000,
                   "B" = sum(ive_main_1$PopulationWeight*ive_main_1$WeightedSpend)/sum(ive_main_1$PopulationWeight), 
                   median_1)


ive_exp_0 <- rbind("A" =sum(ive_main_0$PopulationWeight*ive_main_0$WeightedSpend)/1000000,
                   "B" =sum(ive_main_0$PopulationWeight*ive_main_0$WeightedSpend)/sum(ive_main_0$PopulationWeight), 
                   median_0)


ive_exp_sum <- merge(ive_exp_1, ive_exp_0, by ="row.names")

names(ive_exp_sum) <- c("Category", "present_yr", "previous_yr")

ive_exp_sum <- data.table(ive_exp_sum)

ive_exp_sum_final <- ive_exp_sum[, list(" " = c("Total Expenditure($mil)", "Average Expenditure per person per trip", "Median Expenditure per person per trip"), 
                                        " "= format(dollar(round(ive_exp_sum$present_yr, 0)),big.mark = ","),
                                        "Growth (pa)"= percent(round((ive_exp_sum$present_yr - ive_exp_sum$previous_yr)/ive_exp_sum$previous_yr, 2)))]

ive_exp_tab0 <- print(xtable(ive_exp_sum_final, align = "lp{5.4cm}p{0.9cm}p{1.2cm}",
                             caption= NULL, digits = 0,label=NULL, type="latex"), 
                      floating=FALSE,
                      hline.after= NULL,
#                       hline.after= c(0, nrow(ive_exp_sum_final)),
                      include.rownames=FALSE
)


ive_exp_tab0 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",ive_exp_tab0, fixed=T)
ive_exp_tab0 <- gsub("Total Expenditure($mil)","\\textbf{Total Expenditure($mil)}", ive_exp_tab0, fixed=T)
sink("tables/ive_exp_tab0.tex")
cat(ive_exp_tab0)
sink()


# --------- calculate the annual expenditure for the top market ---------


ive_main_sum_1 <- aggregate(Expenditure ~ CountryGroup, data=ive_main_1, FUN= "sum")

ive_main_sum_0 <- aggregate(Expenditure ~ CountryGroup, data=ive_main_0, FUN= "sum")
ive_exp_org_sum <- merge(ive_main_sum_1, ive_main_sum_0, by ='CountryGroup')

ive_exp_org_sum <- subset(ive_exp_org_sum, substr(CountryGroup, 1, 4) != "Rest")
 
exp_top <- data.table(ive_exp_org_sum[order(ive_exp_org_sum$Expenditure.x, decreasing = TRUE),])[1:5]

exp_top_sum <- exp_top[, list("Key International Markets ($mil)" = exp_top$CountryGroup,
                              "Current_Yr" = format(dollar(round(exp_top$Expenditure.x/1000000, 0)),big.mark = ","),
                              "Growth (pa)" = percent(round((exp_top$Expenditure.x-exp_top$Expenditure.y)/exp_top$Expenditure.y, digits=2)))]


IVE_tab1 <- print(xtable(exp_top_sum, align = "lp{5.4cm}p{0.9cm}p{1.2cm}", 
                         caption= NULL, digits = 0,label=NULL, type="latex"), 
                  floating=FALSE, 
                  hline.after= NULL,
#                   hline.after= c(0, nrow(exp_top_sum)),
                  include.rownames=FALSE)


IVE_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",IVE_tab1, fixed=T)
IVE_tab1 <- gsub("Key International Markets ($mil)","\\textbf{Key International Markets ($mil)}", IVE_tab1, fixed=T)
sink("tables/IVE_tab1.tex")
cat(IVE_tab1)
sink()


# --------- calculate total expenditure by purpose of visit ---------

sql_pov_1 <- paste("SELECT bb.POV, bb.total_sp as total_sp_curr FROM
(
SELECT top 3 POV, round(sum(WeightedSpend*PopulationWeight)/1000000, 0) as total_sp
FROM [TRED].[Production].[vw_IVSSurveyMainHeader]
WHERE (POV like '%Holiday%' or POV like '%Visiting Friends%Relatives%' or POV = 'Business') and Qtr in ", qry_period,
"group by POV
order by total_sp desc
) bb ")

pov_1 <- sqlQuery(TRED_Conn, sql_pov_1)

sql_pov_0 <- paste("SELECT bb.POV, bb.total_sp as total_sp_prev FROM
(
SELECT top 3 POV, round(sum(WeightedSpend*PopulationWeight)/1000000, 0) as total_sp
FROM [TRED].[Production].[vw_IVSSurveyMainHeader]
WHERE (POV like '%Holiday%' or POV like '%Visiting Friends%Relatives%' or POV = 'Business') and Qtr in", qry_period_prev,
"group by POV
order by total_sp desc
) bb ")

pov_0 <- sqlQuery(TRED_Conn, sql_pov_0)

ive_pov_sum <- merge(pov_1, pov_0)
ive_pov_sum <- data.table(ive_pov_sum)
ive_pov_sum <- ive_pov_sum[, list("Total Spend by Purpose of Visit" = ive_pov_sum$POV,
                                "Current_Yr" = format(ive_pov_sum$total_sp_curr, big.mark = ","),
                                "Growth (pa)" = percent(round((ive_pov_sum$total_sp_curr -ive_pov_sum$total_sp_prev)/ive_pov_sum$total_sp_prev, digits= 2)))]

ive_pov_sum <- ive_pov_sum[order(ive_pov_sum$"Current_Yr", decreasing = TRUE), ]

IVE_tab3 <- print(xtable(ive_pov_sum, align = "lp{5.4cm}p{0.9cm}p{1.2cm}", 
                         caption= NULL, digits = 0,label=NULL, type="latex"), 
                  floating=FALSE, 
                  hline.after= NULL,
#                   hline.after= c(0, nrow(ive_pov_sum)),
                  include.rownames=FALSE)


IVE_tab3 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",IVE_tab3, fixed=T)
IVE_tab3 <- gsub("Total Spend by Purpose of Visit","\\textbf{Total Spend by Purpose of Visit}", IVE_tab3, fixed=T)
IVE_tab3 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{8.25cm}}{$^*$Excludes international airfares and individuals whose purpose of visit to New Zealand was to attend a recognised educational institute, and are foreign-fee paying students.}\\\\ \n\\end{tabular}",IVE_tab3, fixed=T)

sink("tables/IVE_tab3.tex")
cat(IVE_tab3)
sink()

ive_note_1 <- paste("$^*$Excludes international airfares and individuals whose purpose of visit to New Zealand was 
                    to attend a recognised educational institute, and are foreign-fee paying students.")

sink("outputs/ive_note_1.txt")
cat(ive_note_1)
sink()

# ================================== Trip aboard by NZers ==================================

NZ_out_sum <- ImportTS(TRED_Conn, "Short-term NZ traveller departure totals (Monthly)")

NZ_out_sum_end_date = max(NZ_out_sum$TimePeriod)


NZ_out_sum_1 <- subset(NZ_out_sum, ClassificationValue == "Actual Counts" & 
                        TimePeriod > NZ_out_sum_end_date -years(1) & 
                        TimePeriod < NZ_out_sum_end_date + days(1))

NZ_out_sum_tot_1 <- sum(NZ_out_sum_1$Value)

NZ_out_sum_0 <- subset(NZ_out_sum, ClassificationValue == "Actual Counts" & 
                         TimePeriod > NZ_out_sum_end_date -years(2) & 
                         TimePeriod < NZ_out_sum_end_date - years(1) + days(1))

NZ_out_sum_tot_0 <- sum(NZ_out_sum_0$Value)

NZ_out_growth <- (NZ_out_sum_tot_1-NZ_out_sum_tot_0)/NZ_out_sum_tot_0

Annual_total_outbound <-data.frame('Annual Outbound Departures:', format(NZ_out_sum_tot_1, big.mark = ","), percent(round(NZ_out_growth, digits=2)))
names(Annual_total_outbound) <- c(" ", " ", "Growth (pa)")

NZ_out_tab1 <- print(xtable(Annual_total_outbound, align = "lp{5cm}p{1.3cm}p{1.2cm}", 
                            caption= NULL, digits = 0,label=NULL, type="latex"), 
                     floating=FALSE, 
                     hline.after= NULL,
                     #                      hline.after= c(0, nrow(Annual_total_outbound)),
                     include.rownames=FALSE)


NZ_out_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",NZ_out_tab1, fixed=T)
NZ_out_tab1 <- gsub("Annual Outbound Departures","\\textbf{Annual Outbound Departures}", NZ_out_tab1, fixed=T)
sink("tables/NZ_out_tab1.tex")
cat(NZ_out_tab1)
sink()



#-------------------------------------------- Key destination countries ---------------------------------


NZ_out <- ImportTS(TRED_Conn, "Short-term NZ traveller departures by EVERY country of main dest and purpose (Monthly)")

Report_end_date_NZ_out = max(NZ_out$TimePeriod)
Report_end_date_NZ_out_1 <- Report_end_date_NZ_out + days(1)

Report_start_date_NZ_out <- Report_end_date_NZ_out - years(1)
Report_start_date_NZ_out_1 <- Report_end_date_NZ_out - years(1) + days(1)

NZ_out_title <- paste("\\small Trips aboard by NZers$^3$ (Yearly ended at", months(Report_end_date_NZ_out), year(Report_end_date_NZ_out), ")")

sink("outputs/NZ_out_title.txt")
cat(NZ_out_title)
sink()


NZ_out <- rename(NZ_out, c("ClassificationValue" = "Country", "ClassificationValue.1" = "Trip_Type"))
save(NZ_out, file = "data/NZ_out.rda")
load("data/NZ_out.rda")

NZ_out_1 <- subset(NZ_out, !(Country %in% c("ASIA", "AMERICAS", "EUROPE", "OCEANIA", "TOTAL ALL COUNTRIES OF MAIN DESTINATION")) &
                     Trip_Type == "TOTAL ALL TRAVEL PURPOSES" & TimePeriod > Report_start_date_NZ_out & TimePeriod < Report_end_date_NZ_out_1 )
NZ_out_0 <- subset(NZ_out, !(Country %in% c("ASIA", "AMERICAS", "EUROPE", "OCEANIA", "TOTAL ALL COUNTRIES OF MAIN DESTINATION")) &
                     Trip_Type == "TOTAL ALL TRAVEL PURPOSES" & TimePeriod > (Report_start_date_NZ_out-years(1)) & TimePeriod < (Report_end_date_NZ_out_1 - years(1)) )


# --- sumary the total visit by country 

NZ_out_total_1 <- aggregate(Value ~ Country, data=NZ_out_1, FUN=sum)
NZ_out_total_0 <- aggregate(Value ~ Country, data=NZ_out_0, FUN=sum)
NZ_out_total <- merge(NZ_out_total_1, NZ_out_total_0, by = "Country")

NZ_out_sum_1 <- data.table(NZ_out_total[order(NZ_out_total$Value.x, decreasing = TRUE), ])[1:5]

NZ_out_sum_1[Country=="China, People's Republic of", Country := "China"]
NZ_out_sum_1[Country=="United States of America", Country := "USA"]
NZ_out_sum_1[Country=="United Kingdom", Country := "UK"]


NZ_out_sum <- NZ_out_sum_1[, list ("Countries Visited by NZers" = NZ_out_sum_1$Country, 
                                   "Visitors" =format(NZ_out_sum_1$Value.x, big.mark = ","),
                                   "Growth (pa)" = percent(round((NZ_out_sum_1$Value.x-NZ_out_sum_1$Value.y)/NZ_out_sum_1$Value.y, digits=3)))]

NZ_out_tab2 <- print(xtable(NZ_out_sum, align = "lp{5cm}p{1.3cm}p{1.2cm}", 
                            caption= NULL, digits = 0,label=NULL, type="latex"), 
                     floating=FALSE, 
                     hline.after= NULL,
#                      hline.after= c(0, nrow(NZ_out_sum)),
                     include.rownames=FALSE)


NZ_out_tab2 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",NZ_out_tab2, fixed=T)
NZ_out_tab2 <- gsub("Countries Visited by NZers","\\textbf{Countries Visited by NZers}", NZ_out_tab2, fixed=T)
sink("tables/NZ_out_tab2.tex")
cat(NZ_out_tab2)
sink()


# ================================== Tourism Forecast ==================================


Fcst_query <- "SELECT * FROM Production.vw_NZTFSurveyMainHeader"

Fcst <- sqlQuery(TRED_Conn, Fcst_query)

Fcst_year <- as.numeric(max(Fcst$ForecastYear))
End_year <- as.numeric(max(Fcst$Year))

fcst_title <- paste("\\small Tourism Forecast$^7$ (Forecast Period", Fcst_year, "-", End_year, ")")
sink("outputs/fcst_title.txt")
cat(fcst_title)
sink()

Fcst_sum <- data.table(subset(Fcst, ForecastYear == Fcst_year & (Year == End_year | Year == Fcst_year-1)))

Fcst_sum_1 <- aggregate(cbind("TotalVisitorArrivals"=TotalVisitorArrivals/1000000, 
                              "TotalVisitorDays" =TotalVisitorDays/1000000,
                              "TotalVisitorSpend" = TotalVisitorSpend/1000) ~ Year, data=Fcst_sum, FUN=sum)

Fcst_sum_2 <- t(Fcst_sum_1)
Fcst_sum_2_new <- data.table(Fcst_sum_2)
Fcst_sum_2_new = data.table(Fcst_sum_2_new[-1, ])
Fcst_sum_2_new <- rename(Fcst_sum_2_new, c("V1" = "Last_Year_Actual", "V2" = "Forecast_2020"))
Forecast_Category <- rbind("VisitorArrivals(mil)", "VisitorDays(mil)", "VisitorSpend($bil)")

Fcst_sum_total <- cbind(Forecast_Category, Fcst_sum_2_new)

Fcst_sum_total <- rename(Fcst_sum_total, c("V1"= "Forecast_Category"))

Fcst_sum_2_rpt <- Fcst_sum_total[, list("Total"= Fcst_sum_total$Forecast_Category,
                                        "Forecast" = format(round(Fcst_sum_total$Forecast_2020, digits =1), big.mark = ","),
                                        "CAGR" = 
                                          percent(CAGR((Fcst_sum_total$Forecast_2020)/Fcst_sum_total$Last_Year_Actual,(End_year-Fcst_year+1))/100))]
                                    

Fcst_tab1 <- print(xtable(Fcst_sum_2_rpt, align = "lp{5.4cm}p{0.9cm}p{1.2cm}", 
                          caption= NULL, digits = 0,label=NULL, type="latex"), 
                   floating=FALSE, 
                   hline.after= NULL,
#                    hline.after= c(0, nrow(Fcst_sum_2_rpt)),
                   include.rownames=FALSE)


Fcst_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Fcst_tab1, fixed=T)
Fcst_tab1 <- gsub("CAGR","CAGR$^-$",Fcst_tab1, fixed=T)
Fcst_tab1 <- gsub("\\end{tabular}","\n\\multicolumn{3}{p{8.25cm}}{$^-$CAGR = Compound Annual Growth Rate.}\\\\ \\end{tabular}",Fcst_tab1, fixed=T)

sink("tables/Fcst_tab1.tex")
cat(Fcst_tab1)
sink()


Fcst_sum_3 <- Fcst_sum_2[, list(]

Fcst_sum_2_rpt <- Fcst_sum_1[, list("Visitor Arrivals" = Fcst_sum_2$row.names, 
                                    "Visitor Arrivals" = Fcst_sum_2$V2, 
                                    "growth_rate" = CAGR((Fcst_sum_1$V2-Fcst_sum_1$V1)/Fcst_sum_1$V1, (as.numeric(max(Fcst$Year))-as.numeric(max(Fcst$ForecastYear))+1)),
                                    "Visit Nights" = Fcst_sum_1$TotalVisitorDays, 
                                    "growth_rate" = CAGR((Fcst_sum_1$V2-Fcst_sum_1$V1)/Fcst_sum_1$V1, (as.numeric(max(Fcst$Year))-as.numeric(max(Fcst$ForecastYear))+1)),
                                    "Expenditure($Mil)" = Fcst_sum_1$V2, 
                                    "growth_rate" = CAGR((Fcst_sum_1$V2-Fcst_sum_1$V1)/Fcst_sum_1$V1, (as.numeric(max(Fcst$Year))-as.numeric(max(Fcst$ForecastYear))+1))
                                    )]



# --------- Key International Market Forecast ---------

Fcst_sum_key <- subset(Fcst_sum, Year == End_year & Country != "Other")
Fcst_sum_key <- Fcst_sum_key[, list("Key International Markets"= Fcst_sum_key$Country,
                                    "Visitors" = format(round(Fcst_sum_key$TotalVisitorArrivals/1000, 0)*1000, big.mark = ","),
                                    "Spend($Mil)" = format(Fcst_sum_key$TotalVisitorSpend, big.mark = ","))]

Fcst_sum_key <- Fcst_sum_key[order(Fcst_sum_key$Visitors, decreasing = TRUE), ][1:5]

Fcst_tab2 <- print(xtable(Fcst_sum_key, align = "lp{5.4cm}p{0.9cm}p{1.2cm}", 
                          caption= NULL, digits = 0,label=NULL, type="latex"), 
                   floating=FALSE, 
                   hline.after= NULL,
#                    hline.after= c(0, nrow(Fcst_sum_key)),
                   include.rownames=FALSE)

Fcst_tab2 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Fcst_tab2, fixed=T)
Fcst_tab2 <- gsub("Key International Markets","\\textbf{Key International Markets}", Fcst_tab2, fixed=T)
sink("tables/Fcst_tab2.tex")
cat(Fcst_tab2)
sink()


# ================================== Economic Contribution ==================================

# --------- Tourism Export ---------



EC_exp <- ImportTS(TRED_Conn, "Summary of Tourism Expenditure by type of tourist (ANZSIC06) (Annual-Mar)")

EC_exp_sum <- subset(EC_exp, TimePeriod == max(EC_exp$TimePeriod))


EC_exp_sum <-data.table(EC_exp_sum)

ec_end_date <- max(EC_exp_sum$TimePeriod)
ec_year <- year(ec_end_date)

ec_title <- paste("\\small Economic Contribution$^6$ (Year End March", ec_year, ")")
sink("outputs/ec_title.txt")
cat(ec_title)
sink()





# calculate the percentage of internations tourism expenditure

EC_exp_sum_1 <- subset(EC_exp_sum, ClassificationValue %in% c('International tourism as a percentage of total exports', select=c(3)))
EC_exp_sum_1 <- data.table(EC_exp_sum_1)                             
EC_exp_share <- EC_exp_sum_1[, "Value", with= FALSE]                    

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
                                            "Growth (pa)" = percent(EC_exp_table_sum$Value.y/100))]


EC_exp_table_sum <- data.table(EC_exp_table_sum, key ="Tourism Market")
EC_exp_table_sum <- data.table(EC_exp_table_sum[order(EC_exp_table_sum$Expenditure, decreasing = FALSE),])

inter_exp <- EC_exp_table_sum[, "Expenditure", with= FALSE][1]

EC_exp_tab1 <- print(xtable(EC_exp_table_sum, align = "lp{5cm}p{1.3cm}p{1.2cm}", 
                            caption= NULL, digits = 0,label=NULL, type="latex"), 
                     floating=FALSE, 
                     hline.after= NULL,
#                      hline.after= c(0, nrow(EC_exp_table_sum)),
                     include.rownames=FALSE)


EC_exp_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",EC_exp_tab1, fixed=T)
EC_exp_tab1 <- gsub("Tourism Market","\\textbf{Tourism Market}", EC_exp_tab1, fixed=T)
EC_exp_tab1 <- gsub("international","international$^+$",EC_exp_tab1, fixed=T)
EC_exp_tab1 <- gsub("\\end{tabular}","\\multicolumn{3}{p{7.5cm}}{$^+$Includes international airfares paid to New Zealand carriers.}\\ \n\\end{tabular}",EC_exp_tab1, fixed=T)

sink("tables/EC_exp_tab1.tex")
cat(EC_exp_tab1)
sink()



EC_comment_1 <- paste("International tourist expenditure accounted for", paste0("\\$", inter_exp), "billion or ", paste0(EC_exp_share, "\\%"), "of New Zealand total export earnings.")
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

inter_gdp4 <- EC_GDP_sum_rpt_1[, "Value", with= FALSE][2]
# inter_gdp4 <- percent(inter_gdp4$Value/100)
inter_gdp4 <- inter_gdp4$Value

EC_comment_2 <- paste("Tourism directly contributes", paste0("\\$", inter_gdp1), "billion (or", paste0(inter_gdp2, "\\%"),
                      ") to New Zealand total GDP. A further", paste0("\\$", inter_gdp3), "billion (or", paste0(inter_gdp4, "\\%"),
                      ") is indirectly contributed. When comparing tourism to other industries, the direct contribution should be used.")

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
inter_emp1 <- format(inter_emp1, big.mark = ",")

inter_emp2 <- EC_emp_sum_rpt[, "Value", with= FALSE][2]
inter_emp2<- format(inter_emp2, big.mark = ",")

EC_comment_3 <- paste("Tourism directly supports",inter_emp1, "full-time equivalent jobs", paste0("(", inter_emp2, "\\%"),
                      "of the total workforce in New Zealand).")

sink("outputs/EC_comment_3.txt")
cat(EC_comment_3)
sink() 


# ================================== Commercial Accommodation Stats ==================================

ACCOM <- ImportTS(TRED_Conn, "Actual by Accommodation by Type by Variable (Monthly)")
ACCOM<- data.table(ACCOM)

accom_report_end_date <- max(ACCOM$TimePeriod)

accom_title <- paste("\\small Commercial Accommodation$^4$ (Yearly ended at", months(accom_report_end_date), year(accom_report_end_date), ")")
sink("outputs/accom_title.txt")
cat(accom_title)
sink()

# I need add this in as this is for annual summary 13 Mar 2015

ACCOM_annual<- ImportTS(TRED_Conn, "Actual by Accommodation by Type by Variable (Annual-Dec)")


# --------- Guest Nights Summary ---------

ACCOM_1 <- subset(ACCOM, ClassificationValue %in% c('Holiday parks', 'Backpackers', 'Motels', 'Hotels', 'Total') & 
                    TimePeriod > accom_report_end_date-years(1) & TimePeriod < accom_report_end_date +days(1) )

ACCOM_0 <- subset(ACCOM, ClassificationValue %in% c('Holiday parks', 'Backpackers', 'Motels', 'Hotels', 'Total') & 
                    TimePeriod > (accom_report_end_date-years(2)) & TimePeriod < (accom_report_end_date - years(1)+days(1)) )

ACCOM_type_1 <- subset(ACCOM_1, ClassificationValue.1 %in% c('Number of guest nights'))
ACCOM_type_0 <- subset(ACCOM_0, ClassificationValue.1 %in% c('Number of guest nights'))
ACCOM_type_sum_1 <- aggregate(Value ~ ClassificationValue, data=ACCOM_type_1, FUN=sum)
ACCOM_type_sum_0 <- aggregate(Value ~ ClassificationValue, data=ACCOM_type_0, FUN=sum)
ACCOM_type_sum_1 <- ACCOM_type_sum_1[order(ACCOM_type_sum_1$Value, decreasing = TRUE), ]
ACCOM_type_sum <- merge(ACCOM_type_sum_1, ACCOM_type_sum_0, by = "ClassificationValue")
ACCOM_type_sum <- data.table(ACCOM_type_sum[order(ACCOM_type_sum$Value.x, decreasing = TRUE), ])

ACCOM_type_sum <- ACCOM_type_sum[, list ("Accommodation Type" = ACCOM_type_sum$ClassificationValue, 
                                         "Nights" =format(ACCOM_type_sum$Value.x, big.mark = ","),
                                         "Growth (pa)" = percent(round((ACCOM_type_sum$Value.x-ACCOM_type_sum$Value.y)/ACCOM_type_sum$Value.y, digits=3)))]

ACCOM_type_sum <- data.table(ACCOM_type_sum[order(ACCOM_type_sum$"Accommodation Type", decreasing = FALSE), ])


Accom_tab1 <- print(xtable(ACCOM_type_sum, align = "lp{5cm}p{1.3cm}p{1.2cm}", 
                           caption= NULL, digits = 0,label=NULL, latex.environments = ""), 
                    floating=FALSE, 
                    hline.after= NULL,
#                     hline.after= c(0, nrow(ACCOM_type_sum)),
                    include.rownames=FALSE)

Accom_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Accom_tab1, fixed=T)
Accom_tab1 <- gsub("Accommodation Type","\\textbf{Accommodation Type}", Accom_tab1, fixed=T)
sink("tables/Accom_tab1.tex")
cat(Accom_tab1)
sink()

# --------- Occupancy Rates Summary ---------

ACCOM_p <- subset(ACCOM, ClassificationValue %in% c('Holiday parks', 'Backpackers', 'Motels', 'Hotels', 'Total'), select=c(1,2,3,4), )
ACCOM_p <- rename(ACCOM_p, c("ClassificationValue" = "Accom_Type", "ClassificationValue.1" = "Class", "Value" = "Num_of_Stay"))
ACCOM_p <- subset(ACCOM_p, Class %in% c('Occupancy rate (percent)'))

ACCOM_p_1 <- subset(ACCOM_p, TimePeriod > accom_report_end_date-years(1) & TimePeriod < accom_report_end_date +days(1))
ACCOM_p_0 <- subset(ACCOM_p, TimePeriod > (accom_report_end_date-years(2)) & TimePeriod < (accom_report_end_date - years(1)+days(1)) )


ACCOM_p_1 <- data.table(ACCOM_p_1[order(ACCOM_p_1$TimePeriod, decreasing = TRUE), ])[1:5]
ACCOM_p_0 <- data.table(ACCOM_p_0[order(ACCOM_p_0$TimePeriod, decreasing = TRUE), ])[1:5]

ACCOM_p_sum <- merge(ACCOM_p_1, ACCOM_p_0, by = "Accom_Type")

ACCOM_p_sum <- ACCOM_p_sum[, list ("Occupancy Rates" = ACCOM_p_sum$Accom_Type, 
                                         "Current YE" =percent(round(ACCOM_p_sum$Num_of_Stay.x/100, digits=2)),
                                         "Last YE" = percent(round(ACCOM_p_sum$Num_of_Stay.y/100, digits=2)))]


Accom_tab2 <- print(xtable(ACCOM_p_sum, align = "lp{5cm}p{1.3cm}p{1.2cm}", 
                           caption= NULL, digits = 0,label=NULL, type="latex"), 
                    floating=FALSE, 
                    hline.after= NULL,
#                     hline.after= c(0, nrow(ACCOM_p_sum)),
                    include.rownames=FALSE)


Accom_tab2 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Accom_tab2, fixed=T)
Accom_tab2 <- gsub("Occupancy Rates","\\textbf{Occupancy Rates}", Accom_tab2, fixed=T)
sink("tables/Accom_tab2.tex")
cat(Accom_tab2)
sink()


# ================================== RTE Summary ==================================


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

rte_end_date <- max(RTE$YearEndMarch)

rte_title <- paste("\\small Regional Tourist Estimate$^5$ (Year End March", rte_end_date, ")")
sink("outputs/rte_title.txt")
cat(rte_title)
sink()



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
                        "International"= format(round(RTE_1$Spend.International, digits =0),big.mark = ","),
                        "Domestic" = format(round(RTE_1$Spend.Domestic, digits =0),big.mark = ","),
                        "Total" =format(round(RTE_1$Spend.Domestic+RTE_1$Spend.International, 0), big.mark = ","),
                        "%" =percent(round((RTE_1$Spend.Domestic+RTE_1$Spend.International)/sum_spend, digits= 2)))]

RTE_sum_1 <- data.table(RTE_sum[order(RTE_sum$Total, decreasing = TRUE), ])[1:6]

RTE_tab1 <- print(xtable(RTE_sum_1, align = "lp{2.3cm}p{1.2cm}p{1cm}p{0.9cm}p{1.2cm}", 
                         caption= NULL, digits = 0,label=NULL, type="latex"), 
                  floating=FALSE, 
                  hline.after= NULL,
#                   hline.after= c(0, nrow(RTE_sum_1)),
                  include.rownames=FALSE)

RTE_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",RTE_tab1, fixed=T)
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











