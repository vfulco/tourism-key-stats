 

#######################################################################################################
## Project description:  Key Tourism Statistics - Part 1: Generate one-page statistics - tables
## 
## Project file location:  
##    P:\OTSP\Comms\KeyStats
##
## Project Objective:    
##    Key Tourism Statistics is one-page summary of latest NZ tourism statistics including forcasts. 
##    The first part is to provide different Statistics tables
##
## Authors: George Fan and Peter Ellis
##    
## Data source: IVS data in TRED
##
## Project Status: to review
##
## Document history:  
##    Created by George Fan on 20 April 2015
##     
##    Reviewed by 
##       1)  Pete McMillen, 11 May 2015 - corrected grammar/typos, removed redundant blanks, reordered source footnotes to reflect page order.
##       2) 
##
## Note: 
##
##
########################################################################################################

.libPaths("P:/R/libraries/3.1.2")

# Preparation -------------------------------------------------------------

library(lubridate)
library(Defaults)
library(utils)
library(xtable)
library(data.table)
library(RODBC)
library(stringr)
library(mbie)
library(mbiedata)
library(mbieDBmisc)
library(survey)
library(scales)
library(dplyr)
library(tidyr)
source("r/functions.R")

# ====================================create summary tables ============================================================

# ================================== Reporting Period Parameters ==================================

TRED <- odbcConnect("TRED_Prod")

Key_Market_arrival = 6
Key_Market_spend = 6
Key_Market_Fcst = 6
Key_outbound_destination = 5
Key_region = 6 

qry_starting_date <- today() - years(3)

yday(qry_starting_date) <- 1


# ================================== International Visitor Arrival Analysis ==================================

iv_pc <- ImportTS(TRED, "Visitor arrival totals (Monthly)",
                  where = paste("cc1.ClassificationValue = 'Actual Counts' and TimePeriod > '", 
                                qry_starting_date, "'"))

iv_report_end_date <- max(iv_pc$TimePeriod)


IVA_title <- paste0("\\small International Visitor Arrivals$^1$ (year ending ", 
                   months(iv_report_end_date), " ", year(iv_report_end_date), ")")
sink("outputs/IVA_title.txt")
  cat(IVA_title)
sink()

# --------- Total International Markets ---------

iv_tot_1 <- iv_pc %>%
  filter(TimePeriod > iv_report_end_date - years(1) & 
         TimePeriod < iv_report_end_date + days(1)) %>%
  select(Value) %>%
  sum()

iv_tot_0 <- iv_pc %>% 
  filter(TimePeriod > iv_report_end_date -years(2) & 
         TimePeriod < iv_report_end_date - years(1) + days(1)) %>%
  select(Value) %>%
  sum()
 
iva_growth <- (iv_tot_1 - iv_tot_0) / iv_tot_0

iva_total <- data.frame('Annual international arrivals:', 
                        ' ',
                        format(round(iv_tot_1/1000, 0)*1000, big.mark = ","), 
                        percent(round(iva_growth, digits = 2)))
names(iva_total) <- c(" ", "Market Share", "Visits", "Growth (pa)")

iva_tot <- iva_total %>% 
  xtable(align = "lp{2.95cm}p{1.35cm}p{1.1cm}p{1.7cm}",
         caption = NULL, digits = 0,label = NULL, type = "latex") %>%
  print(floating = FALSE, 
        hline.after = NULL,
#       hline.after = c(0, nrow(iva_tot)),
        include.rownames = FALSE)
                                 
iva_tot <- gsub("\\begin{tabular}", "\\begin{tabular}[t]", iva_tot, fixed = T)
iva_tot <- gsub("}p{", "}>{\\hfill}p{", iva_tot, fixed = T)
iva_tot <- gsub("Annual international arrivals", "\\textbf{Annual international arrivals}", iva_tot, fixed = T)

sink("tables/iva_tot.tex")
  cat(iva_tot)
sink()

# --------- Key International Markets ---------

iv_pc_p2 <- ImportTS(TRED, "Visitor arrivals by EVERY country of residence and purpose (Monthly)",
                     where = paste("TimePeriod > '", qry_starting_date, "'"))


total_sum <- iv_pc_p2 %>%
  filter(ClassificationValue == "TOTAL ALL COUNTRIES OF RESIDENCE" & 
           ClassificationValue.1 == "TOTAL ALL TRAVEL PURPOSES" &
           TimePeriod > iv_report_end_date - years(1) & 
           TimePeriod < iv_report_end_date + days(1)) %>%
  select(Value) %>%
  sum()


Key_Market <- iv_pc_p2 %>%
  filter(ClassificationValue.1 == "TOTAL ALL TRAVEL PURPOSES" & 
           substr(CountryGrouped, 1, 4) != "Rest" & 
           CountryGrouped != "Other") %>%
  mutate(Year_Period = ifelse(TimePeriod > iv_report_end_date - years(1), "Current", 
                       ifelse(TimePeriod > iv_report_end_date - years(2), "Last", "Earlier"))) %>%
  group_by(CountryGrouped) %>%
  summarise(
    Visitors = sum(Value[Year_Period == "Current"]),
    "Market %" = paste0(round(Visitors / iv_tot_1 * 100), "%"),
    "Growth (pa)" = paste0(round((Visitors / sum(Value[Year_Period == "Last"]) - 1) * 100), "%")
      ) 

Market_share <- Key_Market %>% 
  top_n(Key_Market_arrival, Visitors) %>%
  select(Visitors) %>% 
  sum()/total_sum

Market_share <- percent(round(Market_share, 3))

Key_Market <- Key_Market %>%
  top_n(Key_Market_arrival, Visitors) %>%
  arrange(-Visitors) %>%
  mutate(Visits = format(round(Visitors/1000, 0)*1000, big.mark = ",")) %>%
  select(1, 3, 5, 4) %>%
  rename('Key international markets' = CountryGrouped) %>%
  clean_names()
names(Key_Market) <- c("    ", " ", "   ", "  ")

IVA_tab1 <- print(xtable(Key_Market, align = "lp{3cm}p{1.3cm}p{1.1cm}p{1.7cm}",
                         caption = NULL, digits = 0,label = NULL,  type = "latex"), 
                  floating = FALSE, 
                  hline.after = NULL,
                  #                   hline.after = c(0, nrow(iv_pc_tot_sum_final)),
                  include.rownames = FALSE,
                  
)


IVA_tab1 <- gsub("\\begin{tabular}", "\\begin{tabular}[t]", IVA_tab1, fixed=T)
IVA_tab1 <- gsub("}p{", "}>{\\hfill}p{", IVA_tab1, fixed = T)

sink("tables/IVA_tab1.tex")
cat(IVA_tab1)
sink()

note_1 <- paste0("Combined, these markets provided ", Market_share, " of international visits to New Zealand for the year ended ", 
                  months(iv_report_end_date), " ", year(iv_report_end_date), ".")
note_1 <- gsub("%"," \\\\%",note_1)
sink("outputs/note_1.txt")
cat(note_1)
sink()

line_space <- paste("     ")
sink("outputs/line_space.txt")
cat(line_space)
sink()

#----------------------------- International Visitor Arrival Purpose of Visit ---------------------------

iv_pov <- ImportTS(TRED, "Visitor arrivals by country of residence, purpose and length of stay (Monthly)", 
                   where = paste("TimePeriod > '", qry_starting_date, "'"))

iv_pov_report_end_date <- max(iv_pov$TimePeriod)


unique(iv_pov$ClassificationValue.1)


# POV_sum <- iv_pov %>%
#   filter(ClassificationValue.2 %in% c("TOTAL ALL LENGTHS OF STAY") & 
#            ClassificationValue.1 %in% c("Business", "Holiday/Vacation", "Visit Friends/Relatives")) %>%
#   mutate(Year_Period = ifelse(TimePeriod > iv_report_end_date - years(1), "Current", 
#                               ifelse(TimePeriod > iv_report_end_date - years(2), "Last", "Earlier"))) %>%
#   group_by(ClassificationValue.1) %>%
#   summarise(
#     Nights = sum(Value[Year_Period == "Current"]),
#     "Growth (pa)" = paste0(round((Nights / sum(Value[Year_Period == "Last"]) - 1) * 100), "%")) %>%
#   arrange(-Nights) %>%
#   mutate(Nights = format(round(Nights/1000, 0)*1000, big.mark = ",")) %>%
#   rename('Purpose of visit' = ClassificationValue.1) %>%
#   clean_names()
# 

POV_sum <- iv_pov %>%
  filter(ClassificationValue.2 %in% c("TOTAL ALL LENGTHS OF STAY")) %>%
  mutate(Year_Period = ifelse(TimePeriod > iv_report_end_date - years(1), "Current", 
                              ifelse(TimePeriod > iv_report_end_date - years(2), "Last", "Earlier"))) %>%
  filter(Year_Period != "Earlier") %>%
  rename('POV' = ClassificationValue.1) %>%
  group_by(POV, Year_Period) %>%
  summarise(Value = sum(Value)) %>%
  data.frame()

POV_sum_3 <- POV_sum %>%
  filter(POV != "TOTAL ALL TRAVEL PURPOSES") %>%
  mutate(POV = "Other") %>%
  group_by(POV, Year_Period) %>%
  summarise(Value1 = sum(Value))

POV_sum_All <- POV_sum %>%
  filter(POV == "TOTAL ALL TRAVEL PURPOSES") %>% 
  group_by(POV, Year_Period) %>%
  summarise(Value2 = sum(Value)) %>%
  rename('POV1' = POV)

POV_sum_other <- left_join(POV_sum_3, POV_sum_All, by = "Year_Period") %>%
  mutate(Value = Value2 - Value1) %>%
  select(-Value1, -Value2, -POV1)

POV_sum_data <- bind_rows(POV_sum , POV_sum_other) %>%
  filter(POV != "TOTAL ALL TRAVEL PURPOSES") %>%
  select(POV, Year_Period, Value)


POV_sum_data_3 <- POV_sum_data %>%
  filter(POV != 'Other') %>%
  group_by(POV) %>%
  summarise(
    Visits = sum(Value[Year_Period == "Current"]),
    "Growth (pa)" = paste0(round((Visits / sum(Value[Year_Period == "Last"]) - 1) * 100), "%")) %>%
  arrange(-Visits) %>%
  mutate(Visits = format(round(Visits/1000, 0)*1000, big.mark = ","))

POV_sum_data_other <- POV_sum_data %>%
  filter(POV == 'Other') %>%
  group_by(POV) %>%
  summarise(
    Visits = sum(Value[Year_Period == "Current"]),
    "Growth (pa)" = paste0(round((Visits / sum(Value[Year_Period == "Last"]) - 1) * 100), "%")) %>%
  arrange(-Visits) %>%
  mutate(Visits = format(round(Visits/1000, 0)*1000, big.mark = ","))

POV_sum_final <- rbind(POV_sum_data_3, POV_sum_data_other) %>%
  rename('Purpose of visit' = POV) %>%
  clean_names()

IVA_tab2 <- print(xtable(POV_sum_final, align = "lp{4.7cm}p{1.1cm}p{1.7cm}",
                         caption = NULL, digits = 0,label = NULL, type = "latex"), 
                  floating = FALSE, 
                  hline.after = NULL,
                  include.rownames = FALSE
                  )

IVA_tab2 <- gsub("\\begin{tabular}", "\\begin{tabular}[t]",IVA_tab2, fixed = T)
IVA_tab2 <- gsub("}p{", "}>{\\hfill}p{", IVA_tab2, fixed = T)
IVA_tab2 <- gsub("Purpose of visit", "\\textbf{Purpose of visit}", IVA_tab2, fixed = T)
sink("tables/IVA_tab2.tex")
cat(IVA_tab2)
sink()


# -------------------------- International Visitor Arrival Calculate LOS -----------------------------

LOS_sum <- iv_pov %>%
  filter(ClassificationValue.2 %in% c("Total stay days", "TOTAL ALL LENGTHS OF STAY") & 
           ClassificationValue.1 %in% c("TOTAL ALL TRAVEL PURPOSES")) %>%
  mutate(Year_Period = ifelse(TimePeriod > iv_report_end_date - years(1), "Current", 
                              ifelse(TimePeriod > iv_report_end_date - years(2), "Last", "Earlier"))) %>%
  group_by(ClassificationValue.1) %>%
  summarise(
    Days = sum(Value[Year_Period == "Current" & ClassificationValue.2 == "Total stay days"])/
      sum(Value[Year_Period == "Current" & ClassificationValue.2 == "TOTAL ALL LENGTHS OF STAY"]),
    Days_Last = sum(Value[Year_Period == "Last" & ClassificationValue.2 == "Total stay days"])/
      sum(Value[Year_Period == "Last" & ClassificationValue.2 == "TOTAL ALL LENGTHS OF STAY"]),
    "Growth (pa)" = paste0(round((Days / Days_Last - 1) * 100), "%")) %>%
#   arrange(-Nights) %>%
  mutate(ClassificationValue.1 = 'Average intended length of stay:', Days = round(Days)) %>%
  select(1, 2, 4) %>%
  rename(' ' = ClassificationValue.1)


IVA_los <- print(xtable(LOS_sum, align = "lp{4.45cm}p{1.3cm}p{1.7cm}",
                         caption = NULL, digits = 0,label = NULL, type = "latex"), 
                  floating = FALSE, 
                 hline.after = NULL,
#                  hline.after = c(0, nrow(LOS_sum)),
                  include.rownames = FALSE
#                   include.colnames = FALSE                 
                  )


IVA_los <- gsub("\\begin{tabular}", "\\begin{tabular}[t]", IVA_los, fixed = T)
IVA_los <- gsub("}p{", "}>{\\hfill}p{", IVA_los, fixed = T)
IVA_los <- gsub("Average intended length of stay:", "\\textbf{Average intended length of stay:}", IVA_los, fixed = T)
sink("tables/IVA_los.tex")
cat(IVA_los)
sink()

# ================================== International Visitor Expenditure ==================================

SQL_query <- "select max(Year) as survey_yr, right(max(Qtr), len(max(qtr))-5) as survey_qtr FROM [TRED].[Production].[vw_IVSSurveyMainHeader]"

survey_date <- sqlQuery(TRED, SQL_query)

survey_qtr <- survey_date$survey_qtr
survey_yr <- survey_date$survey_yr

end_month <- ifelse(survey_qtr == 1, "March", 
                    ifelse(survey_qtr == 2, "June", 
                           ifelse(survey_qtr == 3, "September", "December")))

IVE_title <- paste0("\\small International Visitor Expenditure$^*$", "$^2$ (year ending ", end_month, " ", survey_yr, ")")
sink("outputs/IVE_title.txt")
cat(IVE_title)
sink()

if (survey_qtr == '1') {
  qry_period <- paste("(", paste0("'", survey_yr, " 1',"), paste0("'", survey_yr-1, " 2',"), paste0("'", survey_yr-1, " 3',"), paste0("'", survey_yr-1, " 4'"), ")");
  qry_period_prev <- paste("(", paste0("'", survey_yr-1, " 1',"), paste0("'", survey_yr-2, " 2',"), paste0("'", survey_yr-2, " 3',"), paste0("'", survey_yr-2, " 4'"), ")")
} else if (survey_qtr == '2') {
  qry_period <- paste("(", paste0("'", survey_yr, " 1',"), paste0("'", survey_yr, " 2',"), paste0("'", survey_yr-1, " 3',"), paste0("'", survey_yr-1, " 4'"), ")");
  qry_period_prev <- paste("(", paste0("'", survey_yr-1, " 1',"), paste0("'", survey_yr-1, " 2',"), paste0("'", survey_yr-2, " 3',"), paste0("'", survey_yr-2, " 4'"), ")")
} else if (survey_qtr == '3') {
  qry_period <- paste("(", paste0("'", survey_yr, " 1',"), paste0("'", survey_yr, " 2',"), paste0("'", survey_yr, " 3',"), paste0("'", survey_yr-1, " 4'"), ")");
  qry_period_prev <- paste("(", paste0("'", survey_yr-1, " 1',"), paste0("'", survey_yr-1, " 2',"), paste0("'", survey_yr-1, " 3',"), paste0("'", survey_yr-2, " 4'"), ")")
} else {
  qry_period <- paste("(", paste0("'", survey_yr, " 1',"), paste0("'", survey_yr, " 2',"), paste0("'", survey_yr, " 3',"), paste0("'", survey_yr, " 4'"), ")");
    qry_period_prev <- paste("(", paste0("'", survey_yr-1, " 1',"), paste0("'", survey_yr-1, " 2',"), paste0("'", survey_yr-1, " 3',"), paste0("'", survey_yr-1, " 4'"), ")")
}

# --------- calculate the total expenditure ---------

SQL_query_0 <- paste("SELECT SurveyResponseID, CORNextYr, Year, Qtr, PopulationWeight, WeightedSpend, PopulationWeight*WeightedSpend as Expenditure 
                     FROM Production.vw_IVSSurveyMainHeader where Qtr in ", qry_period_prev)

ive_main_0 <- sqlQuery(TRED, SQL_query_0)

ive_median_0 <- svydesign(ids = ~1, weights = ~PopulationWeight, data = ive_main_0)
median_0 <- svyquantile(~WeightedSpend, design = ive_median_0, quantiles = 0.5)

ive_exp_sum_0 <- ive_main_0 %>%
  mutate(CountryGroup = CountryGroup(CORNextYr, shorten = TRUE, type = "IVSweights", OneChina_first = FALSE)) %>% 
  summarise("Total expenditure ($millions)" = sum(Expenditure)/1000000, 
            "Average expenditure per person per trip" = sum(Expenditure)/sum(PopulationWeight),
            "Median expenditure per person per trip" = median_0
  ) %>%
  gather(Summary, Prev)


SQL_query_1 <- paste("SELECT SurveyResponseID, CORNextYr, Year, Qtr, PopulationWeight, WeightedSpend, PopulationWeight*WeightedSpend as Expenditure 
                     FROM Production.vw_IVSSurveyMainHeader where Qtr in ", qry_period)
ive_main_1 <- sqlQuery(TRED, SQL_query_1)

ive_median_1 <- svydesign(ids = ~1, weights = ~PopulationWeight, data = ive_main_1)
median_1 <- svyquantile(~WeightedSpend, design = ive_median_1, quantiles = 0.5)

ive_exp_sum_1 <- ive_main_1 %>%
  mutate(CountryGroup = CountryGroup(CORNextYr, shorten = TRUE, type = "IVSweights", OneChina_first = FALSE)) %>% 
  summarise("Total expenditure ($millions)" = sum(Expenditure)/1000000, 
            "Average expenditure per person per trip" = sum(Expenditure)/sum(PopulationWeight),
            "Median expenditure per person per trip" = median_1
  ) %>%
  gather(Summary, Curr)
  
ive_exp_sum_final <- inner_join(ive_exp_sum_1, ive_exp_sum_0, by = "Summary") %>% 
  mutate("Growth (pa)" = paste0(round((Curr / Prev - 1) * 100), "%")) %>%
  mutate(Curr = format(dollar(round(Curr)), big.mark = ",")) %>% 
  select(-Prev) %>% 
  rename("  " = Summary, " "= Curr)
  
ive_exp_tab0 <- print(xtable(ive_exp_sum_final, align = "lp{4.85cm}p{1.3cm}p{1.35cm}",
                             caption = NULL, digits = 0,label = NULL, type = "latex"), 
                      floating = FALSE,
                      hline.after = NULL,
#                       hline.after = c(0, nrow(ive_exp_sum_final)),
                      include.rownames = FALSE
)


ive_exp_tab0 <- gsub("\\begin{tabular}", "\\begin{tabular}[t]", ive_exp_tab0, fixed = T)
ive_exp_tab0 <- gsub("}p{", "}>{\\hfill}p{", ive_exp_tab0, fixed = T)
ive_exp_tab0 <- gsub("Total expenditure (\\$millions)", "\\textbf{Total expenditure (\\$millions)}", ive_exp_tab0, fixed = T)
sink("tables/ive_exp_tab0.tex")
cat(ive_exp_tab0)
sink()


# --------- calculate the annual expenditure for the top markets ---------

ive_main_sum_0 <- ive_main_0 %>% 
  mutate(CountryGroup = CountryGroup(CORNextYr, shorten = TRUE, type = "IVSweights", OneChina_first = FALSE)) %>% 
  group_by(CountryGroup) %>%
  summarise("Last_Yr" = sum(Expenditure)/1000000)
  
ive_main_sum_1 <- ive_main_1 %>% 
  mutate(CountryGroup = CountryGroup(CORNextYr, shorten = TRUE, type = "IVSweights", OneChina_first = FALSE)) %>% 
  group_by(CountryGroup) %>%
  summarise("Current_Year" = sum(Expenditure)/1000000) %>% 
  inner_join (ive_main_sum_0, by = "CountryGroup") %>% 
  filter(substr(CountryGroup, 1, 4) != "Rest") %>%
  mutate("Growth (pa)" = paste0(round((Current_Year / Last_Yr - 1)*100), "%")) %>% 
  select(-Last_Yr) %>%
  arrange(-Current_Year) %>%
  top_n(Key_Market_spend, Current_Year) %>% 
  mutate("Current_Year" = format(dollar(round(Current_Year, 0)), big.mark = ",")) %>% 
  rename("Key international markets ($millions)" = CountryGroup) %>%
  clean_names()


IVE_tab1 <- print(xtable(ive_main_sum_1, align = "lp{4.85cm}p{1.3cm}p{1.35cm}", 
                         caption = NULL, digits = 0,label = NULL, type = "latex"), 
                  floating = FALSE, 
                  hline.after = NULL,
#                   hline.after = c(0, nrow(ive_main_sum_1)),
                  include.rownames = FALSE)


IVE_tab1 <- gsub("\\begin{tabular}", "\\begin{tabular}[t]", IVE_tab1, fixed = T)
IVE_tab1 <- gsub("}p{", "}>{\\hfill}p{", IVE_tab1, fixed = T)
IVE_tab1 <- gsub("Key international markets (\\$millions)", "\\textbf{Key international markets (\\$millions)}", IVE_tab1, fixed = T)
sink("tables/IVE_tab1.tex")
cat(IVE_tab1)
sink()


# --------- calculate total expenditure by purpose of visit ---------

# sql_pov_1 <- paste("SELECT bb.POV, bb.total_sp as total_sp_curr FROM
# (
# SELECT top 3 POV, round(sum(WeightedSpend*PopulationWeight)/1000000, 0) as total_sp
# FROM [TRED].[Production].[vw_IVSSurveyMainHeader]
# WHERE (POV like '%Holiday%' or POV like '%Visiting Friends%Relatives%' or POV = 'Business') and Qtr in ", qry_period,
# "group by POV
# order by total_sp desc
# ) bb ")
# 
# pov_1 <- sqlQuery(TRED, sql_pov_1)
# 
# sql_pov_0 <- paste("SELECT bb.POV, bb.total_sp as total_sp_prev FROM
# (
# SELECT top 3 POV, round(sum(WeightedSpend*PopulationWeight)/1000000, 0) as total_sp
# FROM [TRED].[Production].[vw_IVSSurveyMainHeader]
# WHERE (POV like '%Holiday%' or POV like '%Visiting Friends%Relatives%' or POV = 'Business') and Qtr in", qry_period_prev,
# "group by POV
# order by total_sp desc
# ) bb ")
# 
# pov_0 <- sqlQuery(TRED, sql_pov_0)




sql_pov_1 <- paste("SELECT POV, round(sum(WeightedSpend*PopulationWeight)/1000000, 0) as total_sp_curr 
                    FROM [TRED].[Production].[vw_IVSSurveyMainHeader]
                    WHERE  Qtr in ", qry_period, 
                   "group by POV 
                   order by total_sp_curr desc")

pov_1 <- sqlQuery(TRED, sql_pov_1) %>%
  mutate(POV = ifelse(POV %in% c('Holiday / vacation', 'Visiting friends / relatives', 'Business'), as.character(POV), "Other")) %>%
  group_by(POV) %>%
  summarise(total_sp_curr = sum(total_sp_curr))


sql_pov_0 <- paste("SELECT POV, round(sum(WeightedSpend*PopulationWeight)/1000000, 0) as total_sp_prev 
                    FROM [TRED].[Production].[vw_IVSSurveyMainHeader]
                    WHERE  Qtr in ", qry_period_prev, 
                   "group by POV 
                   order by total_sp_prev desc")

pov_0 <- sqlQuery(TRED, sql_pov_0) %>%
  mutate(POV = ifelse(POV %in% c('Holiday / vacation', 'Visiting friends / relatives', 'Business'), as.character(POV), "Other")) %>%
  group_by(POV) %>%
  summarise(total_sp_prev = sum(total_sp_prev))

ive_pov_sum_3 <- left_join(pov_1, pov_0, by = "POV") %>%
  filter(POV != "Other") %>%
  group_by(POV) %>%
  summarise(
    "Current_Year" = sum(total_sp_curr),
    "Growth (pa)" = paste0(round((sum(total_sp_curr) / sum(total_sp_prev) - 1) * 100), "%")) %>%
  arrange(-Current_Year) %>%
  mutate( Current_Year = format(dollar(round(Current_Year, 0)), big.mark = ","))

ive_pov_sum_Other <- left_join(pov_1, pov_0, by = "POV") %>%
  filter(POV == "Other") %>%
  group_by(POV) %>%
  summarise(
    "Current_Year" = sum(total_sp_curr),
    "Growth (pa)" = paste0(round((sum(total_sp_curr) / sum(total_sp_prev) - 1) * 100), "%")) %>%
  mutate( Current_Year = format(dollar(round(Current_Year, 0)), big.mark = ","))

ive_pov_sum <- rbind(ive_pov_sum_3, ive_pov_sum_Other) %>%
  rename('Total spend by purpose of visit ($millions)' = POV) %>%
  clean_names()




# ive_pov_sum <- inner_join(pov_1, pov_0, by = "POV") %>%
#   group_by(POV) %>%
#   summarise(
#     "Current_Year" = sum(total_sp_curr),
#     "Growth (pa)" = paste0(round((sum(total_sp_curr) / sum(total_sp_prev) - 1) * 100), "%")) %>%
#   arrange(-Current_Year) %>%
#   mutate( Current_Year = format(dollar(round(Current_Year, 0)), big.mark = ",")) %>%
#   rename('Total spend by purpose of visit' = POV) %>%
#   clean_names()

IVE_tab3 <- print(xtable(ive_pov_sum, align = "lp{4.85cm}p{1.3cm}p{1.35cm}", 
                         caption = NULL, digits = 0,label = NULL, type = "latex"), 
                  floating = FALSE, 
                  hline.after = NULL,
#                   hline.after = c(0, nrow(ive_pov_sum)),
                  include.rownames = FALSE)


IVE_tab3 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",IVE_tab3, fixed = T)
IVE_tab3 <- gsub("}p{", "}>{\\hfill}p{", IVE_tab3, fixed = T)
IVE_tab3 <- gsub("Total spend by purpose of visit (\\$millions)","\\textbf{Total spend by purpose of visit (\\$millions)}", IVE_tab3, fixed = T)
IVE_tab3 <- gsub("\n\\end{tabular}","\n\\multicolumn{3}{p{8.25cm}}{$^*$Excludes international airfares and individuals whose purpose of visit to New Zealand was to attend a recognised educational institute, and are foreign-fee paying students.}\\\\ \n\\end{tabular}",IVE_tab3, fixed = T)

sink("tables/IVE_tab3.tex")
cat(IVE_tab3)
sink()

ive_note_1 <- paste("$^*$Excludes international airfares and individuals whose purpose of visit to New Zealand was 
                    to attend a recognised educational institute, and are foreign-fee paying students.")

sink("outputs/ive_note_1.txt")
cat(ive_note_1)
sink()


# ================================== Trip aboard by New Zealanders ==================================

NZ_out_sum <- ImportTS(TRED, "Short-term NZ traveller departure totals (Monthly)",
                       where = paste("TimePeriod > '", qry_starting_date, "'"))

NZ_out_sum_end_date = max(NZ_out_sum$TimePeriod)


Annual_total_outbound <- NZ_out_sum %>%
  filter(ClassificationValue == "Actual Counts") %>%
  mutate(Year_Period = ifelse(TimePeriod > NZ_out_sum_end_date - years(1), "Current", 
                              ifelse(TimePeriod > NZ_out_sum_end_date - years(2), "Last", "Earlier"))) %>%
  group_by(ClassificationValue) %>%
  summarise(
    Trips = sum(Value[Year_Period == "Current"]),
    "Growth (pa)" = paste0(round((Trips / sum(Value[Year_Period == "Last"]) - 1) * 100), "%")) %>%
  mutate(ClassificationValue.1 = 'Annual Outbound Departures', Trips = format(round(Trips/1000, 0)*1000, big.mark = ",")) %>%
  rename(' ' = ClassificationValue.1) %>%
  select(4, 2, 3) %>%
  clean_names()

NZ_out_tab1 <- print(xtable(Annual_total_outbound, align = "lp{4.7cm}p{1.1cm}p{1.7cm}", 
                            caption = NULL, digits = 0,label = NULL, type = "latex"), 
                     floating = FALSE, 
                     hline.after = NULL,
                     #                      hline.after = c(0, nrow(Annual_total_outbound)),
                     include.rownames = FALSE)

NZ_out_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",NZ_out_tab1, fixed = T)
NZ_out_tab1 <- gsub("}p{", "}>{\\hfill}p{", NZ_out_tab1, fixed = T)
NZ_out_tab1 <- gsub("Annual Outbound Departures","\\textbf{Annual Outbound Departures}", NZ_out_tab1, fixed = T)
sink("tables/NZ_out_tab1.tex")
cat(NZ_out_tab1)
sink()


#-------------------------------------------- Key destination countries ---------------------------------


NZ_out <- ImportTS(TRED, "Short-term NZ traveller departures by EVERY country of main dest and purpose (Monthly)",
                   where = paste("TimePeriod > '", qry_starting_date, "'"))

Report_end_date_NZ_out = max(NZ_out$TimePeriod)

NZ_out_title <- paste0("\\small Trips abroad by New Zealanders$^5$ (year ending ", months(Report_end_date_NZ_out), " ", year(Report_end_date_NZ_out), ")")

sink("outputs/NZ_out_title.txt")
cat(NZ_out_title)
sink()

#--------------------

# NZ_out_total <- NZ_out %>%
#   rename("Country" = ClassificationValue, "Trip_Type" = ClassificationValue.1) %>%
#   filter(Country != toupper(Country) &
#   #%in% c("ASIA", "AMERICAS", "EUROPE", "OCEANIA", "AFRICA AND THE MIDDLE EAST", "TOTAL ALL COUNTRIES OF MAIN DESTINATION")) & 
#            Trip_Type == "TOTAL ALL TRAVEL PURPOSES") %>%
#   mutate(Year_Period = ifelse(TimePeriod > Report_end_date_NZ_out - years(1), "Current", 
#                               ifelse(TimePeriod > Report_end_date_NZ_out - years(2), "Last", "Earlier"))) %>%
#   mutate(Country = ifelse(CountryGrouped == "China", "China", 
#                           ifelse(CountryGrouped == "Australia", "Australia", 
#                                  ifelse(CountryGrouped == "UK", "UK", 
#                                         ifelse(CountryGrouped == "Japan", "Japan", 
#                                                ifelse(CountryGrouped == "Germany", "Germany", 
#                                                       ifelse(CountryGrouped == "Canada", "Canada", 
#                                                              ifelse(CountryGrouped == "Korea, Republic of", "Korea, Republic of", 
#                                      ifelse(CountryGrouped == "USA", "USA", as.character(Country)))))))))) %>%
#   group_by(Country) %>%
#   summarise( Trips = sum(Value[Year_Period == "Current"]),
#     " " = paste0(round((Trips / sum(Value[Year_Period == "Last"]) - 1) * 100), "%")
#     ) %>%
#   top_n(Key_outbound_destination, Trips) %>%
#   arrange(-Trips) %>%
#   mutate(Trips = format(round(Trips/1000, 0)*1000, big.mark = ",")) %>%
#   rename('Countries visited by New Zealanders' = Country, ' ' = Trips) %>%
#   clean_names()
# 

NZ_out_data <- NZ_out %>%
  rename("Country" = ClassificationValue, "Trip_Type" = ClassificationValue.1) %>%
  filter(Country != toupper(Country) &
           #%in% c("ASIA", "AMERICAS", "EUROPE", "OCEANIA", "AFRICA AND THE MIDDLE EAST", "TOTAL ALL COUNTRIES OF MAIN DESTINATION")) & 
           Trip_Type == "TOTAL ALL TRAVEL PURPOSES") %>%
  mutate(Year_Period = ifelse(TimePeriod > Report_end_date_NZ_out - years(1), "Current", 
                              ifelse(TimePeriod > Report_end_date_NZ_out - years(2), "Last", "Earlier"))) %>%
  mutate(Country = ifelse(CountryGrouped == "China", "China", 
                          ifelse(CountryGrouped == "Australia", "Australia", 
                                 ifelse(CountryGrouped == "UK", "UK", 
                                        ifelse(CountryGrouped == "Japan", "Japan", 
                                               ifelse(CountryGrouped == "Germany", "Germany", 
                                                      ifelse(CountryGrouped == "Canada", "Canada", 
                                                             ifelse(CountryGrouped == "Korea, Republic of", "Korea, Republic of", 
                                                                    ifelse(CountryGrouped == "USA", "USA", as.character(Country))))))))))


Top_Dest <- NZ_out_data %>%
  filter(Year_Period == "Current") %>%
  group_by(Country) %>%
  summarise("Total_trips" = sum(Value)) %>%
  top_n(Key_outbound_destination, Total_trips) %>%
  arrange(-Total_trips) %>%
  select(Country) %>%
  unique()

Top_Dest_list <- rbind(Top_Dest, c("Other"))

NZ_out_total_temp <- NZ_out_data  %>%
  mutate(Country = ifelse(Country %in% Top_Dest$Country, Country, "Other")) %>%
  group_by(Country) %>%
  summarise( Trips = sum(Value[Year_Period == "Current"]),
             " " = paste0(round((Trips / sum(Value[Year_Period == "Last"]) - 1) * 100), "%")
  )
  
NZ_out_total <- left_join(Top_Dest_list, NZ_out_total_temp, by = "Country") %>%
  mutate(Trips = format(round(Trips/1000, 0)*1000, big.mark = ",")) %>%
  rename('Countries visited by New Zealanders' = Country, ' ' = Trips) %>%
  clean_names()

NZ_out_tab2 <- print(xtable(NZ_out_total, align = "lp{4.7cm}p{1.1cm}p{1.7cm}", 
                            caption = NULL, digits = 0,label = NULL, type = "latex"), 
                     floating = FALSE, 
                     hline.after = NULL,
#                      hline.after = c(0, nrow(NZ_out_total)),
                     include.rownames = FALSE)


NZ_out_tab2 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",NZ_out_tab2, fixed = T)
NZ_out_tab2 <- gsub("}p{", "}>{\\hfill}p{", NZ_out_tab2, fixed = T)
NZ_out_tab2 <- gsub("Countries visited by New Zealanders","\\textbf{Countries visited by New Zealanders}", NZ_out_tab2, fixed = T)
sink("tables/NZ_out_tab2.tex")
cat(NZ_out_tab2)
sink()


# ================================== Tourism Forecast ==================================
# Important note! The structure of 2015-2021 forecasts is different to previous forecasts, as follows:
#   1. 'Country' without filtering will double count due to the inclusion of 'All' and multiple 'Other...' categories.
#     Hence, for all country estimates, 'Country' must be filtered to include only the 'All' category.
#     For key market estimates, 'Country' must filter out 'All' and the three 'Other...' categories.
#   2. The measurement unit for spend in 2015-2021 forecasts is actual dollars, but for previous forecasts in $millions.
#     Hence, expenditure estimates must be rebased to $millions or $billions as required.


Fcst_query <- paste("SELECT * FROM Production.vw_NZTFSurveyMainHeader where Year >= ", year(qry_starting_date),
                    "order by ForecastYear, Country, Year")

Fcst <- sqlQuery(TRED, Fcst_query)

# fcst_2015 <- Fcst %>%
#   filter(Year == 2015, ForecastYear == 2015)
# 
# write.csv(fcst_2015, file = "data/fcst_2015.csv")

Fcst_year <- as.numeric(max(Fcst$ForecastYear))
End_year <- as.numeric(max(Fcst$Year))

fcst_title <- paste0("\\small Tourism Forecast$^7$ (forecast period ", Fcst_year, " to ", End_year, ")")
sink("outputs/fcst_title.txt")
cat(fcst_title)
sink()

#---------------------------------------

Fcst_sum_Arrivals <- Fcst %>%
#   group_by(ForecastYear) %>% 
  filter(ForecastYear == Fcst_year & (Year == End_year | Year == Fcst_year-1) & Country == "All") %>%
  summarise(Total = "Total visitor arrivals (millions)",
             Forecast = round(sum(TotalVisitorArrivals[Year == End_year])/1000000, 1),
             'Growth (pa)' = percent(CAGR(sum(TotalVisitorArrivals[Year == End_year])/sum(TotalVisitorArrivals[Year == Fcst_year - 1]), 
                                 (End_year - Fcst_year + 1))/100))

Fcst_sum_VisitorDays <- Fcst %>%
  #   group_by(ForecastYear) %>% 
  filter(ForecastYear == Fcst_year & (Year == End_year | Year == Fcst_year-1) & Country == "All") %>%
  summarise(Total = "Total visitor days (millions)",
             Forecast = round(sum(TotalVisitorDays[Year == End_year])/1000000, 1),
             'Growth (pa)' = percent(CAGR(sum(TotalVisitorDays[Year == End_year])/sum(TotalVisitorDays[Year == Fcst_year - 1]), 
                                 (End_year - Fcst_year + 1))/100))             
             
Fcst_sum_VisitorSpend <- Fcst %>%
  #   group_by(ForecastYear) %>% 
  filter(ForecastYear == Fcst_year & (Year == End_year | Year == Fcst_year - 1) & Country == "All") %>%
  summarise(Total = "Total visitor expenditure ($billions)",
             Forecast = round(sum(TotalVisitorSpend[Year == End_year])/10^9, 1),
             'Growth (pa)' = percent(CAGR(sum(TotalVisitorSpend[Year == End_year])/sum(TotalVisitorSpend[Year == Fcst_year - 1]), 
                                 (End_year - Fcst_year + 1))/100))
  

Fcst_sum_2_rpt <- bind_rows(Fcst_sum_Arrivals, Fcst_sum_VisitorDays, Fcst_sum_VisitorSpend) %>%
  rename(' ' = Total)

Fcst_tab1 <- print(xtable(Fcst_sum_2_rpt, align = "lp{4.7cm}p{1.1cm}p{1.7cm}", 
                          caption = NULL, digits = 1,label = NULL, type = "latex"), 
                   floating = FALSE, 
                   hline.after = NULL,
#                    hline.after = c(0, nrow(Fcst_sum_2_rpt)),
                   include.rownames = FALSE)


Fcst_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Fcst_tab1, fixed = T)
Fcst_tab1 <- gsub("Forecast", End_year, Fcst_tab1, fixed = T)
Fcst_tab1 <- gsub("}p{", "}>{\\hfill}p{", Fcst_tab1, fixed = T)
# Fcst_tab1 <- gsub("CAGR","CAGR$^{\\text \\dagger}$",Fcst_tab1, fixed = T)
# Fcst_tab1 <- gsub("\\end{tabular}","\n\\multicolumn{3}{p{8.25cm}}{$^{\\text \\dagger}$CAGR = compound annual growth rate.}\\\\ \\end{tabular}",Fcst_tab1, fixed = T)

sink("tables/Fcst_tab1.tex")
cat(Fcst_tab1)
sink()


# --------- Key International Market Forecast ---------

Fcst_sum_key <- Fcst %>%
  data.frame() %>%
  filter(Year == End_year & !Country %in% c("All") & substr(Country, 1, 5) != "Other") %>%  
  group_by(Country) %>%
  summarise( Visits = sum(TotalVisitorArrivals),
             "Spend ($m)" = format(round(sum(TotalVisitorSpend[Year == End_year]) / 10^6, 0), big.mark = ",")
  ) %>%
  top_n(Key_Market_Fcst, Visits) %>%
  arrange(-Visits) %>%
  mutate(Visits = format(round(Visits/1000, 0)*1000, big.mark = ","),
         Country = ifelse(Country == 'US', 'USA', as.character(Country))) 

names(Fcst_sum_key)[names(Fcst_sum_key) == 'Country'] <- paste("Key international markets in", End_year)


text_title <- paste('"', 'Key international markets in', End_year, '"')

Fcst_tab2 <- print(xtable(Fcst_sum_key, align = "lp{4.7cm}p{1.1cm}p{1.7cm}", 
                          caption = NULL, digits = 0,label = NULL, type = "latex"), 
                   floating = FALSE, 
                   hline.after = NULL,
#                    hline.after = c(0, nrow(Fcst_sum_key)),
                   include.rownames = FALSE)

Fcst_tab2 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Fcst_tab2, fixed = T)
Fcst_tab2 <- gsub("}p{", "}>{\\hfill}p{", Fcst_tab2, fixed = T)
# Fcst_tab2 <- gsub("Key international markets", "text_title", Fcst_tab2, fixed = T)
Fcst_tab2 <- gsub(substr(Fcst_tab2, str_locate(Fcst_tab2, "Key international markets in")[1], str_locate(Fcst_tab2, "Key international markets in")[2] + 5),
                  paste0("\\textbf{", substr(Fcst_tab2, str_locate(Fcst_tab2, "Key international markets in")[1], str_locate(Fcst_tab2, "Key international markets in")[2] + 5), "}"), Fcst_tab2, fixed = T)
sink("tables/Fcst_tab2.tex")
cat(Fcst_tab2)
sink()

    
# ================================== Economic Contribution ==================================

# --------- Tourism Export ---------

EC_exp <- ImportTS(TRED, "Summary of Tourism Expenditure by type of tourist (ANZSIC06) (Annual-Mar)", 
                   where = paste("TimePeriod > '", qry_starting_date, "'"))

ec_end_date <- max(EC_exp$TimePeriod)
ec_year <- year(ec_end_date)

ec_title <- paste0("\\small Economic Contribution$^4$ (year ending March ", ec_year, ")")
sink("outputs/ec_title.txt")
cat(ec_title)
sink()

# calculate the percentage of international tourism expenditure

EC_exp_share <- EC_exp %>%
  filter(TimePeriod == ec_end_date & ClassificationValue %in% c('International tourism as a percentage of total exports')) %>%
  select(Value)

EC_exp_growth <- EC_exp %>%
  filter(TimePeriod == ec_end_date & 
           ClassificationValue %in% c('Annual percentage change in international tourism expenditure', 'Annual percentage change in domestic tourism expenditure', 
                                      'Annual percentage change in total tourism expenditure')) %>%
  select(ClassificationValue, Value) %>%
  mutate(Class = ifelse(ClassificationValue == "Annual percentage change in domestic tourism expenditure","Domestic", 
                        ifelse(ClassificationValue == "Annual percentage change in international tourism expenditure","international", "Total"))) %>% 
  select(Class, Value) %>% 
  mutate(Value = percent(Value/100)) %>% 
  rename('Growth (pa)' = Value)
  
  
EC_exp_total <- EC_exp %>%
  filter(TimePeriod == ec_end_date & 
           ClassificationValue %in% c('Domestic tourism expenditure', 'International tourism expenditure', 'Total tourism expenditure')) %>%
  select(ClassificationValue, Value) %>%
  mutate(Class = ifelse(ClassificationValue == "Domestic tourism expenditure","Domestic", 
                        ifelse(ClassificationValue == "International tourism expenditure","international", "Total"))) %>% 
  select(Class, Value) %>% 
  mutate(Value = format(round(Value/1000, 1), big.mark = ",")) %>% 
  rename('Expenditure ($b)' = Value) %>% 
  inner_join(EC_exp_growth, by = "Class")


inter_exp <- EC_exp_total %>%
  filter(Class == 'international') %>%
  select(starts_with('Expenditure'))

EC_exp_total <- EC_exp_total %>%
  rename("Tourism market" = Class)

EC_exp_tab1 <- print(xtable(EC_exp_total, align = "lp{4.45cm}p{1.7cm}p{1.35cm}", 
                            caption = NULL, digits = 0,label = NULL, type = "latex"), 
                     floating = FALSE, 
                     hline.after = NULL,
#                      hline.after = c(0, nrow(EC_exp_total)),
                     include.rownames = FALSE)


EC_exp_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",EC_exp_tab1, fixed = T)
EC_exp_tab1 <- gsub("}p{", "}>{\\hfill}p{", EC_exp_tab1, fixed = T)
EC_exp_tab1 <- gsub("Tourism market","\\textbf{Tourism market}", EC_exp_tab1, fixed = T)
EC_exp_tab1 <- gsub("international","International$^+$",EC_exp_tab1, fixed = T)
EC_exp_tab1 <- gsub("\\end{tabular}","\\multicolumn{3}{p{7.5cm}}{$^+$Includes international airfares paid to New Zealand carriers.}\\ \n\\end{tabular}",EC_exp_tab1, fixed = T)

sink("tables/EC_exp_tab1.tex")
cat(EC_exp_tab1)
sink()

EC_comment_1 <- paste("International tourist expenditure accounted for", paste0("\\$", inter_exp), "billions or ", paste0(EC_exp_share, "\\%"), "of New Zealand total export earnings.")
sink("outputs/EC_comment_1.txt")
cat(EC_comment_1)
sink()

# --------- Tourism GDP Contribution ---------


EC_GDP <- ImportTS(TRED, "Tourism expenditure by component (ANZSIC06) (Annual-Mar)")

inter_gdp1 <- EC_GDP %>% 
  filter(TimePeriod == max(TimePeriod) & ClassificationValue %in% c('Direct tourism value added')) %>%
  select(Value)

inter_gdp1 <-  round(inter_gdp1/1000, 1)
  
inter_gdp2_0 <- EC_GDP %>% 
  filter(TimePeriod == max(TimePeriod) & ClassificationValue %in% c('Direct tourism value added as a percentage of total industry contribution to GDP')) %>%
  select(Value)

inter_gdp2 <- sprintf("%.01f", inter_gdp2_0)


inter_gdp3 <- EC_GDP %>% 
  filter(TimePeriod == max(TimePeriod) & ClassificationValue %in% c('Indirect tourism value added')) %>%
  select(Value)
  
inter_gdp3 <-  round(inter_gdp3/1000, 1)

inter_gdp4_0 <- EC_GDP %>% 
  filter(TimePeriod == max(TimePeriod) & ClassificationValue %in% c('Indirect tourism value added as a percentage of total industry contribution to GDP')) %>%
  select(Value)

inter_gdp4 <- sprintf("%.01f", inter_gdp4_0)

EC_comment_2 <- paste0("Tourism directly contributes ", "\\$", inter_gdp1, " billions (or ", inter_gdp2, "\\%",
                      ") to New Zealand total GDP. A further ", "\\$", round(inter_gdp3, 1), " billions (or ", inter_gdp4, "\\%",
                      ") is indirectly contributed. When comparing tourism to other industries, the direct contribution should be used.")

sink("outputs/EC_comment_2.txt")
cat(EC_comment_2)
sink() 


# --------- Tourism Employment ---------

EC_emp <- ImportTS(TRED, "Summary of Tourism Employment (ANZSIC06) (Annual-Mar)")

inter_emp1 <- EC_emp %>% 
  filter(TimePeriod == max(TimePeriod) & ClassificationValue %in% c('FTE persons directly employed in tourism in New Zealand')) %>%
  select(Value)
inter_emp1 <-  format(inter_emp1, big.mark = ",")

inter_emp2_0 <- EC_emp %>% 
  filter(TimePeriod == max(TimePeriod) & ClassificationValue %in% c('FTE persons directly employed in tourism as a percentage of total employment in New Zealand')) %>%
  select(Value)

inter_emp2 <- sprintf("%.01f", inter_emp2_0)

EC_comment_3 <- paste("Tourism directly supports",inter_emp1, "full-time equivalent jobs", paste0("(", inter_emp2, "\\%"),
                      "of the total workforce in New Zealand).")

sink("outputs/EC_comment_3.txt")
cat(EC_comment_3)
sink() 


# ================================== Commercial Accommodation Stats ==================================


ACCOM <- ImportTS(TRED, "Actual by Accommodation by Type by Variable (Monthly)",
                  where = paste("TimePeriod > '", qry_starting_date, "'"))

accom_report_end_date <- max(ACCOM$TimePeriod)

accom_title <- paste0("\\small Commercial Accommodation$^3$ (year ending ", 
                     months(accom_report_end_date), " ", year(accom_report_end_date), ")")
sink("outputs/accom_title.txt")
cat(accom_title)
sink()

# --------- Guest Nights Summary ---------

ACCOM_type_sum_0 <- ACCOM %>%
  filter(ClassificationValue %in% c('Hotels', 'Motels', 'Backpackers', 'Holiday parks', 'Total') & 
           ClassificationValue.1 %in% c('Number of guest nights')) %>%
  mutate(Year_Period = ifelse(TimePeriod > accom_report_end_date-years(1), "Current", 
                              ifelse(TimePeriod > accom_report_end_date-years(2), "Last", "Earlier"))) %>%
  group_by(ClassificationValue) %>%
  summarise(
     Nights = sum(Value[Year_Period == "Current"]),
    "Growth (pa)" = paste0(round((Nights / sum(Value[Year_Period == "Last"]) - 1) * 100), "%"))

ACCOM_type_sum_1 <- ACCOM_type_sum_0 %>%
  filter(!ClassificationValue %in% c('Total')) %>%
  arrange(-Nights) 
  
ACCOM_type_sum_total <- ACCOM_type_sum_0 %>% 
  filter(ClassificationValue %in% c('Total'))

ACCOM_type_sum <- rbind(ACCOM_type_sum_1, ACCOM_type_sum_total) %>%
  mutate(Nights = format(round(Nights/1000, 0)*1000, big.mark = ",")) %>%
  rename('Accom_Type' = ClassificationValue) 


ACCOM_type_list <- ACCOM_type_sum %>%
  select(1) %>%
  unique() %>%
  data.frame()
  
ACCOM_type_sum <- ACCOM_type_sum %>%
  clean_names()

Accom_tab1 <- print(xtable(ACCOM_type_sum, align = "lp{4.5cm}p{1.3cm}p{1.7cm}", 
                           caption = NULL, digits = 0,label = NULL, latex.environments = ""), 
                    floating = FALSE, 
                    hline.after = NULL,
#                     hline.after = c(0, nrow(ACCOM_type_sum)),
                    include.rownames = FALSE)

Accom_tab1 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Accom_tab1, fixed = T)
Accom_tab1 <- gsub("}p{", "}>{\\hfill}p{", Accom_tab1, fixed = T)
Accom_tab1 <- gsub("Accom Type","\\textbf{Accommodation Type}", Accom_tab1, fixed = T)
sink("tables/Accom_tab1.tex")
cat(Accom_tab1)
sink()


# --------- Occupancy Rates Summary ---------

ACCOM_p_sum <- ACCOM %>%
  filter(ClassificationValue %in% c('Holiday parks', 'Backpackers', 'Motels', 'Hotels', 'Total') & 
           ClassificationValue.1 %in% c('Occupancy rate (percent)')) %>%
  select(1, 2, 3, 4) %>%
  rename('Accom_Type' = ClassificationValue, 'Class' = ClassificationValue.1, 'Num_of_Stay' = Value) %>%
  mutate(Year_Period = ifelse(TimePeriod > accom_report_end_date-years(1), "Current", 
                              ifelse(TimePeriod > accom_report_end_date-years(2), "Last", "Earlier"))) %>%
  filter(TimePeriod == max(TimePeriod) | TimePeriod == max(TimePeriod) - years(1)) %>%
  select(2, 4, 5) %>%
  group_by(Accom_Type) %>% 
  summarise(
    'This Month' = paste0(sum(Num_of_Stay[Year_Period == "Current"]), "%"),
    'Month Last Year' = paste0(sum(Num_of_Stay[Year_Period == "Last"]), "%")) 

ACCOM_p_sum_1 <- left_join(ACCOM_type_list, ACCOM_p_sum, by = "Accom_Type") %>%
  rename('Occupancy Rates' = Accom_Type)
  

Accom_tab2 <- print(xtable(ACCOM_p_sum_1, align = "lp{4.5cm}p{1.3cm}p{1.7cm}", 
                           caption = NULL, digits = 0,label = NULL, type = "latex"), 
                    floating = FALSE, 
                    hline.after = NULL,
#                     hline.after = c(0, nrow(ACCOM_p_sum)),
                    include.rownames = FALSE)


Accom_tab2 <- gsub("\\begin{tabular}","\\begin{tabular}[t]",Accom_tab2, fixed = T)
Accom_tab2 <- gsub("}p{", "}>{\\hfill}p{", Accom_tab2, fixed = T)
Accom_tab2 <- gsub("Occupancy Rates","\\textbf{Occupancy Rates}", Accom_tab2, fixed = T)
#  Accom_tab2 <- gsub("}p{","}rp{", Accom_tab2, fixed = T)
#  Accom_tab2 <- gsub("{p{","{lp{", Accom_tab2, fixed = T)
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

RTE <- sqlQuery(TRED, RTE_query)

rte_end_date <- max(RTE$YearEndMarch)

rte_title <- paste0("\\small Regional Tourism Estimates$^6$ (year ending March ", rte_end_date, ")")
sink("outputs/rte_title.txt")
cat(rte_title)
sink()

RTE_1 <- reshape(RTE, idvar = 'RTO', v.names = 'Spend', timevar = 'Type', direction = 'wide')
RTE_1 <-data.table(RTE_1)


RTE_Data <- RTE_1 %>%
  mutate(RTO = ifelse(str_detect(RTO, "RTO"), substr(RTO, 1, str_length(RTO)-4), as.character(RTO))) %>%
  mutate("Total" = (Spend.Domestic + Spend.International))

RTE_Total_Spend <- RTE_Data %>%
  select(Total) %>%
  sum()

RTE_sum <- RTE_Data %>%
  group_by(RTO, YearEndMarch) %>%
  summarise("International" = sum(Spend.International),
            "Domestic" = sum(Spend.Domestic),
            "Total_0" = sum(Total)
            ) %>%
  data.frame()

Top_RTO <- RTE_sum %>%
  top_n(Key_region, Total_0) %>%
  arrange(-Total_0) %>%
  select(RTO) %>%
  unique()

Top_RTO_list <- rbind(Top_RTO, c("Other"))

RTE_sum_temp <- RTE_sum %>%
  mutate(RTO = ifelse(RTO %in% Top_RTO$RTO, RTO, "Other")) %>%
  group_by(RTO, YearEndMarch) %>%
  summarise("International" = format(round(sum(International), digits = 0), big.mark = ","),
            "Domestic" = format(round(sum(Domestic), digits = 0), big.mark = ","),
            "Total" = format(round(sum(Total_0), digits = 0), big.mark = ","),
            "Market Share" = percent(round(sum(Total_0)/RTE_Total_Spend, digits = 2))
  )


RTE_sum_final <- left_join(Top_RTO_list, RTE_sum_temp, by = "RTO") %>%
  select(-YearEndMarch) %>%
  rename("RTO ($millions)" = RTO)


RTE_tab1 <- print(xtable(RTE_sum_final, align = "lp{2cm}p{1.45cm}p{1cm}p{0.9cm}p{1.35cm}", 
                         caption = NULL, digits = 0,label = NULL, type = "latex"), 
                  floating = FALSE, 
                  hline.after = NULL,
#                   hline.after = c(0, nrow(RTE_sum_1)),
                  include.rownames = FALSE)

RTE_tab1 <- gsub("\\begin{tabular}", "\\begin{tabular}[t]", RTE_tab1, fixed = T)
RTE_tab1 <- gsub("}p{", "}>{\\hfill}p{", RTE_tab1, fixed = T)
RTE_tab1 <- gsub("RTO (\\$millions)","\\textbf{RTO (\\$millions)}", RTE_tab1, fixed = T)
sink("tables/RTE_tab1.tex")
cat(RTE_tab1)
sink()



# ==================== Data Sources ==========================

Data_S_1 <- paste("$^1$Statistics New Zealand: International Travel and Migration. 'Business' excludes conferences.")
sink("outputs/Data_S_1.txt")
cat(Data_S_1)
sink() 

Data_S_2 <- paste("$^2$Statistics New Zealand: International Visitor Survey. 'Business' excludes conferences.")
sink("outputs/Data_S_2.txt")
cat(Data_S_2)
sink()

Data_S_3 <- paste("$^3$Statistics New Zealand: Accommodation Survey.")
sink("outputs/Data_S_3.txt")
cat(Data_S_3)
sink()

Data_S_4 <- paste("$^4$Statistics New Zealand: Tourism Satellite Account.")
sink("outputs/Data_S_4.txt")
cat(Data_S_4)
sink()


Data_S_5 <- paste("$^5$Statistics New Zealand: International Travel and Migration.")
sink("outputs/Data_S_5.txt")
cat(Data_S_5)
sink()

Data_S_6 <- paste("$^6$Statistics New Zealand: Regional Tourism Estimates (RTO = Regional Tourism Organisation).")
sink("outputs/Data_S_6.txt")
cat(Data_S_6)
sink()

Data_S_7 <- paste0("$^7$MBIE and NZIER: New Zealand Tourism Forecasts ", Fcst_year, " to ", End_year, ".")
sink("outputs/Data_S_7.txt")
cat(Data_S_7)
sink()

Data_S_8 <- paste("This paper was prepared by the Ministry of Business, Innovation and Employment. For further information visit the 
                  tourism data section of the Ministry's website url{http://www.med.govt.nz/sectors-industries/tourism}. 
                  Key Tourism Statistics are updated regularly throughout the month as data is released."
                  )

Data_S_8 <- gsub("url","\\url",Data_S_8, fixed = T)

sink("outputs/Data_S_8.txt")

cat(Data_S_8)
sink() 
