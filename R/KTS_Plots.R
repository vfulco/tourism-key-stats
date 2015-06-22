#######################################################################################################
## Project Title:  Key Tourism Statistics - Part 2: Generate one-page plots
## 
## Project file location:  
##    P:\OTSP\Comms\KeyStats
##
## Project Objective:    
##    Key Tourism Statistics is one-page summary of latest NZ tourism statistics including forcast. 
##    The second part is to provide visual version of different Statistics.
##
## Authors: Peter Ellis and George Fan
##    
## Data source: IVS, RTE, TSA, and Forecast data in TRED
##
## Project Status: to review
##
## Document history:  
##    Created by George Fan on 29 April 2015
##     
## Reviewed by 
##  1)  Pete McMillen, 11 May 2015 - corrected grammar/typos, specified legend titles, $ format relevant y-axes.
##  2)  Pete McMillen, 22 June 2015 - final review of final changes, confirmed OK
##
## Note: For TSA plot, given its latest date is not available in TRED, Manual Option has been deployed. Once the data is available in TRED, 
##       this option should be switched to the Automatic Option, which is currently commented out.
##
##
########################################################################################################



#================ 0 Includes---------------------------------

library(lubridate)
library(RODBC)
library(mbiedata) # shorthand to get the RTEs, should use TRED
library(mbie)
library(mbiemaps)
library(dplyr)
library(ggplot2)
library(extrafont)
library(mbieDBmisc)
library(RColorBrewer)
library(Cairo)
library(gridExtra)
library(magrittr)
library(utils)
library(xtable)
library(data.table)
library(stringr)
library(scales)

#========================== 1 Global parameters and DB Connection =========================


TRED <- odbcConnect("TRED_Prod")

base_size <- 12
TheFont <- "Calibri"

growth_period <- 1

Fcst_start_Year <- year(today())

#========================== 2 RTE Plot  =========================

# ----------- load map data

data(rto_gg)

# ------------- RTE Query -----------------

RTE_query <- "  select sp.Type, d.L1Description as RTO, mh.YearEndMarch, sum(sp.Spend) as Spend from 
Production.vw_RTESurveyMainHeader mh
left join Production.vw_RTESpend sp
on mh.SurveyResponseID = sp.SurveyResponseID
left join
(
Select distinct L1Description, L2Description from Classifications.vw_ClassificationLevels cl
where cl.L2ClassificationName = 'MBIEInt_TAMOD_1_Geography' and 
cl.L1ClassificationName = 'MBIEPub_RTO_1_Geography'
) d
on d.L2Description = mh.TA_MOD
group by mh.YearEndMarch, sp.Type, d.L1Description, d.L2Description
order by mh.YearEndMarch"

RTE <- sqlQuery(TRED, RTE_query)

LastYear <- max(RTE$YearEndMarch)
# FirstYear <- min(RTE$YearEndMarch)
FirstYear <- LastYear - growth_period



RTE_sum <- RTE %>%
  group_by(RTO, Type, YearEndMarch)  %>%
  summarise(Spend = sum(Spend)) %>%
  group_by(RTO, Type) %>%
  summarise(
    LatestSpend = Spend[YearEndMarch == LastYear],
    cagr        = CAGR(ratio = Spend[YearEndMarch == LastYear] / Spend[YearEndMarch == FirstYear], period = LastYear - FirstYear)
  ) %>%
  data.frame()

RTE_combined <- merge(rto_gg, RTE_sum, all.x = TRUE, by = "RTO") 
spend_only <- unique(RTE_combined [ , c("RTO", "long.centre", "lat.centre", "LatestSpend", "Type")])

RTEMap <- ggplot() +
  geom_polygon(data = RTE_combined, aes(x = long, y = lat, group = group, fill = cagr / 100), colour = "grey75", size = 0.2) + # size refers to linewidth
  geom_point(data = spend_only, aes(x = long.centre, y = lat.centre, size = LatestSpend), shape = 1) +
  facet_wrap(~Type) +
  theme_nothing(6, base_family = TheFont) +
  scale_size(paste0("Spend in ", LastYear, " ($m)"), label = comma, range = c(1, 10)) +
  scale_fill_gradientn(paste0("Average growth ",  FirstYear, " to ", LastYear) %>% wrap(18), 
                       colours = brewer.pal(9, "Spectral"),
                       label = percent) +
  theme(legend.text = element_text(lineheight = 0.2), 
        legend.key.height = unit(0.3, "cm")) +
  coord_map() +
  ggtitle("Regional tourism spend distribution (international compared to domestic)")



#========================== 3 International Vistor Expenditure: Actuals and Forecast ==========================

SQL_qry_ive <- paste("SELECT SurveyResponseID, CORNextYr, Year, Qtr, RIGHT(Qtr, 1) as Qtr_Num, POV, PopulationWeight, WeightedSpend, PopulationWeight*WeightedSpend as Expenditure 
                     FROM Production.vw_IVSSurveyMainHeader ")
ive <- sqlQuery(TRED, SQL_qry_ive)


Latest_date_Spend <- ifelse(max(ive$Qtr_Num[ive$Year == max(ive$Year)]) < 4, max(ive$Year)-1, max(ive$Year))


Fcst_query <- paste("SELECT * FROM Production.vw_NZTFSurveyMainHeader where Year > ", Latest_date_Spend)
Fcst <- sqlQuery(TRED, Fcst_query)

Fcst_start_Year <- min(Fcst$Year)
Fcst_End_Year <- max(Fcst$Year)
Fcst_year <- max(Fcst$ForecastYear)

Fcst <- Fcst %>%
  filter(ForecastYear == Fcst_year)

Country_List_fcst <- Fcst %>%
  filter(Country != 'All' & substr(Country, 1, 5) != 'Other') %>%
  #   mutate(Country = ifelse(substr(Country, 1, 5) == 'Other', 'Other', as.character(Country))) %>%
  select(Country) %>%
  unique()

ive_2 <- ive %>% 
  filter(Year < Latest_date_Spend + 1 ) %>%
  mutate(CountryGroup = CountryGroup(CORNextYr, shorten = TRUE, type = "IVSweights", OneChina_first = FALSE)) %>% 
  mutate(CountryGroup = ifelse(CountryGroup == "Korea, Republic of", "Korea", 
                               ifelse(CountryGroup == "USA", "US", as.character(CountryGroup)))) %>%
  mutate(CountryGroup = ifelse(CountryGroup %in% Country_List_fcst$Country, as.character(CountryGroup), "Other")) %>%
  group_by(CountryGroup, Year) %>% 
  summarise("TotalVisitorSpend" = sum(Expenditure)/1000000)

Country_List <- unique(ive_2$CountryGroup)

ive_Other <- Fcst %>%
  select(Country, Year, TotalVisitorSpend) %>%
  filter(Country %in% Country_List | Country == 'All') %>%
  mutate(Country = ifelse(Country != 'All', 'Other', as.character(Country))) %>%
  group_by(Country, Year) %>% 
  summarise("TotalVisitorSpend" = sum(TotalVisitorSpend)/1000000) %>%
  spread(Country, TotalVisitorSpend) %>%
  mutate(Country = 'Other',
         TotalVisitorSpend = All - Other) %>%
  select(-All, -Other)

ive_Single <- Fcst %>% 
  select(Country, Year, TotalVisitorSpend) %>%
  filter(Country %in% Country_List & Country != 'All') %>%
  group_by(Country, Year) %>% 
  summarise("TotalVisitorSpend" = sum(TotalVisitorSpend)/1000000)

ive_fcst <- bind_rows(ive_Single, ive_Other) %>%
  arrange(Country, Year) %>%
  rename("CountryGroup" = Country) %>%
  group_by(CountryGroup, Year) %>% 
  summarise("TotalVisitorSpend" = sum(TotalVisitorSpend))

ive_sum <- bind_rows(ive_2, ive_fcst) %>%
  arrange(Year, CountryGroup)

# order countries by total spend highest to lowest in maximum forecast year
  ive_sum$CountryGroup <- factor(ive_sum$CountryGroup,
                                levels = rev(ive_sum$CountryGroup[order(ive_sum$TotalVisitorSpend[ive_sum$Year == Fcst_End_Year])]))


ivs_exp_plot <- ggplot(ive_sum, aes(x = Year, y = TotalVisitorSpend, color = CountryGroup)) +
  theme_minimal() +
  annotate("rect", xmin = Fcst_start_Year, xmax = Fcst_End_Year, ymin = 0, ymax = Inf, fill = "lightblue") +
  geom_line() + 
  theme_light(6, base_family = TheFont) +
  scale_colour_manual("Country", values = tourism.cols("Alternating")) + 
  scale_y_continuous("Spend ($millions)\n", label = dollar) +
  theme(legend.text = element_text(lineheight = 0.3), legend.key.height = unit(0.4, "cm")) +
  theme(axis.title.x = element_blank()) +
  theme(legend.key = element_blank()) +
  ggtitle(paste0("Total spend by country (year ending Dec ", Latest_date_Spend, ")", "\n(blue shaded area is forecast)"))


# ============================ Arrival vs POV with forecats Plot ================================


arr_pov_with_fcst <- sqlQuery(TRED, paste0("select 
                                 VisitorType, 
                                 NoOfVisitors, 
                                 Year, 
                                 Country
                              from vw_NZTFVisitorNumbers v,
                                 vw_NZTFSurveyMainHeader m
                              where v.SurveyResponseID = m.SurveyResponseID
                                 and m.ForecastYear =",Fcst_year)) %>%
  filter(Country == 'All') %>%
  mutate(VisitorType = ifelse(VisitorType == "Holiday", "Holiday/Vacation", 
                              ifelse(VisitorType == "VFR", "Visit Friends/Relatives", as.character(VisitorType)))) %>%
  arrange(Year, VisitorType)


# order countries by total spend highest to lowest in maximum forecast year
arr_pov_with_fcst$VisitorType <- factor(arr_pov_with_fcst$VisitorType,
                              levels = rev(arr_pov_with_fcst$VisitorType[order(arr_pov_with_fcst$NoOfVisitors[arr_pov_with_fcst$Year == Fcst_year - 1])]))


arr_pov_with_fcst_plot <- ggplot(arr_pov_with_fcst, aes(x = Year, y = NoOfVisitors/1000, color = VisitorType)) +
  theme_minimal() +
  annotate("rect", xmin = Fcst_start_Year, xmax = Fcst_End_Year, ymin = 0, ymax = Inf, fill = "lightblue") +
  geom_line() +
  theme_light(6, base_family = TheFont) +
  scale_colour_manual("Purpose\nof visit", values = tourism.cols("Alternating")) + 
  #   scale_x_continuous(breaks=pretty_breaks(n=10)) + 
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) + 
  #scale_size("Visitors\nper year\n('000s)", label = comma) +
  scale_y_continuous("Total arrivals ('000s)\n", label = comma) +
  theme(legend.text = element_text(lineheight = 0.2), legend.key.height = unit(0.2, "cm"), legend.position = "bottom") + 
  theme(legend.key = element_blank()) + 
  theme(axis.title.x = element_blank()) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  ggtitle(paste0("Total arrivals by purpose of visit (year ending Dec ", Latest_date_Spend, ")", "\n(blue shaded area is forecast)"))


#=================================== 4 TSA Plot ==============================================

#---- Manual Option: The following code is to query manually processed TSA data to create plots

EC_exp_plot <- read.csv("data/tourism_expenditure_by_product_v1.csv", header = TRUE)

EC_exp_sum_plot <- EC_exp_plot %>%
  filter(Year == max(Year)) %>%
  filter(Demand_Type == "Domestic demand" | Demand_Type == "International demand") %>%
  mutate(Demand_Type = ifelse(Demand_Type == "Domestic demand", "Domestic", "International")) %>% 
  filter(substr(Product, 1, 3) != "GST" & substr(Product, 1, 5) != "Total") %>%
  group_by(Year, Demand_Type, Product) %>%
  summarise("Expenditure" = sum(Value)) %>%
  data.frame()
  
Year_TSA <- max(EC_exp_sum_plot$Year)

# wrap product levels so plot is legible
EC_exp_sum_plot$Product = str_wrap(EC_exp_sum_plot$Product, width = 20)

# force Product factor level to align with published TSA convention
EC_exp_sum_plot$Product <- factor(EC_exp_sum_plot$Product, levels = c("Other tourism\nproducts",
                                                                      "Education services",
                                                                      "Retail sales -\nother",
                                                                      "Retail sales - fuel\nand other\nautomotive products",
                                                                      "Other passenger\ntransport",
                                                                      "Air passenger\ntransport",
                                                                      "Food and beverage\nserving services",
                                                                      "Accommodation\nservices"))

TSAPlot <- EC_exp_sum_plot %>%
  ggplot(aes(x = Demand_Type, weight = Expenditure, fill = Product), size = 1) +
  geom_bar(width = 0.8, height = 0.2) +
  theme_light(6, base_family = TheFont) +
  scale_y_continuous("Tourism expenditure ($millions)\n(GST exclusive)", label = dollar) +
  scale_fill_manual("", values = tourism.cols("Alternating"), guide = guide_legend(reverse = TRUE)) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(legend.text = element_text(lineheight = 1), legend.key.height = unit(0.6, "cm")) +  
  theme(axis.title.x = element_blank()) +
  guides(col = guide_legend(ncol = 2, byrow = TRUE)) +
  ggtitle(paste0("Tourism expenditure by industry (year ending Mar ", Year_TSA, ")"))




# #---- Automatic Option: The following code is to automatically query TSA data to create plot. When the TSA data is available in TRED, these codes can be used.
# 
# Current_Year <- year(today())- 1
# Previous_Year <- Current_Year - 1
# 
# EC_exp_plot <- ImportTS(TRED, "Tourism Expenditure by Type of Product and Type of Tourist (ANZSIC06) (Annual-Mar)") %>%
#   filter(year(TimePeriod) == year(max(TimePeriod)) | year(TimePeriod) == year(max(TimePeriod)) - 1) %>%
#   mutate(Year = as.character(year(TimePeriod))) %>%
#   select(Year, ClassificationValue.1, ClassificationValue.2, Value) %>%
#   rename("Demand_Type" = ClassificationValue.1) %>% 
#   filter(Demand_Type == "Domestic demand" | Demand_Type == "International demand") %>%
#   mutate(Demand_Type = ifelse(Demand_Type == "Domestic demand", "Domestic", "International")) %>% 
#   rename("Product" = ClassificationValue.2) %>% 
#   filter(substr(Product, 1, 3) != "GST" & substr(Product, 1, 5) != "Total") %>%
#   mutate(Product = ifelse(Product %in% c("Accommodation services", "Food and beverage serving services", "Air passenger transport", 
#                                          "Retail sales - fuel and other automotive products", "Education services"), as.character(Product), 
#                           ifelse(substr(Product, 1, 6) == "Retail", "Retail sales - other", 
#                                  ifelse(Product %in% c("Road, rail and water passenger transport", "Motor vehicle hire or rental",  
#                                                        "Travel agency services"), "Other passenger transport", "Other tourism products")))) %>%
#   filter(Year == max(Year)) %>%
#   group_by(Year, Demand_Type, Product) %>%
#   summarise("Expenditure" = sum(Value)) %>%
#   data.frame()
# 
# 
# Year_TSA <- max(EC_exp_plot$Year)
# 
# # wrap product levels so plot is legible
# EC_exp_sum_plot$Product = str_wrap(EC_exp_sum_plot$Product, width = 20)
# 
# # force Product factor level to align with published TSA convention
# EC_exp_sum_plot$Product <- factor(EC_exp_sum_plot$Product, levels = c("Other tourism\nproducts",
#                                                                       "Education services",
#                                                                       "Retail sales -\nother",
#                                                                       "Retail sales - fuel\nand other\nautomotive products",
#                                                                       "Other passenger\ntransport",
#                                                                       "Air passenger\ntransport",
#                                                                       "Food and beverage\nserving services",
#                                                                       "Accommodation\nservices"))
# 
# TSAPlot <- EC_exp_plot %>%
#   ggplot(aes(x = Demand_Type, weight = Expenditure, fill = Product), size = 1) +
#   geom_bar(width = 0.8, height = 0.2) +
#   theme_light(6, base_family = TheFont) +
#   scale_y_continuous("Tourism expenditure ($millions)\n", label = dollar) +
#   scale_fill_manual("", values = tourism.cols("Alternating"), guide = guide_legend(reverse = TRUE)) +
#   theme(axis.text.x = element_text(color = "black")) +
#   theme(legend.text = element_text(lineheight = 1), legend.key.height = unit(0.6, "cm")) +  
#   theme(axis.title.x = element_blank()) +
#   guides(col = guide_legend(ncol = 2, byrow = TRUE)) +
#   ggtitle(paste0("Tourism expenditure by industry (year ending Mar ", Year_TSA, ")"))
# 


#=================================== 5 Accommodation Guest Nights Plot==============================================


# --------- Guest Nights Summary (Monthly) ---------

ACCOM_monthly <- ImportTS(TRED, "Actual by Accommodation by Type by Variable (Monthly)")

ACCOM_monthly_rpt_end_date <- max(ACCOM_monthly$TimePeriod)


Guest_nights_Monthly <- ACCOM_monthly %>%
  filter(ClassificationValue %in% c('Holiday parks', 'Backpackers', 'Motels', 'Hotels') & 
           ClassificationValue.1 %in% c('Number of guest nights')) %>%
  rename('Accom_Type' = ClassificationValue, 'Guest_Nights' = Value) %>%
  select(TimePeriod, Accom_Type, Guest_Nights)

# End_of_Accom_Rpt <- Guest_nights_Monthly %>%
#   select(TimePeriod) %>%
#   top_n(1) %>%
#   unique()

Month_Accom <- months(ACCOM_monthly_rpt_end_date)
Year_Accom <- year(ACCOM_monthly_rpt_end_date)


Guest_nights_Monthly$Accom_Type <- factor(Guest_nights_Monthly$Accom_Type,
                                     levels = rev(Guest_nights_Monthly$Accom_Type[order(Guest_nights_Monthly$Guest_Nights[Guest_nights_Monthly$TimePeriod == ACCOM_monthly_rpt_end_date])]))


Guest_nights_Monthly_ra <- Guest_nights_Monthly %>%
  group_by(Accom_Type) %>%
  mutate(Guest_Nights = rollapplyr(Guest_Nights, width = 12, FUN = mean, fill = NA)) %>%
  filter(!is.na(Guest_Nights)) %>% 
  data.frame()

accom_plot <- ggplot(Guest_nights_Monthly_ra, aes(x = TimePeriod, y = Guest_Nights/10^3, color = Accom_Type)) +
  theme_minimal() +
  geom_line() +
  theme_light(6, base_family = TheFont) +
  scale_colour_manual("Accommodation type", values = tourism.cols("Alternating")) + 
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) + 
  #scale_size("Visitors\nper year\n('000s)", label = comma) +
  scale_y_continuous("Number of guest nights ('000s)\n", label = comma) +
  theme(legend.text = element_text(lineheight = 0.2), legend.key.height = unit(0.2, "cm"), legend.position = "bottom") + 
  theme(legend.key = element_blank()) +
  theme(axis.title.x = element_blank()) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  ggtitle(paste0("Guest nights by accommodation type (month ending ", substr(Month_Accom, 1, 3), " ", Year_Accom, ")"))

# # --------- Guest Nights Summary yearly plot ---------
# 
# ACCOM_annual<- ImportTS(TRED, "Actual by Accommodation by Type by Variable (Annual-Dec)")
# 
# accom_annual_rpt_end_date <- max(ACCOM_annual$TimePeriod)
# 
# 
# Guest_nights_Yr <- ACCOM_annual %>%
#   filter(ClassificationValue %in% c('Holiday parks', 'Backpackers', 'Motels', 'Hotels') & 
#            ClassificationValue.1 %in% c('Number of guest nights')) %>%
#   rename('Accom_Type' = ClassificationValue, 'Guest_Nights' = Value) %>%
#   select(TimePeriod, Accom_Type, Guest_Nights)
# 
# End_of_Accom_Rpt <- Guest_nights_Yr %>%
#   select(TimePeriod) %>%
#   top_n(1) %>%
#   unique()
# Month_Accom <- months(End_of_Accom_Rpt$TimePeriod)
# Year_Accom <- year(End_of_Accom_Rpt$TimePeriod)
# 
# 
# Guest_nights_Yr$Accom_Type <- factor(Guest_nights_Yr$Accom_Type,
#                                      levels = rev(Guest_nights_Yr$Accom_Type[order(Guest_nights_Yr$Guest_Nights[Guest_nights_Yr$TimePeriod == accom_annual_rpt_end_date])]))
# 
# accom_plot <- ggplot(Guest_nights_Yr, aes(x = TimePeriod, y = Guest_Nights/10^6, color = Accom_Type)) +
#   theme_minimal() +
#   geom_line() +
#   theme_light(6, base_family = TheFont) +
#   scale_colour_manual("Accommodation type", values = tourism.cols("Alternating")) + 
#   guides(fill = guide_legend(nrow = 2, byrow = TRUE)) + 
#   #scale_size("Visitors\nper year\n('000s)", label = comma) +
#   scale_y_continuous("Number of guest nights (millions)\n", label = comma) +
#   theme(legend.text = element_text(lineheight = 0.2), legend.key.height = unit(0.2, "cm"), legend.position = "bottom") + 
#   theme(legend.key = element_blank()) +
#   theme(axis.title.x = element_blank()) +
#   guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
#   ggtitle(paste0("Guest nights by accommodation type (year ending ", substr(Month_Accom, 1, 3), " ", Year_Accom, ")"))
# 



#===================== 6 Place on page===========================

# paste0("$^7$MBIE: New Zealand Tourism Forecasts ", Fcst_year, " to ", End_year, ".")




CairoPDF("figures/KTS_Page2_Plot.pdf", 7, 10, bg = "transparent")
grid.newpage()

vp1 <- viewport(x = 0.45, y = 0.81, width = 0.7, height = 0.6)
print(RTEMap, vp = vp1)
grid.text("Source: MBIE - Regional Tourism Estimates", x = 0.65, y = 0.67, just = "left",
          gp = gpar(fontfamily = TheFont, fontface = "italic", cex = 0.5))

vp2 <- viewport(x = 0.3, y= 0.51, width = 0.55, height = 0.3)
print(ivs_exp_plot, vp = vp2)
grid.text(paste0("Source: MBIE - International Visitor Survey, \nMBIE - New Zealand Tourism Forecasts ", Fcst_year, " to ", Fcst_End_Year), x = 0.2, y = 0.365, just = "left",
          gp = gpar(fontfamily = TheFont, fontface = "italic", cex = 0.5))

vp3 <- viewport(x = 0.75, y = 0.51, width = 0.4, height = 0.3)
print(arr_pov_with_fcst_plot, vp=vp3)
grid.text(paste0("Source: Statistics New Zealand - International Travel and Migration, \nMBIE - New Zealand Tourism Forecasts ", Fcst_year, " to ", Fcst_End_Year), x = 0.6, y = 0.365, just = "left",
          gp = gpar(fontfamily = TheFont, fontface = "italic", cex = 0.5))

vp4 <- viewport(x = 0.3, y = 0.2, width = 0.5, height = 0.3)
print(TSAPlot, vp = vp4)
grid.text("Source: Statistics New Zealand - Tourism Satellite Account", x = 0.2, y = 0.05, just = "left",
          gp = gpar(fontfamily = TheFont, fontface = "italic", cex = 0.5))

vp5 <- viewport(x = 0.75, y = 0.2, width = 0.45, height = 0.3)
print(accom_plot, vp = vp5)
grid.text("Source: Statistics New Zealand - Accommodation Survey", x = 0.6, y = 0.05, just = "left",
          gp = gpar(fontfamily = TheFont, fontface = "italic", cex = 0.5))

dev.off()


