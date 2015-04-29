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


base_size <- 9
TheFont <- "Calibri"

#=============================RTE map=====================
# this next line is a cheat, please change this so it gets the RTEs properly from TRED.  However, after
# joining with all the classifications etc, it should look the same as the object called into existence
# by data(RTEs)
data(RTEs)

LastYear <- max(RTEs$YEMar)

# load map data
data(rto_gg)


RTE_sum <- RTEs %>%
  group_by(RTO, Type, YEMar)  %>%
  summarise(Spend = sum(Spend)) %>%
  group_by(RTO, Type) %>%
  summarise(
    LatestSpend = Spend[YEMar == LastYear],
    cagr        = CAGR(ratio = Spend[YEMar == LastYear] / Spend[YEMar == 2009], period = LastYear - 2009 + 1)
    ) %>%
  data.frame()

combined <- merge(rto_gg, RTE_sum, all.x=TRUE, by="RTO") 
spend_only <- unique(combined [ , c("RTO", "long.centre", "lat.centre", "LatestSpend", "Type")])

RTEMap <- ggplot() +
  geom_polygon(data=combined, aes(x=long, y=lat, group=group, fill=cagr / 100), colour="grey75", size=0.2) + # size refers to linewidth
  geom_point(data=spend_only, aes(x=long.centre, y=lat.centre, size=LatestSpend), shape=1) +
  facet_wrap(~Type) +
  theme_nothing(base_size, base_family=TheFont) +
  scale_size(paste0("Spend in ", LastYear, ", $m"), label=comma, range = c(1,10)) +
  scale_fill_gradientn(paste0("Average growth 2009 to ", LastYear) %>% wrap(18), 
                       colours=brewer.pal(9, "Spectral"),
                       label = percent) +
  coord_map() +
  ggtitle("Regional distribution")



#=======================TSA plot==========================
TRED <- odbcConnect("TRED_Prod")

# hmm, something wrong with the TSA data in infoshare.  To illustrate the graph I'll type the data in

TSATable8 <- data.frame(
  Product = c("Accommodation", "Food and beverage serving", "Air transport", "Other transport", "Retail - fuel and automotive", 
              "Retail - other", "Education", "Other", "GST") %>% rep(2),
  Year   =  rep(c(2011, 2014), each=9) %>% as.character(),
  Value =   c(2000,2624,3934,2160,2527,4741,423,1870,1610,2173,2839,4222,2306,2887,5069,481,2019,1756)
) %>%
  mutate(Product = factor(Product, levels = Product[1:9])) # for ordering in plot

TSAPlot <- TSATable8 %>%
  ggplot(aes(x=Year, weight=Value, fill=Product)) +
  theme_light(base_size, base_family=TheFont) +
  geom_bar() +
  scale_y_continuous("Tourism expenditure , $m\n", label=comma) +
  scale_fill_manual("", values=tourism.cols("Alternating"), guide = guide_legend(reverse=TRUE)) +
  ggtitle("Expenditure by product") +
  labs(x="Year ending March")

  

#=====================Place on page===========================

# We'll use one device for the whole page, so easier to place just one thing in LaTeX.  Wdith and height  in inches.
CairoPDF("figures/PageTwoPlot.pdf", 7, 10)
grid.newpage()

# for viewports, the x, y, width and height are proportional to the whole page.  And (0,0) is bottom left.
  vp1 <- viewport(x=0.5, y=0.8, width=1, height=0.4)
  print(RTEMap, vp=vp1)
  grid.text("Source: Regional Tourism Estimates", x=0.7, y=0.6, just="left",
            gp =gpar(fontfamily=TheFont, fontface="italic", cex=0.5))
  


  vp2 <- viewport(x=0.2, y=1/6, width=1, height=0.2)
  print(TSAPlot, vp=vp2)
  grid.text("Source: Tourism Satellite Account", x=0.7, y=0.03, just="left",
          gp =gpar(fontfamily=TheFont, fontface="italic", cex=0.5))

dev.off()

