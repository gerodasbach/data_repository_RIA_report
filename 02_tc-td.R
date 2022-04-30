##############################################################
# Data preparation
##############################################################

# SETUP -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(foreign)
library(fixest)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
# Load data ---------------------------------------------------------------

x = read_dta("C:/Users/gerod/Desktop/EGEI Master/Regional Integration Analysis/5. Research Project/data/Gravity_V202102.dta")


# Clean data----------------------------------------------------------------
Gravity_22 = x %>% filter(year >= 1980)

#rename exporter and importer
Gravity_22 = Gravity_22 %>% rename(exporter = iso3_o, importer = iso3_d)

Gravity_22 = Gravity_22 %>% select(year,iso3_o,iso3_d,tradeflow_comtrade_d,tradeflow_baci,rta,rta_coverage,rta_type)

Gravity_22 =  Gravity_22 %>% mutate(trade = ifelse(is.na(Gravity_22$tradeflow_baci),
                                                        tradeflow_comtrade_d,
                                                        tradeflow_baci))


# Create dummy variables-----------------------------------------------------
##group countries belong to ACFTA (11 countries)

ACFTA = c("IDN","VNM","MYS","PHL","SGP","THA","LAO","MMR","KHM","CHN","BRN")


##############################################################
# Trade Creation, Trade Diversion?
##############################################################

# 1. Trade Creation within the ACFTA block: 

# calculate total export volume of ACFTA exporters to countries in ACFTA:

## Create ACFTA = 1 if "iso3_o" and "iso3_d" belong to ACFTA in year t 
###(ACFTA  entered into force the 1rst of January of 2005)

Gravity_acfta = Gravity_22 %>% mutate(acfta = ifelse
                                      (exporter %in%  ACFTA & importer %in%  ACFTA 
                                        , 1, 0)) %>%
                               select(year,exporter,importer,trade,acfta) 

Gravity_acfta = Gravity_acfta %>% drop_na()

ACFTA_Import_Flows = Gravity_acfta %>%
  subset(acfta == 1) %>%
  group_by(year) %>% 
  summarise(sum_trade = sum(trade)) %>%
  arrange(desc(year)) 

plot1 <- ggplot(ACFTA_Import_Flows, 
       aes(x = year,
           y = sum_trade)) +
  geom_col(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") +
  ylab("Trade Flows") +
  ggtitle("Total trade flows among ACFTA members in current USD (1980-2019)")


# 2. Trade Diversion with countries having rtas other than ACFTA:

#Create OWNFTA (own FTA effect)
## OWNFTA=1 if "iso3_o" and "iso3_d" are members of any of FTAs between 
## either of ACFTA countries with another (third) country at time t
## for example: ASEAN-Japan; ASEAN-India; ASEAN-Australia and New Zealand; China- Pakistan, China- New Zealand, China-Chile, etc.

Gravity_rta = Gravity_22 %>%  mutate(own_FTA = ifelse( (exporter %in%ACFTA & !importer %in%  ACFTA & rta == 1)|
                                                         (!exporter %in%ACFTA & importer %in%  ACFTA & rta == 1),
                                                       1, 0)) %>%  
                              select(year,exporter,importer,trade,own_FTA) 

Gravity_rta= Gravity_rta %>% drop_na()

RTA_Import_Flows = Gravity_rta %>%
  subset(own_FTA == 1) %>%
  group_by(importer) %>% 
  group_by(year) %>%
  summarise(sum_trade = sum(trade)) %>%
  arrange(desc(year)) 


plot2 <- ggplot(RTA_Import_Flows, 
       aes(x = year,
           y = sum_trade)) +
  geom_col(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") +
  ylab("Trade Flows") +
  ggtitle("Total trade flows of countries having an FTA with ACFTA members in current USD (1980-2019)")


# 3. Trade Diversion with countries having no trade agreements with ACFTA members:

# Create CROSSFTA (cross FTA effect)
##CROSS=1 if "iso3_o" and "iso3_d" are members of the remaining FTAs 
## which are not related to ACFTA countries at time t
Gravity_cross = Gravity_22 %>%  mutate(cross_FTA = ifelse( (!exporter %in%ACFTA & !importer%in%  ACFTA & rta ==1),
                                                           1, 0)) %>%
                                select(year,exporter,importer,trade,cross_FTA) 

Gravity_cross = Gravity_cross %>% drop_na()

Cross_Import_Flows = Gravity_cross %>%
  subset(cross_FTA == 1) %>%
  group_by(year) %>% 
  summarise(sum_trade = sum(trade)) %>%
  arrange(desc(year)) 


plot3 <- ggplot(Cross_Import_Flows, 
       aes(x = year,
           y = sum_trade)) +
  geom_col(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") +
  ylab("Trade Flows") +
  ggtitle("Total trade flows of countries having no FTA with ACFTA members in current USD (1980-2019)")

grid.arrange(plot1, plot2, plot3, ncol=1, nrow =3)



# find out which countries are together making up 95% of ACFTA's exports
# quantile(ROW_Import_Flows$sum_exports, probs = seq(0, 1, 1/20)) 

# the 5% quantile is 53628820000. 95% of the countries' import value from ASEAN exceeds this threshold

# Check: 

#ROW_Import_Flows_5 <- 
 # subset(ROW_Import_Flows, sum_exports <= 5.362882e+04) 

#ROW_Import_Flows_95 <-
 # subset(ROW_Import_Flows, sum_exports >= 5.362882e+04) 
  
#ROW_Import_Flows_5 = ROW_Import_Flows_5 %>%
 # summarise(row_import_flow_5 = sum(sum_exports))  
  
#ROW_Import_Flows_95 = ROW_Import_Flows_95 %>%
 # summarise(row_import_flow_95 = sum(sum_exports))






  # 2. limit analysis to countries: top 50 of total external ASEAN trade













