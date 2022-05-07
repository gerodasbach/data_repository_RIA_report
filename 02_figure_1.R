
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
library(here)

# Load data ----------------------------------------------------------------

Gravity_21 = readRDS(here("input","Gravity.RDS")) 

# Create dummy variables-----------------------------------------------------
##group countries belonging to ACFTA (11 countries)

ACFTA = c("IDN","VNM","MYS","PHL","SGP","THA","LAO","MMR","KHM","CHN","BRN")

##group countries belonging to the ASEAN6 and China (7 countries)
ACFTA6 = c("IDN","MYS","PHL","SGP","THA","CHN","BRN")

##group countries belonging to the ASEAN4 and China (5 countries)
ACFTA4 = c("VNM","LAO","MMR","KHM","CHN")

## Create ACFTA = 1 if "iso3_o" and "iso3_d" belong to ACFTA in year t 
###(ACFTA  entered into force on the 1st of January of 2005)

Gravity_21 = Gravity_21 %>% mutate(acfta = ifelse
                                       (exporter %in%  ACFTA & importer %in%  ACFTA 
                                         , 1, 0))

Gravity_21 = Gravity_21 %>% mutate(acfta6 = ifelse
                                           (exporter %in%  ACFTA6 & importer %in%  ACFTA6 
                                             , 1, 0))

Gravity_21 = Gravity_21 %>% mutate(acfta4 = ifelse
                                           (exporter %in%  ACFTA4 & importer %in%  ACFTA4 
                                             , 1, 0))


# 1. Trade flows between ASEAN and CHN 
## imports from ASEAN to CHN
Gravity_acfta_d = Gravity_21 %>% filter(acfta == 1, importer == "CHN")

Gravity_acfta_d = Gravity_acfta_d %>% select(year,exporter, importer, trade)

Gravity_acfta_d <-subset(Gravity_acfta_d, exporter!="CHN")


## aggregate imports from ASEAN to CHN
CHN_ASEAN_imports <- Gravity_acfta_d %>% group_by(year) %>% summarise(sum_imports = sum(trade)) 

## annual growth rate of imports from ASEAN to CHN 
CHN_ASEAN_imports_g = CHN_ASEAN_imports %>%
  # first sort by year
  arrange(year) %>%
  mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
         Diff_growth = sum_imports - lag(sum_imports), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/sum_imports * 100) # growth rate in percent

plot1 <-
ggplot(CHN_ASEAN_imports, aes(x=year, y=sum_imports, yScale="log2")) +
  geom_point() +
  ggtitle("China's Imports from ASEAN, (1995-2018)") +
  xlab("Year") +
  ylab("Volume of Imports in Current USD")
plot1

barplot1 <-
ggplot(data=CHN_ASEAN_imports_g, aes(x=year, y=Rate_percent)) +
  geom_bar(stat = "identity") +
  ggtitle("Year-to-Year Growth Rates of China's Imports from ASEAN, (1995-2018)") +
  xlab("Year") +
  ylab("Growth Rate of Imports vis-a-vis Previous Year (%)")
barplot1 

## exports from CHN to ASEAN
Gravity_acfta_o = Gravity_21 %>% filter(acfta == 1, exporter == "CHN")

Gravity_acfta_o = Gravity_acfta_o %>% select(year,exporter,importer,trade)

Gravity_acfta_o <-subset(Gravity_acfta_o, importer!="CHN")

## aggregate exports from CHN to ASEAN
CHN_ASEAN_exports <- Gravity_acfta_o %>% group_by(year) %>% summarise(sum_exports = sum(trade)) 

## annual growth rate of exports from ASEAN to CHN 
CHN_ASEAN_exports_g = CHN_ASEAN_exports %>%
  # first sort by year
  arrange(year) %>%
  mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
         Diff_growth = sum_exports - lag(sum_exports), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/sum_exports * 100) # growth rate in percent

plot2 <-
  ggplot(CHN_ASEAN_exports, aes(x=year, y=sum_exports, yScale="log2")) +
  geom_point() +
  ggtitle("China's Exports to ASEAN, (1995-2019)") +
  xlab("Year") +
  ylab("Volume of Exports in Current USD")
plot2


barplot2 <-
  ggplot(data=CHN_ASEAN_exports_g, aes(x=year, y=Rate_percent)) +
  geom_bar(stat = "identity") +
  ggtitle("Year-to-Year Growth Rates of China's Exports to ASEAN, (1995-2019)") +
  xlab("Year") +
  ylab("Growth Rate of Exports vis-a-vis Previous Year (%)")
barplot2 


# 2. Trade flows between ASEAN6 and CHN 
## imports from ASEAN6 to CHN
Gravity_acfta6_d = Gravity_21 %>% filter(acfta6 == 1, importer == "CHN")

Gravity_acfta6_d = Gravity_acfta6_d %>% select(year,exporter,importer,trade)

Gravity_acfta6_d <-subset(Gravity_acfta6_d, exporter!="CHN")


## aggregate imports from ASEAN6 to CHN
CHN_ASEAN6_imports <- Gravity_acfta6_d %>% group_by(year) %>% summarise(sum_imports = sum(trade)) 
## annual growth rate of imports from ASEAN6 to CHN 
CHN_ASEAN6_imports_g = CHN_ASEAN6_imports %>%
  # first sort by year
  arrange(year) %>%
  mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
         Diff_growth = sum_imports - lag(sum_imports), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/sum_imports * 100) # growth rate in percent

plot3 <-
  ggplot(CHN_ASEAN6_imports, aes(x=year, y=sum_imports)) +
  geom_point() +
  ggtitle("China's Imports from ASEAN 6, (1995-2018)") +
  xlab("Year") +
  ylab("Volume of Imports in Current USD")
plot3

barplot3 <-
  ggplot(data=CHN_ASEAN6_imports_g, aes(x=year, y=Rate_percent)) +
  geom_bar(stat = "identity") +
  ggtitle("Year-to-Year Growth Rates of China's Imports from ASEAN6, (1995-2019)") +
  xlab("Year") +
  ylab("Growth Rate of Imports vis-a-vis Previous Year (%)")
barplot3

## exports from CHN to ASEAN6
Gravity_acfta6_o = Gravity_21 %>% filter(acfta6 == 1, exporter == "CHN")

Gravity_acfta6_o = Gravity_acfta6_o %>% select(year,exporter,importer,trade)

Gravity_acfta6_o <-subset(Gravity_acfta6_o, importer!="CHN")

## aggregate exports from CHN to ASEAN6
CHN_ASEAN6_exports <- Gravity_acfta6_o %>% group_by(year) %>% summarise(sum_exports = sum(trade)) 
## annual growth rate of exports from ASEAN6 to CHN 
CHN_ASEAN6_exports_g = CHN_ASEAN6_exports %>%
  # first sort by year
  arrange(year) %>%
  mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
         Diff_growth = sum_exports - lag(sum_exports), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/sum_exports * 100) # growth rate in percent

plot4 <-
  ggplot(CHN_ASEAN6_exports, aes(x=year, y=sum_exports, yScale="log2")) +
  geom_point() +
  ggtitle("China's Exports to ASEAN 6, (1995-2019)") +
  xlab("Year") +
  ylab("Volume of Exports in Current USD")
plot4

barplot4 <-
  ggplot(data=CHN_ASEAN6_exports_g, aes(x=year, y=Rate_percent)) +
  geom_bar(stat = "identity") +
  ggtitle("Year-to-Year Growth Rates of China's Exports to ASEAN6, (1995-2019)") +
  xlab("Year") +
  ylab("Growth Rate of Exports vis-a-vis Previous Year (%)")
barplot4 


# 3. Trade flows between ASEAN4 and CHN 
## imports from ASEAN4 to CHN
Gravity_acfta4_d = Gravity_21 %>% filter(acfta4 == 1, importer == "CHN")

Gravity_acfta4_d = Gravity_acfta4_d %>% select(year,exporter,importer,trade)

Gravity_acfta4_d <-subset(Gravity_acfta4_d, exporter!="CHN")

## aggregate imports from ASEAN4 to CHN
CHN_ASEAN4_imports <- Gravity_acfta4_d %>% group_by(year) %>% summarise(sum_imports = sum(trade)) 
## annual growth rate of imports from ASEAN4 to CHN 
CHN_ASEAN4_imports_g = CHN_ASEAN4_imports %>%
  # first sort by year
  arrange(year) %>%
  mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
         Diff_growth = sum_imports - lag(sum_imports), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/sum_imports * 100) # growth rate in percent

plot5 <-
  ggplot(CHN_ASEAN4_imports, aes(x=year, y=sum_imports)) +
  geom_point() +
  ggtitle("China's Imports from ASEAN 4, (1995-2018)") +
  xlab("Year") +
  ylab("Volume of Imports in Current USD")
plot5

barplot5 <-
  ggplot(data=CHN_ASEAN4_imports_g, aes(x=year, y=Rate_percent)) +
  geom_bar(stat = "identity") +
  ggtitle("Year-to-Year Growth Rates of China's Imports from ASEAN4, (1995-2019)") +
  xlab("Year") +
  ylab("Growth Rate of Imports vis-a-vis Previous Year (%)")
barplot5


## exports from CHN to ASEAN4
Gravity_acfta4_o = Gravity_21 %>% filter(acfta4 == 1, exporter == "CHN")

Gravity_acfta4_o = Gravity_acfta4_o %>% select(year,exporter,importer,trade)

Gravity_acfta4_o <-subset(Gravity_acfta4_o, importer!="CHN")

## aggregate exports from CHN to ASEAN4
CHN_ASEAN4_exports <- Gravity_acfta4_o %>% group_by(year) %>% summarise(sum_exports = sum(trade)) 
## annual growth rate of exports from ASEAN4 to CHN 
CHN_ASEAN4_exports_g = CHN_ASEAN4_exports %>%
  # first sort by year
  arrange(year) %>%
  mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
         Diff_growth = sum_exports - lag(sum_exports), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/sum_exports * 100) # growth rate in percent

plot6 <-
  ggplot(CHN_ASEAN4_exports, aes(x=year, y=sum_exports)) +
  geom_point() +
  ggtitle("China's Exports to ASEAN 4 (1995-2019)") +
  xlab("Year") +
  ylab("Volume of Exports in Current USD")
plot6

barplot6 <-
  ggplot(data=CHN_ASEAN4_exports_g, aes(x=year, y=Rate_percent)) +
  geom_bar(stat = "identity") +
  ggtitle("Year-to-Year Growth Rates of China's Exports to ASEAN4, (1995-2019)") +
  xlab("Year") +
  ylab("Growth Rate of Exports vis-a-vis Previous Year (%)")
barplot6


# 4. Trade flows between Non-ASEAN and CHN 
## imports from Non-ASEAN to CHN
Gravity_acfta_d = Gravity_21 %>% filter(acfta == 0, importer == "CHN")

Gravity_acfta_d = Gravity_acfta_d %>% select(year,exporter, importer,trade)

Gravity_acfta_d <-subset(Gravity_acfta_d, exporter!="CHN")

## aggregate imports from ASEAN to CHN
CHN_NonASEAN_imports <- Gravity_acfta_d %>% group_by(year) %>% summarise(sum_imports = sum(trade)) 

## annual growth rate of imports from Non-ASEAN to CHN 
CHN_NonASEAN_imports_g = CHN_NonASEAN_imports %>%
  # first sort by year
  arrange(year) %>%
  mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
         Diff_growth = sum_imports - lag(sum_imports), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/sum_imports * 100) # growth rate in percent

plot7 <-
  ggplot(CHN_NonASEAN_imports, aes(x=year, y=sum_imports)) +
  geom_point() +
  ggtitle("China's Imports from Non-ASEAN, (1995-2019)") +
  xlab("Year") +
  ylab("Volume of Imports in Current USD")
plot7

barplot7 <-
  ggplot(data=CHN_NonASEAN_imports_g, aes(x=year, y=Rate_percent)) +
  geom_bar(stat = "identity") +
  ggtitle("Year-to-Year Growth Rates of China's Imports from Non-ASEAN, (1995-2019)") +
  xlab("Year") +
  ylab("Growth Rate of Imports vis-a-vis Previous Year (%)")
barplot7




## exports from CHN to ASEAN
Gravity_acfta_o = Gravity_21 %>% filter(acfta == 0, exporter == "CHN")

Gravity_acfta_o = Gravity_acfta_o %>% select(year,exporter,importer,trade)

Gravity_acfta_o <-subset(Gravity_acfta_o, importer!="CHN")

## aggregate exports from CHN to ASEAN
CHN_NonASEAN_exports <- Gravity_acfta_o %>% group_by(year) %>% summarise(sum_exports = sum(trade)) 

## annual growth rate of exports from Non-ASEAN to CHN 
CHN_NonASEAN_exports_g = CHN_NonASEAN_exports %>%
  # first sort by year
  arrange(year) %>%
  mutate(Diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
         Diff_growth = sum_exports - lag(sum_exports), # Difference in route between years
         Rate_percent = (Diff_growth / Diff_year)/sum_exports * 100) # growth rate in percent

plot8 <-
  ggplot(CHN_NonASEAN_exports, aes(x=year, y=sum_exports)) +
  geom_point() +
  ggtitle("China's Exports to Non-ASEAN, (1995-2019)") +
  xlab("Year") +
  ylab("Volume of Exports in Current USD")
plot8

barplot8 <-
  ggplot(data=CHN_NonASEAN_exports_g, aes(x=year, y=Rate_percent)) +
  geom_bar(stat = "identity") +
  ggtitle("Year-to-Year Growth Rates of China's Exports to Non-ASEAN, (1995-2019)") +
  xlab("Year") +
  ylab("Growth Rate of Exports vis-a-vis Previous Year (%)")
barplot8



library(gridExtra)

figure_1 <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol=2, nrow =4)

figure_1 <- figure_1 %>%

ggsave(file=here("output","figures","figure_1.png"))

# grid.arrange(barplot1, barplot2, barplot3, barplot4, barplot5, barplot6, barplot7, barplot8, ncol=2, nrow =4)



