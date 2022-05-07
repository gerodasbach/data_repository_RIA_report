
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

Gravity_22 = readRDS(here("input","Gravity.RDS")) 

# Clean data----------------------------------------------------------------

Gravity_22 = Gravity_22 %>% select(year,exporter,importer,trade,rta)


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

Gravity_acfta = Gravity_22 %>% mutate(ACFTA_intra = ifelse
                                      (exporter %in%  ACFTA & importer %in%  ACFTA 
                                        , 1, 0)) %>%
  select(year,exporter,importer,trade,ACFTA_intra) 

ACFTA_Import_Flows = Gravity_acfta %>%
  subset(ACFTA_intra== 1) %>%
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
  ggtitle("Total Trade Flows among ACFTA Member Countries in Current USD (1995-2019)")


# 2. Trade Diversion with countries having RTAs other than ACFTA:

#Create OWNFTA (own FTA effect)
## OWNFTA=1 if "iso3_o" and "iso3_d" are members of any of FTAs between 
## either of ACFTA countries with another (third) country at time t
## for example: ASEAN-Japan; ASEAN-India; ASEAN-Australia and New Zealand; China- Pakistan, China- New Zealand, China-Chile, etc.

Gravity_export = Gravity_22 %>% 
  mutate(ACFTA_export = ifelse((exporter %in% ACFTA & !importer %in%  ACFTA 
                                ), 1, 0)) %>%
        select(year,exporter,importer,trade,ACFTA_export)

ACFTA_Export_Flows = Gravity_export %>%
  subset(ACFTA_export == 1) %>%
  group_by(importer) %>% 
  group_by(year) %>%
  summarise(sum_trade = sum(trade)) %>%
  arrange(desc(year)) 

plot2 <- ggplot(ACFTA_Export_Flows, 
                aes(x = year,
                    y = sum_trade)) +
  geom_col(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") +
  ylab("Export Flows") +
  ggtitle("Total Export Flows from ACFTA Member Countries to Third Countries in Current USD (1995-2019)")


# 3. Trade Diversion with countries having no trade agreements with ACFTA members:

# Create CROSSFTA (cross FTA effect)
##CROSS=1 if "iso3_o" and "iso3_d" are members of the remaining FTAs 
## which are not related to ACFTA countries at time t
Gravity_import = Gravity_22 %>%  mutate(ACFTA_import = ifelse( (!exporter %in% ACFTA 
                                                                         & importer %in%  ACFTA),
                                                                        1, 0)) %>%
                                  select(year,exporter,importer,trade,ACFTA_import) 

ACFTA_Import_Flows = Gravity_import %>%
  subset(ACFTA_import == 1) %>%
  group_by(year) %>% 
  summarise(sum_trade = sum(trade)) %>%
  arrange(desc(year)) 

plot3 <- ggplot(ACFTA_Import_Flows, 
                aes(x = year,
                    y = sum_trade)) +
  geom_col(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") +
  ylab("Import Flows") +
  ggtitle("Total Imports of ACFTA Member Countries from Third Countries in Current USD (1995-2019)")

figure_2 <- grid.arrange(plot1, plot2, plot3, ncol=1, nrow =3)

figure_2 <- figure_2 %>%
  
  ggsave(file=here("output","figures","figure_2.png"))










