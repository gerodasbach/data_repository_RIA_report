##############################################################
# Data preparation
##############################################################

# Load packages
library(tidyverse)
library(foreign)
library(fixest)
library(haven)
library(here)
library(flextable) # Output tables to word
library(huxtable) # Output tables
library(here)

# Load data ----------------------------------------------------------------

Gravity_data = readRDS(here("input","Gravity.RDS")) 

# Create dummy variables-----------------------------------------------------
##group countries belong to ACFTA (11 countries)

ACFTA = c("IDN","VNM","MYS","PHL","SGP","THA","LAO","MMR","KHM","CHN","BRN")

## 1. Create a dummy variable to measure intra-bloc trade
###   (Create ACFTA = 1 if "iso3_o" and "iso3_d" belong to ACFTA in year t)
###   Although the China-ASEAN trade agreement on goods entered into force 
###   the 1rst of January of 2005, the EHP started in 2004 and we assume that 
###   there could have been anticipation effects (before entry into force)

Gravity_data = Gravity_data %>% mutate(ACFTA_intra = ifelse
                                       (exporter %in%  ACFTA & importer %in%  ACFTA 
                                         & year >= 2003
                                         , 1, 0))

## 2. create a control variable for the rest of RTAs in the world
###   (means excluding ACFTA)
###   other_RTA = 1  if ACFTA_intra=0 & rta=1

Gravity_data = Gravity_data %>% mutate(other_RTA = ifelse( (ACFTA_intra==0 & rta ==1),
                                                           1, 0))

## 3. Create dummy variables to measure Extra-bloc trade
###   ACFTA_export = 1 if exporter i belongs to the ACFTA in year t and 
###                       importer j does not and zero otherwise
###   ACFTA_import = 1 if exporter i does not belongs to the ACFTA in year t and 
###                       importer j does and zero otherwise

Gravity_data = Gravity_data %>% 
  mutate(ACFTA_export = ifelse((exporter %in% ACFTA & !importer %in%  ACFTA 
                                & year>=2003),
                               1, 0),
         ACFTA_import = ifelse( (!exporter %in% ACFTA 
                                 & importer %in%  ACFTA & year>=2003),
                                1, 0))

saveRDS(Gravity_data,file=here("output","acfta_applications.RDS"))

