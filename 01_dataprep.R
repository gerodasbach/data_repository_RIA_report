
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
library(here)

# PROBLEM

# Github upload cannot exceed 25 MB: please request the full data set from us

# Load data --------------------------------------------------------------------

x = read_dta(here("input","Gravity_V202102.dta")) 

# we transfer the full "Gravity_V202102.dta" data set upon request: 
# please send an email to gero.dasbach@gmail.com

# once you have obtained the data set, you can put it into the ./input folder 
# and run this code:

# Wrangle data -----------------------------------------------------------------

Gravity = x %>% select(year,iso3_o,iso3_d,tradeflow_comtrade_d,
                             tradeflow_baci,contig,dist,comlang_off,
                             pop_o,pop_d,gdp_o,gdp_d,rta)

Gravity = Gravity %>% filter(year >= 1995)

Gravity =  Gravity %>% mutate(trade = ifelse(is.na(Gravity$tradeflow_baci),
                                             tradeflow_comtrade_d,
                                             tradeflow_baci))

Gravity = Gravity %>% rename(exporter = iso3_o, importer = iso3_d)

Gravity = Gravity %>% select(year,exporter,importer,trade,contig,dist,comlang_off,
                             pop_o,pop_d,gdp_o,gdp_d,rta)

Gravity <- Gravity %>% drop_na(year,trade,exporter,importer,trade,contig,dist,comlang_off,
                               pop_o,pop_d,gdp_o,gdp_d,rta)  

saveRDS(Gravity,file=here("input","Gravity.RDS"))
