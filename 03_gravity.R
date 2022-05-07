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

# Load data ---------------------------------------------------------------

Gravity_data = readRDS(here("output","acfta_applications.RDS"))

##############################################################
# Gravity models
##############################################################

#Specification 1: Naive Gravity-------------------------------------------------------------
## Select necessary data
x_app1 = Gravity_data %>%
  filter(year %in% seq(1995, 2019, 4))

## Construct symmetric pair id's
x_app1 = x_app1 %>%
  mutate(pair = paste(pmin(exporter,importer),pmax(exporter,importer),sep = "_")) %>%
  group_by(pair) %>%
  mutate(pair_id = cur_group_id())

## calculate logs
x_app1 = x_app1 %>%
  mutate(across(c(trade, gdp_d , gdp_o, dist, pop_d, pop_o )
                ,~log(.x),.names="ln_{.col}"))

## Estimation
fit_ols = feols(ln_trade~ ln_gdp_o + ln_gdp_d + ln_pop_o + ln_pop_d +
                  ln_dist + comlang_off + contig + ACFTA_intra + other_RTA + 
                  ACFTA_export + ACFTA_import,
                data = x_app1 %>%
                  filter(trade > 0 & exporter != importer), 
                vcov = cluster ~ pair_id)
summary(fit_ols)
#--------------------------------------------------------------------------------
#Specification 2: OLS estimation controlling for multilateral resistance terms 
#                  with fixed effects.
#-------------------------------------------------------------------------------
## Create fixed effects 
x_app2 = x_app1 %>%
  unite("fe_exp_year",c(exporter,year),sep="_",remove=FALSE) %>%
  unite("fe_imp_year",c(importer,year),sep="_",remove=FALSE)

## Estimate

fit_fixedeffects  = feols(ln_trade ~ ln_dist + comlang_off + contig 
                          + ACFTA_intra + other_RTA 
                          |fe_exp_year+fe_imp_year, 
                          data = x_app2 %>%
                            filter(trade>0 & exporter != importer), 
                          vcov = cluster ~ pair_id)
summary(fit_fixedeffects)
#--------------------------------------------------------------------------------
#Specification 3: PPML estimation controlling for multilateral resistance terms
#                 with fixed effects
#-------------------------------------------------------------------------------
rta_poisson = fepois(trade ~ ln_dist + comlang_off + contig + 
                             ACFTA_intra + other_RTA| 
                             fe_exp_year + fe_imp_year,
                             data = x_app2 %>%
                             filter(exporter != importer), 
                             vcov = cluster ~ pair_id)
summary(rta_poisson)

#--------------------------------------------------------------------------------
#Specification 4: Addressing potential endogeneity of FTAs
#-------------------------------------------------------------------------------

rta_poisson_2 = fepois(trade ~ ACFTA_intra + other_RTA 
                               | fe_exp_year + fe_imp_year + pair_id ,
                               data = x_app2 %>%
                              filter(exporter != importer), 
                              vcov = cluster ~ pair_id)
summary(rta_poisson_2)

##############################################################
# Overview
##############################################################
## Create table
tab_gravity =  huxreg(" " = fit_ols,
                           "Fixed Effects" = fit_fixedeffects,
                           "Fixed Effects" = rta_poisson,
                           "Fixed Effects" = rta_poisson_2,
                           coefs = c("log exporter's GDP"="ln_gdp_o",
                                     "log importer's GDP"="ln_gdp_d",
                                     "log exporter's population"="ln_pop_o",
                                     "log importer's population"="ln_pop_d",
                                     "Log distance" = "ln_dist",
                                     "Common language" = "comlang_off",
                                     "Contiguity" = "contig",
                                     "ACFTA intra" = "ACFTA_intra",
                                     "other RTAs" = "other_RTA",
                                     "ACFTA export" = "ACFTA_export",
                                     "ACFTA import" = "ACFTA_import"),
                           
                           stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
                           statistics = c(n = "nobs", R2 = "r.squared"),
                           note = "Notes: All estimates are obtained with data for the years 1995, 1999, 2003, 2007, 2011, 2015, and 2019. We use international trade flows only. Column (1) applies the naive OLS estimator. Column (2) uses an OLS estimator with fixed effects. Column (3) uses the PPML estimator with fixed effects. Column (4) uses the PPML estimator and adds pair fixed effects. Standard errors are clustered by country pair and are reported in parentheses. ***, **, and * indicate statistical significance at 99.9%, 99%, and 95% confidence levels, respectively") %>% 
  insert_row("","(1) OLS", "(2) OLS", "(3) PPML", " (4) ENDG", after = 0) %>%
  set_top_border(1,everywhere,1) %>%
  set_align(1, everywhere, "center") %>%
  insert_row(c("exporter-time- & importer-time fixed effects", "No", "Yes", "Yes", "Yes"),after = 26) %>% 
  insert_row(c("pair fixed effects", "No", "No", "No", "Yes"),after = 27) %>% 
  #set_number_format(26, everywhere,3)%>%
  set_tb_borders(26:27,everywhere,0) %>%
  set_tb_padding(0) %>% 
  set_col_width(c(0.3,rep(0.7/4,4))) %>%
  set_align(everywhere,-1,"center") %>%
  set_caption("Estimating the Effects of the ASEAN-China Free Trade Agreement") %>%
  set_label("tab_3") 

width(tab_gravity) = 1 #Set relative table width for use in documents

## Export table to latex
cat(to_latex(tab_gravity),file=here("output","tables","tab_gravity.tex"))
## Export table to word
tab_gravity_docx = as_flextable(tab_gravity)
save_as_docx(tab_gravity_docx, path = here("output","tables","tab_gravity.docx"))




