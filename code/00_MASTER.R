################################################################################
# MASTER file for data repository "An Ex-Post Assessment of the ASEAN-China Free Trade
# Agreement (ACFTA) Estimates Based on a Gravity Model"
# Thuy Linh Bui, Gero Dasbach
# May 2022
################################################################################


# SETUP -----------------------------------------------------------------------

# Load libraries
library(here) #easy file referencing, root is set at project root

# Data preparation
source(here("code","01_dataprep.R")) 

# Analysis ----------------------------------------------------------------

# 2. Literature Review --------------------------------------------------------

# 2.1 Evolution of Trade Between China and ASEAN ------------------------------

source(here("code","02_figure_1.R")) # Reproduction code for Figure 1

# We also include barplots of annual growth rates of trade between countries

# 2.2 Ex-Post Assessment of Regional Trade Agreements: The Gravity Model ------

# 2.2.2 Empirical Review: Trade Creation, Trade Diversion or Both? ------------

source(here("code","02_figure_2.R")) # Reproduction code for Figure 2


# 3.Methodology and Data ------------------------------------------------------

# 3.1 Methodology -------------------------------------------------------------

# Data preparation 

source(here("code","03_dataprep.R"))

# Analysis ----------------------------------------------------------------

source(here("code","03_gravity.R")) # Reproduction code for Table 3


