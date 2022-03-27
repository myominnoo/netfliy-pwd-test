# -------------------------------------------------------------------------
# 
#                       SET UP - MALCON REPORT
#
# -------------------------------------------------------------------------


# clear workspace ---------------------------------------------------------

rm(list = ls())

source("scripts/functions.R")

# set up Environment  -----------------------------------------------------

library(tidyverse)
library(magrittr)

source("scripts/setup.R", echo = TRUE)

# Process HFC -------------------------------------------------------------

source("scripts/hfc.R", echo = TRUE)

# Process PI --------------------------------------------------------------

source("scripts/pi.R", echo = TRUE)

# Process PPA -------------------------------------------------------------

source("scripts/ppa.R", echo = TRUE)

# Process EI --------------------------------------------------------------

source("scripts/ei.R", echo = TRUE)

