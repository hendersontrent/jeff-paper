#-----------------------------------------
# This script aims to produce the packages
# and reusables needed to run the project
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 24 April 2020
#-----------------------------------------

# Load packages

library(tidyverse)
library(janitor)
library(forecast)
library(ggpubr)
library(tseries)

# Turn off scientific notation

options(scipen = 999)
