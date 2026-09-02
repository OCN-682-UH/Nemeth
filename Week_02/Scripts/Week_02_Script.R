# This is my first script for MBIO 612
# We are learning how to import data
# Created by Cameron Nemeth
# Created on 2026-09-01
######################################################

# Load libraries
library(tidyverse)
library(here)

# Read in data
WeightData <- read_csv(here("Week_02", "Data", "weightdata.csv"))

# Data analysis
head(WeightData)
tail(WeightData)
view(WeightData)

