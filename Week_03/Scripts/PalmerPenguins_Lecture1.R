## This code will be used for completing the in-class assignment for 2026-09-08
##
## Created by: Cameron Nemeth
## Created on: 2026-09-08
## Last updated on: 2026-09-08
################################################################################

# Libraries --------------------------------------------------------------------
# The usual libraries I always add
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(writexl)
library(here)

#Specific libraries for today's class
library(palmerpenguins)

#Glimpse at penguin data
glimpse(penguins)

# Load data --------------------------------------------------------------------
# Never loaded in the data on the lecture
#data <- read_csv(here("Data", "data.csv"))


# Functions --------------------------------------------------------------------
# No need for today's assignment. 

# Data analysis ----------------------------------------------------------------

# Visualize penguin dataframe
# Just playing around with different options Dr. Silbiger showed us
ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
       y = bill_length_mm,
       color = species,
       shape = island)) +
  geom_point(size = 2, alpha = 0.5) +
  labs(title = "Bill depth and length",
       subtitle = "Dimensions for Adeleie, Chinstrap, and Gentoo penguins",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       shape = "Island",
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d()
