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
library(colorBlindness)
library(praise) # New package from today's class to regularly use :)


#Specific libraries for today's class
library(palmerpenguins)
library(beyonce)
par(mfrow=c(26,5))
for(i in 1:130) print(beyonce_palette(i))

# Load data --------------------------------------------------------------------
#data <- read_csv(here("Data", "data.csv"))


# Functions --------------------------------------------------------------------
# No need for today's assignment. 

# Data analysis ----------------------------------------------------------------

# Visualize penguin dataframe
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species)) + 
  geom_point()+ 
  geom_smooth(method = "lm")+ 
  labs(x = "Bill depth (mm)", 
       y = "Bill length (mm)"
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 15,
                                  color = "black"))


ggsave(here("Week_03/Output/Penguin.png"),
       width = 7, height = 5) # reminder that this is in inches

#Coord trans example
ggplot(diamonds, aes(carat, price)) +
  geom_point() +
  coord_trans(x = "log10", y = "log10")