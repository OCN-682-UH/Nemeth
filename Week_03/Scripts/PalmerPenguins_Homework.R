## This code will be used for completing the homework for Week 3. 
## In today's homework, we are to spend no more than 1 hour creating a plot
## using the Penguin data. 
##
## Created by: Cameron Nemeth
## Created on: 2026-09-08
## Last updated on: 2026-09-08
################################################################################

# Libraries --------------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(writexl)
library(here)
library(palmerpenguins)



# Load data --------------------------------------------------------------------
# Never loaded in the data on the lecture, but we did use the glimpse function
# to understand the data.

#Glimpse at penguin data
glimpse(penguins)


# Functions --------------------------------------------------------------------
# No need for today's assignment. 

# Data analysis ----------------------------------------------------------------

# Create my plot for our homework. 
# I want to work on a plot that I will be able to use as a template for 
# future analyses... Something I commonly would encounter. 

# I really like the combination of violin, box plot, and jitter, as it really
# shows all aspects of the data, so I will try to work on that and create
# something clean and reproducible. 

ggplot(data = penguins, 
       mapping = aes(x = species, 
                     y = body_mass_g,
                     color = species,
                     fill = species)) +
  geom_violin(alpha = 0.5, 
              width = 1.00,
              color = NA) + # NA for color so no border on violin plot
  geom_jitter(width = 0.25,
              alpha = 0.75) +
  geom_boxplot(color = "black",
               width = 0.50,
               fill = NA,
               outliers = FALSE) +
  stat_summary( # this section adds the white mean bar inside of the boxplot 
    fun = mean, 
    geom = "crossbar",
    width = 0.50,
    fatten = 2,
    color = "white",
    show.legend = FALSE
  ) +
  labs(x = "Species", # x-axis title
       y = "Body mass (g)") + #y-axis title
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 15), # x-axis title size
        axis.title.y = element_text(size = 15), # y-axis title size
        axis.text.x = element_text(size = 10), # x-axis text size
        axis.text.y = element_text(size = 10), #y-axis text size
        panel.grid.major.y = element_line(), # Major y axis gridlines
        panel.grid.minor.y = element_line() # Minor y axis gridlines
        )+
  scale_y_continuous(
    breaks = scales::breaks_width(1000), # Y axis scale
    minor_breaks = scales::breaks_width(500) #Gridlines every 500
  )

#Save plot
ggsave(here("Week_03/Output/PenguinHomework_MyPlot.png"),
       width = 7, height = 5) # reminder that this is in inches
