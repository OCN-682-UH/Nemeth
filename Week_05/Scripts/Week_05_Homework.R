## This code will be used for completing the homework for Week 5. 
## 
## 
##
## Created by: Cameron Nemeth
## Created on: 2026-09-25
## Last updated on: 2026-09-25
################################################################################

# Libraries --------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(readxl)
library(writexl)
library(here)
library(geomtextpath)

# Load data --------------------------------------------------------------------
# Use "Data_ in front so that the items sit together in my environment
Data_Conductivity <- read.csv(here("Week_05", "Data", "CondData.csv"))

Data_Depth <- read.csv(here("Week_05", "Data", "DepthData.csv"))

# Functions --------------------------------------------------------------------
# No need for today's assignment. 



# Data analysis ----------------------------------------------------------------

# Convert date columns
Data_Conductivity <-
  Data_Conductivity |>  mutate(date = mdy_hms(date))

Data_Depth <- # This looked like a good format, but would not join, so I repeated.
  Data_Depth |>  mutate(date = ymd_hms(date)) # I think it was character

# Round the conductivity data to the 
# nearest 10 seconds to match depth data
Data_Conductivity <-
  Data_Conductivity |> mutate(date = round_date(date, "10 seconds"))

# Join the two dataframes 
# using inner_join() (only exact matches)
Data_Combined <-
  inner_join(Data_Conductivity, Data_Depth)

# Calculate averages of date, depth, temperature, 
# and salinity by minute
Averages_Minute_CombinedData <- 
  Data_Combined |> mutate(minute = minute(date)) |> 
  group_by(minute) |> # This grouping seems weird to me, but I am following instructions.
  summarize(
    MeanDate = mean(date, na.rm = TRUE),
    MeanDepth = mean(Depth, na.rm = TRUE),
    MeanTemp = mean(Temperature, na.rm = TRUE))
# Dataframe has 60 rows, so looks successful. 

# Make a plot (the fun part!)
# I will look on the R graph gallery again and try to do something fun
# fun that I will be able to use in the future
Plot_Temp_Depth <-
  ggplot(data = Averages_Minute_CombinedData, 
       aes(x = MeanDepth, y = MeanTemp)) +
  geom_point() +
  geom_labelsmooth(aes(label = 'Quadratic model'), fill = "white",
                   method = "lm", formula = y ~ poly(x, 2),
                   size = 5, linewidth = 2, boxlinewidth = 0.6) +
  labs(x = "Mean depth (m)", y = "Mean temperature (°C)") +
  theme_bw() +
  theme(axis.title = element_text(size = 14))

Plot_Temp_Depth

#Save plot
ggsave(here("Week_05","Output", "Homework_Plot_Temp_Depth.png"),
       plot = Plot_Temp_Depth,
       width = 9, height = 5)



# Use pipes throughout (minimize separate dataframes)
# I only have 4 dataframes! :)

# Add comments to your code! 
# Hello! See my comments to understand my thoughts and workflow :)

# Save output, data, and scripts appropriately
# I will do this after I make  my plot, 
# hopefully I do not forget and get a zero on this :)