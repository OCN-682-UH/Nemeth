## This code will be used for completing the homework for Week 4. 
## This script will include both HW 1 and 2.
## 
##
## Created by: Cameron Nemeth
## Created on: 2026-09-16
## Last updated on: 2026-09-16
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
library(ggExtra) # For my new plot I am trying from R graph gallery

# Load data --------------------------------------------------------------------
Penguins <- penguins


# Functions --------------------------------------------------------------------
# No need for today's assignment. 



# Data analysis ----------------------------------------------------------------


# Homework 1
# Problem 1:
# calculate the mean and variance of body mass by 
# species, island, and sex without any NAs

# Remove NAs from the columns we are using
Penguins <- Penguins |> 
  drop_na(species, island, sex, body_mass_g)

# Mean and variance for species
Penguins |> 
  group_by(species) |> 
  summarise(mean_body_mass_g  = mean(body_mass_g, na.rm = TRUE),
            variance_body_mass_g = var(body_mass_g, na.rm = TRUE))

# Mean and variance for island
Penguins |> 
  group_by(island) |> 
  summarise(mean_body_mass_g  = mean(body_mass_g, na.rm = TRUE),
            variance_body_mass_g = var(body_mass_g, na.rm = TRUE))

# Mean and variance for sex
Penguins |> 
  group_by(sex) |> 
  summarise(mean_body_mass_g  = mean(body_mass_g, na.rm = TRUE),
            variance_body_mass_g = var(body_mass_g, na.rm = TRUE))


# Problem 2:
# filters out (i.e. excludes) male penguins, 
# then calculates the log body mass
# then selects only the columns for species, island, sex, and log body mass
# then use these data to make any plot. 
# Make sure the plot has clean and clear labels and follows best practices. 
# Save the plot in the correct output folder.

Penguins_Females <- Penguins |> 
  filter(sex != "male") |>  # Filter out male penguins 
  mutate(log_body_mass_g = log10(body_mass_g)) |>  # log body mass
  select("species", "island", "sex", "log_body_mass_g")    # select requested columns
  
# Going to try to use my template from last week's plotting exercise
# and improve a bit more
ggplot(data = Penguins_Females, 
         mapping = aes(x = species, 
                       y = log_body_mass_g,
                       color = species,
                       fill = species)) +
  geom_violin(alpha = 0.5, 
              width = 1.00,
              color = "black",
              fill = NA) + # NA for color so no border on violin plot
  geom_jitter(width = 0.25,
              alpha = 0.75,
              size = 3.0) +
  geom_boxplot(color = "black",
               width = 0.50,
               fill = NA,
               outliers = FALSE) +
  stat_summary( # this section adds the white mean bar inside of the boxplot 
    fun = mean, 
    geom = "crossbar",
    width = 0.50,
    fatten = 2,
    color = "grey",
    show.legend = FALSE
  ) +
  labs(x = "Species (females only)", # x-axis title
       y = expression("Log"[10]*" body mass (g)")) + #y-axis title (added subscipt today)
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 15), # x-axis title size
        axis.title.y = element_text(size = 15), # y-axis title size
        axis.text.x = element_text(size = 10), # x-axis text size
        axis.text.y = element_text(size = 10), #y-axis text size
        panel.grid.major.y = element_line(), # Major y axis gridlines
        panel.grid.minor.y = element_line() # Minor y axis gridlines
  )

#Save plot
ggsave(here("Week_04/Output/Homework_Plot_FemalePenguins.png"),
       width = 7, height = 5) # reminder that this is in inches




# Homework 2: 
# Using the chemistry data
ChemData <- read.csv(here("Week_04/Data/chemicaldata_maunalua.csv"))

# Create a new clean script (doing both HW's in the same script 
# like we were told we could)
# Remove all the NAs
ChemData_Clean <- ChemData |> 
  drop_na() 

# Separate the Tide_time column into appropriate columns for analysis
ChemData_Clean <- ChemData_Clean |>
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide", "Time"),
                       cols_remove = FALSE)

# Filter out a subset of data (your choice)
ChemData_Clean <- ChemData_Clean |> 
  select(Waypoint:Temp_in, pH)

# Use either pivot_longer() or pivot_wider() at least once
ChemData_Long <- ChemData_Clean |> 
  pivot_longer(cols = c("Temp_in", "pH"),
             names_to = "Variables",
             values_to = "Values")

# Calculate some summary statistics (can be anything) and export the csv file into the output folder
ChemData_Means <- ChemData_Long |> 
  group_by(Variables, Season, Tide) |> 
  summarise(mean_values = mean(Values, na.rm = TRUE),
            sd_values = sd(Values, na.rm = TRUE))

# Make any kind of plot (it cannot be a boxplot) and export it into the output folder
# Trying to make something from the R graph gallery resource
# I want to use gg marginal to have distribution around the sides

Chem_Plot <- ggplot(data = ChemData_Clean,
       aes(x = Temp_in, y = pH, 
           color = Season)) +
  geom_point() +
  scale_color_manual(values = c("FALL" = "#EE9A00", "SPRING" = "#66CD00")) +
  geom_smooth(method = "lm") + #linear trendline for each season
  labs(x = "Temperature (°C)", y = "pH")+
  theme_bw()+
  theme(legend.position = "inside", # move legend inside the plot
        legend.position.inside = c(0.90, 0.15)) # relative position of legend (1,1) is top right

Chem_Plot

# Add marginal density
# marginal density
Chem_Plot_Density <- ggMarginal(Chem_Plot, 
                                type = "density",
                                groupColour = TRUE, # Add different density plots per group
                                groupFill = TRUE,
                                alpha = 0.60)

Chem_Plot_Density

#Save plot
ggsave(here("Week_04/Output/Homework_Plot_ChemData.png"),
       plot = Chem_Plot_Density,
       width = 7, height = 5)

# Make sure you comment your code and your data, outputs, and script are in the appropriate folders
# Done :)