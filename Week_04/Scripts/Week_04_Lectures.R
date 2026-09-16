# Today we are plotting the penguin data again 
### Created by: Cameron Nemeth 
### Updated on: 2026-09-15

# Load Libraries 
library(palmerpenguins)
library(tidyverse)
library(here)

# Load data 
# The data is part of the package and is called penguins
glimpse(penguins)


# Lecture 1 

# Create a dataframe
Penguins <- penguins

# Add column to convert body mass to kg
Penguins <- mutate(Penguins, body_mass_kg = body_mass_g / 1000)

# Filter only the female penguins
filter(penguins, sex == "female")

# Filter for penguins in year 2008 and body mass greater than 5000
filter(penguins, year == 2008, body_mass_g > "5000")

# Dataframe for fat penguins only
FatPenguins <- Penguins %>%
  filter(body_mass_g > 5000)
 
# Dataframe for penguins from 2008 or 2009
OldPenguinData <- Penguins %>%
  filter(year == 2008| year == 2009)

# Penguins not from island dream
Penguins_NotIslandDream <- Penguins %>%
  filter(island != "Dream")

# Penguins in the species adeleide and gentoo
Penguins_Adelie_Gentoo <- Penguins %>%
  filter(species == "Adelie" | species == "Gentoo")

# Add size column where body mass greater than 4000 is big 
# and everything else is small
Penguins <- Penguins %>%
  mutate(Size = if_else(body_mass_g > 4000, "Big", "Small"))

# Add a column for rough wing size of long or short, long if greater than 200 mm
Penguins <- Penguins |> # new pipe
  mutate(WingSize = if_else(flipper_length_mm > 200, "Long", "Short"))

# Arrange penguins dataframe by body mass (default is ascending)
Penguins <- Penguins |> 
  arrange(body_mass_g)

# Summarize data, mean and min, no NAs
penguins |>
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
            min_flipper  = min(flipper_length_mm, na.rm = TRUE))

# Summarize data again, but group by island
penguins |>
  group_by(island) |>
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length  = max(bill_length_mm, na.rm = TRUE),
            n                = n())

# Summarize data again, but group by island and sex
penguins |>
  group_by(island, sex) |>
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length  = max(bill_length_mm, na.rm = TRUE),
            n                = n())

# Count species in penguins 
penguins |> 
  count(species)

# Count by species and island
penguins |> 
  count(species, island)

# Remove NAs
penguins |> 
  drop_na(sex)

# Do  multiple of these at once
penguins |> 
  drop_na(sex) |> 
  group_by(island, sex) |> 
  summarise(mean_bill_length  = mean(bill_length_mm, na.rm = TRUE))




# Lecture 2 

# Load new datasets

ChemDataDictionary <- read.csv(
  here("Week_04", "Data", "chem_data_dictionary.csv")
)

ChemData <- read.csv(here("Week_04/Data/chemicaldata_maunalua.csv")) # Interesting the two different formats work the same

# Another way to remove all NAs in a dataset
ChemData_Clean <- ChemData |> 
  filter(complete.cases(ChemData)) # Filters everything that is not a complete row

# Separate the Tide_time column
ChemData_Clean <- ChemData_Clean |> 
  separate_wider_delim(
  cols = Tide_time,
  delim = "_",
  names = c("Tide", "Time"),
  cols_remove = FALSE) #To keep original column


# Combining columns with paste()
ChemData_Clean <- ChemData_Clean |> 
  mutate(Site_Zone = paste(Site, Zone, sep = "."))

# Pivoting between wide and long data
# Our data is currently wide
ChemData_Long <- ChemData_Clean |> 
  pivot_longer(cols = Temp_in:percent_sgd, # Columns we will pivot
               names_to = "Variables", #New column for old column names
               values_to = "Values") # new column for the values

# means and variance for all variables
# ChemData_Long |> 
#  group_by(Variables, Site, Zone, Tide) |> 
#  summarise(Params_means = mean(Values, na.rm = TRUE),
#            Params_vars = var(Values, na.rm = TRUE),
#            Params_SD = sd(Values, na.rm = TRUE))

# Example using facet wrap with long data
ChemData_Long |> 
  ggplot(aes(x = Site, y = Values)) +
  geom_boxplot()+
  facet_wrap(~Variables, scales = "free")


# Pivot wide
ChemData_Wide <- ChemData_Long |> 
  pivot_wider(names_from = Variables,
              values_from = Values)


# Full pipeline for summary stats and export
# Remove NAs
ChemData_Clean <- ChemData |> 
  drop_na() |> 
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide", "Time"),
                       cols_remove = FALSE) |> 
  pivot_longer(cols = Temp_in:percent_sgd,
               names_to = "Variables",
               values_to = "Values") |> 
  group_by(Variables, Site, Time) |>
  summarise(mean_vals = mean(Values, na.rm = TRUE)) |> 
  pivot_wider(names_from = Variables,
              values_from = mean_vals) |> 
  write_csv(here("Week_04", "Output", "summary.csv"))
      
                    
                                          