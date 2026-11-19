library(tidyverse)
library(here)


# Simple loop idea

print(paste("The year is", 2000))

years <- c(2015:2021)

for (i in years) { # i is the index
  print(paste("The year is", i)) # loop over i
}




# Pre-allocate space: empty columns as long as the years vector
year_data <- tibble(year = rep(NA, length(years)),
                    year_name = rep(NA, length(years)))

year_data



# Simple for loop 

for (i in seq_along(years)) {
  year_data$year_name[i] <- paste("The year is", years[i])
}

year_data



# Fill in the colun year too
for (i in seq_along(years)) {
  year_data$year_name[i] <- paste("The year is", years[i])
  year_data$year[i] <- years[i]
}

year_data




--------------------------------------------------------------------------------
# Real example



# Load in Moorea data

testdata <- read_csv(here("Week_13", "data", "cond_data", "011521_CT316_1pcal.csv"))

glimpse(testdata)




# List files in a directory
# Point to the folder on the computer
CondPath <- here("Week_13", "data", "cond_data")

# List all files in that path matching a pattern (here, anything with .csv)
# You can use regex to be more specific about filenames
files <- dir(path = CondPath, pattern = ".csv")

files


# Pre-allocate space for the loop
# Calculcate the mean temp and salinity for each file
# One row for each file and 3 columns
cond_data <- tibble(filename = rep(NA, length(files)), # 3 files, so we want 3 rows
                    mean_temp = rep(NA, length(files)),
                    mean_sal = rep(NA, length(files)))

cond_data

# Write the basic code first
# Test by reading in the first file
raw_data <- read_csv(paste0(CondPath, "/", files[1]))
head(raw_data)

mean_temp <- mean(raw_data$Temperature, na.rm = TRUE)
mean_temp


# Turn it into a for loop
for (i in seq_along(files)) { # loop over each file (1, 2, 3)
  
  
}

# Add in the loop over the raw data
for (i in seq_along(files)) {
  
  raw_data <- read_csv(paste0(CondPath, "/", files[i]))
  glimpse(raw_data)
  
}




# Add in the columns
for (i in seq_along(files)) {
  
  raw_data <- read_csv(paste0(CondPath, "/", files[i]))
  
  cond_data$filename[i] <- files[i]
  
}

cond_data


# Add in means
for (i in seq_along(files)) {
  
  raw_data <- read_csv(paste0(CondPath, "/", files[i]))
  
  cond_data$filename[i] <- files[i]
  cond_data$mean_temp[i] <- mean(raw_data$Temperature, na.rm = TRUE)
  cond_data$mean_sal[i] <- mean(raw_data$Salinity, na.rm = TRUE)
}

cond_data




--------------------------------------------------------------------------------
# Now do the exact same thing with purrr

# Create a vector from 1:10
  1:10 # a vector from 1 to 10 (we are going to do this 10 times)


# Simple example

# For each value in 1:10, make a vector of 15 random numbers from a normal distribution
set.seed(3817) # makes the random numbers reproducible
1:10 |> # the vector to iterate over
  map(rnorm, n = 15) # 15 random numbers from a normal distribution, in a list

# Calculate the mean of each list element
set.seed(3817)
1:10 |>
  map(rnorm, n = 15) |>
  map_dbl(mean) # the result is now a vector of type "double"

# Same thing different notation
set.seed(3817)
1:10 |>
  map(function(x) rnorm(15, x)) |> # make your own function
  map_dbl(mean)

# Option 3, using a formula
set.seed(3817)
1:10 |>
  map(~ rnorm(15, .x)) |> # changes the arguments inside the function
  map_dbl(mean)




# Bring in Moorea files using purrr

# find the files
CondPath <- here("Week_13", "data", "cond_data")
files <- dir(path = CondPath, pattern = ".csv")

files

# Or get the full file names in one less step
files <- dir(path = CondPath, pattern = ".csv", full.names = TRUE) # saves the entire path
files


# Read in the files
data <- files |>
  set_names() |> # names each list element with its file name
  map(read_csv) |> # a list of 3 dataframes
  list_rbind(names_to = "filename") # stack them; list names go in a "filename" column

data


# Calculate means
data <- files |>
  set_names() |>
  map(read_csv) |>
  list_rbind(names_to = "filename") |>
  group_by(filename) |>
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity, na.rm = TRUE))

data


# Shortcut: readr can read many files at once
data <- read_csv(files, id = "filename") |>
  group_by(filename) |>
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity, na.rm = TRUE))
data





# Other uses -------------------------------------------------------------------

# A plot for every file
file_names <- dir(path = CondPath, pattern = ".csv") # just the file names

save_temp_plot <- function(file_name) {
  site_name <- str_remove(file_name, "\\.csv")
  
  p <- read_csv(paste0(CondPath, "/", file_name)) |>
    ggplot(aes(x = date, y = Temperature)) +
    geom_line() +
    labs(title = site_name, x = "Date", y = "Temperature (°C)")
  
  ggsave(here("Week_13", "output", paste0(site_name, "_temp.png")),
         plot = p, width = 6, height = 4)
}

walk(file_names, save_temp_plot)