pacman::p_load(tidyverse, ISOweek, lubridate)

# Clean Lyme disease case counts reported to the German surveillance system and 
# published in the web platform Survstat@RKI
# Author: Martín LB

# Import case counts and population size data
files <- list.files("data/survstat", pattern = "*.csv", full.names = TRUE)
files <- files[!grepl("Unknown", files)]
data <- lapply(files, read.csv, sep="\t", header=FALSE, fileEncoding="UTF-16")

pop <- read.csv("data/population_data.csv")

# To each dataset in data
cleaned_data <- NULL
for(i in 1:length(data)){
  cleaned_data[[i]] <- data[[i]] %>%
    # Before using the second row as column names, add a 0 to values below 10
    # that don't have one yet and then add a W for week
    mutate(across(2:ncol(.), ~ifelse(row_number() == 2 & . < 10 & 
                                       !grepl("^0", as.character(.)), 
                                     paste0("0", .), .)),
           across(2:ncol(.), ~ifelse(row_number() == 2, paste0("W", .), .))) %>%
    # Change column names
    set_names(c("nuts3", .[2,-1])) %>%
    # Remove first two rows
    slice(-c(1:2)) %>%
    # Transform columns into rows
    pivot_longer(-nuts3, names_to = "week", values_to = "counts") %>%
    # Remove case counts without district
    filter(!grepl("Unknown|^$", nuts3)) %>%
    # Extract age and sex from file names,
    # Then convert age to numeric,
    # Then convert case counts to numeric and replace NAs with zeros,
    # Then convert weeks in 2021 to dates,
    # Last, remove subdivisions in Berlin
    mutate(age = str_extract(files[i], "(?<=A).*?(?=\\.\\.|\\+)"),
           sex = str_extract(files[i], "(?<=-)[^-]*(?=\\.csv)"),
           age = as.numeric(age),
           counts = as.numeric(counts),
           counts = if_else(is.na(counts), 0, counts),
           date = ISOweek2date(paste(2021, week, "1", sep = "-")),
           nuts3 = ifelse(grepl("Berlin", nuts3), "City of Berlin", nuts3)) %>%
    # Group data and summarise it, then drop groups
    group_by(nuts3, date, age, sex) %>%
    summarise(counts = sum(counts, na.rm = TRUE), .groups = "drop") 
  
  cat(i, "\r")
}

# Join all cleaned datasets
dt <- cleaned_data %>%
  bind_rows() %>% 
  # Group age to match the population size data
  mutate(age_cat = case_when(
    age < 10 ~ "[00-10)",
    age < 20 ~ "[10-20)",
    age < 30 ~ "[20-30)",
    age < 40 ~ "[30-40)",
    age < 50 ~ "[40-50)",
    age < 60 ~ "[50-60)",
    age < 75 ~ "[60-75)",
    age < 100 ~ "[75+)",
    TRUE ~ NA_character_
  )) %>%
  # group_by(nuts3, date, age, sex) %>%
  # summarise(counts = sum(counts, na.rm = TRUE), .groups = "drop") %>%
  filter(counts > 0)

write.csv(dt, "data/clean_data.csv", row.names=FALSE)

# END