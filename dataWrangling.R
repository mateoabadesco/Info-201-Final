library("dplyr")
library("ggplot2")
library("stringr")

setwd("C:\\Users\\4nime.DESKTOP-MQ9FBUC\\OneDrive\\Desktop\\school\\info 201\\")

homelessness_2023_df <- read.csv("2023-HIC-Counts-by-State.csv")
homelessness_2022_df <- read.csv("2022-HIC-Counts-by-State.csv")
homelessness_2021_df <- read.csv("2021-HIC-Counts-by-State.csv")
property_df <- read.csv("HPI_master.csv")

# create new var based on change in population per shelter based on growth in housing costs

homelessness_df <- merge(homelessness_2021_df, homelessness_2022_df, all = TRUE)
homelessness_df <- merge(homelessness_df, homelessness_2023_df, all =  TRUE)

homelessness_df$yearState <- paste(homelessness_df$year, homelessness_df$CocState, sep = " ")

PIT_df <- homelessness_df %>% group_by(yearState) %>% summarize(total_estimate = sum(PIT.Count, na.rm = TRUE)) 


property_df <- property_df %>% filter(level == "State") %>% filter(yr > 2020)
property_df$yearState <- paste(property_df$yr, property_df$place_id, sep = " ")

Price_df <- property_df %>% group_by(yearState) %>% summarize(total_estimate = mean(index_nsa, na.rm = TRUE)) 

joined_df <- left_join(Price_df, PIT_df, by = "yearState")

          