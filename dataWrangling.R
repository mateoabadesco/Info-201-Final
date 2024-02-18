library("dplyr")
library("ggplot2")

setwd("C:\\Users\\4nime.DESKTOP-MQ9FBUC\\OneDrive\\Desktop\\school\\info 201\\")

homelessness_df <- read.csv("2023-HIC-Counts-by-State.csv")
property_df <- read.csv("HPI_master.csv")

# create new var based on change in population per shelter based on growth in housing costs

PIT_df <- homelessness_df %>% group_by(CocState) %>% summarize(PIT = sum(PIT.Count, na.rm = TRUE)) 

