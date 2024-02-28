
rm(list = ls())

library("dplyr")
library("ggplot2")
library("stringr")
library("tidyr")
library(plotly)
library(markdown)

setwd
homelessness_2023_df <- read.csv("2023-HIC-Counts-by-State.csv")
homelessness_2022_df <- read.csv("2022-HIC-Counts-by-State.csv")
homelessness_2021_df <- read.csv("2021-HIC-Counts-by-State.csv")
property_df <- read.csv("HPI_master.csv")

# create new var based on change in population per shelter based on growth in housing costs

homelessness_df <- merge(homelessness_2021_df, homelessness_2022_df, all = TRUE)
homelessness_df <- merge(homelessness_df, homelessness_2023_df, all =  TRUE)

homelessness_df$yearState <- paste(homelessness_df$year, homelessness_df$CocState, sep = " ")

PIT_df <- homelessness_df %>% group_by(yearState) %>% summarize(PIT = sum(PIT.Count, na.rm = TRUE)) 


property_df <- property_df %>% filter(level == "State") %>% filter(yr > 2020)
property_df$yearState <- paste(property_df$yr, property_df$place_id, sep = " ")

Price_df <- property_df %>% group_by(yearState) %>% summarize(costIndex = mean(index_nsa, na.rm = TRUE)) 

joined_df <- left_join(Price_df, PIT_df, by = "yearState")

column_names <- colnames(joined_df)
print(column_names)

# Data cleaning
# Check for missing values in each column
apply(joined_df, 2, function(x) any(is.na(x)))
# From our output, we have no missing values
# Lets create a new categorical variable by Separating YearState Column
joined_df <- separate(joined_df, yearState, into = c("year", "state"), sep = " ")
joined_df

# Read in data
df <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/NationalNames.csv?raw=true")

# Filter the data in some way (aka pick a subset of names to examine — it's too large to include all names for all years)
top_names <- df %>% 
  group_by(Name) %>% 
  summarize(total = sum(Count)) %>% 
  slice_max(n = 100, order_by = total)

subset_df <- df %>% filter(Name %in% top_names$Name)

server <- function(input, output) {

  output$names_plot <- renderPlotly({

    filtered_df <- joined_df %>% 
      # Filter for user's gender selection
      filter(yearState %in% input$year_selection)
    
    # Line plot
    names_plot <- ggplot(data = joined_df) +
      geom_line(mapping = 
                  aes(x = yearState, 
                      y = PIT/costIndex, 
                      color = Name))

    return(names_plot)
    
  })

}
