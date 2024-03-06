rm(list = ls())

library("dplyr")
library("ggplot2")
library("stringr")
library("tidyr")
library(plotly)
library(markdown)
library(maps)
library(mapdata)

setwd("C:\\Users\\4nime\\Desktop\\School\\Info 201\\Info-201-Final")
state_name_abv_df <- data.frame(State.Code = state.abb, State.Name = state.name)

server <- function(input, output) {

homelessness_2023_df <- read.csv("2023-HIC-Counts-by-State.csv")
homelessness_2022_df <- read.csv("2022-HIC-Counts-by-State.csv")
homelessness_2021_df <- read.csv("2021-HIC-Counts-by-State.csv")
property_df <- read.csv("HPI_master.csv")
state <- map_data("state")
state_name_abv_df$State.Name <- tolower(state_name_abv_df$State.Name)

state <- left_join(state, state_name_abv_df, by = c("region" = "State.Name"))

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
  
  output$state_plot <- renderPlotly({
    
    filtered_df <- joined_df %>% 
      # Filter for user's year selection
      filter(year == input$year_selection)
  
    state <- left_join(state, filtered_df, by = c("State.Code" = "state"))
    
    state$deviation <- (state$PIT/state$costIndex)
    state$standard_deviation <- (state$deviation - mean((state %>% group_by(region) %>% distinct(region, .keep_all = TRUE))$deviation, na.rm=TRUE))/ sd((state %>% group_by(region) %>% distinct(region, .keep_all = TRUE))$deviation, na.rm=TRUE)
    
    
    
    state_plot <- ggplot(data=state, aes(x=long, y=lat, fill=standard_deviation, group=group)) + 
      geom_polygon(color = "white") + 
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      guides(fill=FALSE) + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      ggtitle('U.S. Map with States Homlessness to housing index') + 
      coord_fixed(1.3)
    

    return(state_plot)
    
  })
  
  output$change_plot <- renderPlotly({
    
    
    filtered_df <- joined_df %>% 
      # Filter for user's year selection
      filter(year == input$year_slider[1] | year == input$year_slider[2])
    
    state <- left_join(state, filtered_df, by = c("State.Code" = "state"))

    origin <- state %>% filter(year == input$year_slider[1])
    state <- state %>% filter(year == input$year_slider[2])
    
    state$PIT <- state$PIT - origin$PIT
    state$costIndex <- state$costIndex - origin$costIndex
    
    state$deviation <- (state$PIT/state$costIndex)
    state$standard_deviation <- (state$deviation - mean((state %>% group_by(region) %>% distinct(region, .keep_all = TRUE))$deviation, na.rm=TRUE))/ sd((state %>% group_by(region) %>% distinct(region, .keep_all = TRUE))$deviation, na.rm=TRUE)
    
    state_plot <- ggplot(data=state, aes(x=long, y=lat, fill=standard_deviation, group=group)) + 
      geom_polygon(color = "white") + 
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      guides(fill=FALSE) + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
      ggtitle('U.S. Map with States Homlessness to housing index') + 
      coord_fixed(1.3)
    
    
    return(state_plot)
    
  })
  

}
