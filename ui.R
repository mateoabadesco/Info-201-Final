library("dplyr")
library("ggplot2")
library("stringr")
library("tidyr")
library(plotly)
library(markdown)

# 1. Customize your app via my_style
setwd("C:\\Users\\4nime\\Desktop\\School\\Info 201\\Info-201-Final")
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


# 2. Publish your app

# Home page tab
overview_tab <- tabPanel("Introduction",
                         h1("Exploring Population and Housing Trends Across Economic Types"),
                         p("Welcome to our immersive journey into urban dynamics. This project offers 
     an insightful exploration of the intricate interplay between population 
     and housing trends across diverse economic landscapes within urban areas.", 
                           style = "font-size: 18px;"),
                         
                         p("Our interactive tool provides a rich tapestry of analyses, unraveling the 
     complex patterns of population distribution, housing unit allocation, and 
     their profound relationship with various economic factors across 
     multifaceted geographic regions. Through dynamic visualizations and 
     data-driven insights, users are invited to embark on a voyage of discovery,
     uncovering the multifaceted layers of urban dynamics.", 
                           style = "font-size: 18px;"),
                         
                         p("Embark on your exploration by navigating through the tabs, 
     each offering a unique lens through which to view urban phenomena.", 
                           style = "font-size: 18px;"),
                         
                         p("In the 'Population & Housing Distribution' tab, delve into the 
     distribution patterns of population and housing units across different 
     economic types within urban areas. Explore interactive visualizations to 
     gain deeper insights into the spatial dynamics of urban populations and 
     housing.", style = "font-size: 18px;"),
                         
                         p("The 'Population vs. Housing' tab allows you to examine the relationship 
     between population and housing units across various economic types. Through 
     scatter plots and trend analysis, uncover correlations and patterns that 
     shed light on the urban landscape.", style = "font-size: 18px;"),
                         
                         p("Finally, the 'Conclusion' tab offers a summary of key insights gleaned 
     from the analysis. Reflect on the findings and implications for urban 
     planning, policy-making, and future research endeavors.", 
                           style = "font-size: 18px;")
                         
)

conclusion_tab <- tabPanel("Conclusion",
                           h1("Conclusion"),
                           p("In conclusion, this project offers valuable insights into the dynamics of urban 
    populations and housing. By examining population and housing distributions across 
    different economic types, we gain a deeper understanding of the socioeconomic 
    landscape within urban areas.", style = "font-size: 18px;"),
                           p("The interactive visualizations provided in this tool allow users to explore 
    various aspects of the data and draw meaningful conclusions. From analyzing 
    population and housing trends to exploring their relationship with economic 
    types, this tool serves as a valuable resource for urban planners, policymakers, 
    and researchers alike.", style = "font-size: 18px;")
)


# We want our next tab to have a sidebar layout
# So we're going to create a sidebarPanel() and a mainPanel() and then add them together

# https://shiny.rstudio.com/gallery/widget-gallery.html
slider_widget <- selectInput(
  inputId = "year_selection",
  label = "Year",
  selected = "2021",
  choices = joined_df$year)

# Put a plot in the middle of the page
main_panel_plot <- mainPanel(
  # Make plot interactive
  plotlyOutput(outputId = "state_plot")
)

# Data viz tab  — combine sidebar panel and main panel
viz_tab <- tabPanel(
  "Data Viz",
  sidebarLayout(
    sidebarPanel(
    slider_widget),
    main_panel_plot
  )
  )

ui <- navbarPage(
  # Home page title
  "Home Page",
  overview_tab,
  viz_tab,
  conclusion_tab
)

