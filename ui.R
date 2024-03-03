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
intro_tab <- tabPanel(
  # Title of tab
  "Introduction",
  fluidPage(
    # Include a Markdown file!
    includeMarkdown("sample-text.md"),
    p("Our project is focusing...")
  )
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
  intro_tab,
  viz_tab
)

