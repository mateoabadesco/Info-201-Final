# Data wrangling 
# Loads in your datasets
food_df <- read.csv("food_access.csv") 
county_df <- read.csv("CountyCodes.csv")

# Make a dataframe out of US State names/abbreviations that R has included
state_name_abv_df <- data.frame(State.Code = state.abb, State.Name = state.name)
#Convert state names to full names in county_df
county_df <- merge(county_df, state_name_abv_df, by.x = "State", by.y = "State.Code")

# Rename the State.Name column to State to match the column name in food_df
names(county_df)[names(county_df) == 'State'] <- "State.Code"
names(county_df)[names(county_df) == "State.Name"] <- "State"
names(county_df)[names(county_df) == "County_name"] <- "County"

# Merge food_df and county_df based on the common columns "County" and "State"
merged_data <- merge(food_df, county_df, by = c("County", "State"))


## OVERVIEW TAB INFO

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

## VIZ 1 TAB INFO

viz_1_sidebar <- sidebarPanel(
  h2("Population and Housing Distribution by State"),
  p("Analyze the distribution of population and housing units using histograms, 
    across different State."),
  p("Select state to check for distribution of populatin and housing in a 
    specific State"),
  
  selectInput("state_filter_hist", "Select State:",
              c("All States", unique(merged_data$State)),
              selected = "All States"),
  sliderInput("pop_bins_slider", "Bins Population:",
              min = 1, max = 50, value = 10),
  sliderInput("housing_bins_slider", "Bins Housing:",
              min = 1, max = 50, value = 10)
)

viz_1_main_panel <- mainPanel(
  h2("Population and Housing Distribution"),
  plotOutput(outputId = "viz1_pop_plot", height = "500px"),
  plotOutput(outputId = "viz1_housing_plot", height = "500px")
)

viz_1_tab <- tabPanel("Population & Housing Distribution",
                      sidebarLayout(
                        viz_1_sidebar,
                        viz_1_main_panel
                      )
)

## VIZ 2 TAB INFO

viz_2_sidebar <- sidebarPanel(
  h2("Population Distribution by Economic Type"),
  p("This visualization illustrates the distribution of population and housing across 
    various economic types within urban areas."),
  p("Select a state and county to view the population and housing by economic type"),
  selectInput("state_filter", "Select State:",
              c("All States", unique(merged_data$State)), 
              selected = "All States"),
  uiOutput("county_filter"),
  selectInput("county_filter", "Select County:",
              "All Counties",
              selected = "All Counties")
)

viz_2_main_panel <- mainPanel(
  h2("Population and Housing by Economic Type"),
  plotOutput(outputId = "viz2_pop_plot", height = "500px"),
  plotOutput(outputId = "viz2_housing_plot", height = "500px")
)

viz_2_tab <- tabPanel("Population & Housing by Economic Type",
                      sidebarLayout(
                        viz_2_sidebar,
                        viz_2_main_panel
                      )
)

## VIZ 3 TAB INFO

viz_3_sidebar <- sidebarPanel(
  h2("Population vs Housing Units by Economic Type"),
  p("Explore how housing units are relate to population across different economic 
  types within urban areas."),
  p("Select a state, county, and economic label to explore the relationship between 
     population and housing units across different economic types."),
  selectInput("state_filter1", "Select State:",
              c("All States", unique(merged_data$State)),
              selected = "All States"),
  uiOutput("county_filter1"),
  selectInput("county_filter1", "Select County:",
              "All Counties",
              selected = "All Counties"),
  checkboxGroupInput("economic_label_filter", "Select Economic Labels:",
                     choices = unique(merged_data$Economic_Type_Label),
                     selected = unique(merged_data$Economic_Type_Label))
)

viz_3_main_panel <- mainPanel(
  h2("Population vs Housing Units by Economic Type"),
  plotOutput(outputId = "viz3_plot", height = "800px")
)

viz_3_tab <- tabPanel("Population vs Housing Units by Economic Type",
                      sidebarLayout(
                        viz_3_sidebar,
                        viz_3_main_panel
                      )
)


## CONCLUSIONS TAB INFO

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


ui <- navbarPage("Urban Dynamics: Exploring Population and Housing Trends",
                 overview_tab,
                 viz_1_tab,
                 viz_2_tab,
                 viz_3_tab,
                 conclusion_tab
)

