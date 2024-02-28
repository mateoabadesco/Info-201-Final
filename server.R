library(ggplot2)
library(plotly)
library(dplyr)

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

    filtered_df <- subset_df %>% 
      # Filter for user's gender selection
      filter(Gender %in% input$gender_selection) %>%
      # Filter for user's name selection
      filter(Name %in% input$name_selection) %>% 
      # Filter for user's year selection
      filter(Year > input$year_selection[1] & Year < input$year_selection[2])
    
    # Line plot
    names_plot <- ggplot(data = filtered_df) +
      geom_line(mapping = 
                  aes(x = Year, 
                      y = Count, 
                      color = Name))

    return(names_plot)
    
  })

}
