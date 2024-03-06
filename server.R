library(tidyverse)

server <- function(input, output, session){
  
  # Filtered data reactive expression
  filtered_data_hist <- reactive({
    filtered <- merged_data
    if (input$state_filter_hist != "All States") {
      filtered <- filtered[filtered$State == input$state_filter_hist, ]
    }
    return(filtered)
  })
  
  # Render population histogram
  output$viz1_pop_plot <- renderPlot({
    ggplot(filtered_data_hist(), aes(x = Population)) +
      geom_histogram(bins = input$pop_bins_slider, fill = "skyblue", color = "black") +
      labs(title = "Population Distribution", x = "Population", y = "Frequency") +
      theme(text = element_text(size = 16))
  })
  
  # Render housing histogram
  output$viz1_housing_plot <- renderPlot({
    ggplot(filtered_data_hist(), aes(x = Housing.Data.Total.Housing.Units)) +
      geom_histogram(bins = input$housing_bins_slider, fill = "salmon", color = "black") +
      labs(title = "Housing Units Distribution", x = "Housing Units", y = "Frequency") +
      theme(text = element_text(size = 16))
  })
  
  # Filtered data reactive expression
  filtered_data <- reactive({
    filtered <- merged_data
    if (input$state_filter != "All States") {
      filtered <- filtered[filtered$State == input$state_filter, ]
    }
    if (input$county_filter != "All Counties") {
      filtered <- filtered[filtered$State == input$state_filter & filtered$County == input$county_filter, ]
    }
    return(filtered)
  })
  
  # Update county dropdown menu based on selected state
  observeEvent(input$state_filter, {
    counties <- unique(merged_data$County[merged_data$State == input$state_filter])
    updateSelectInput(session, "county_filter", choices = c("All Counties", counties))
  })
  
  # Render bar plot population
  output$viz2_pop_plot <- renderPlot({
    filtered_data() %>%
      group_by(Economic_Type_Label) %>%
      summarise(Population = sum(Population)) %>%
      ggplot(aes(x = Economic_Type_Label, y = Population, fill = Economic_Type_Label)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Population), vjust = -0.5, size = 6, color = "black") +  # Add labels to each bar
      labs(title = "Total Population by Economic Type", x = "Economic Type", y = "Total Population") +
      theme(text = element_text(size = 18))
  })
  
  # Render bar plot Housing
  output$viz2_housing_plot <- renderPlot({
    filtered_data() %>%
      group_by(Economic_Type_Label) %>%
      summarise(Housing_Data_Total_Housing_Units = sum(Housing.Data.Total.Housing.Units)) %>%
      ggplot(aes(x = Economic_Type_Label, y = Housing_Data_Total_Housing_Units, fill = Economic_Type_Label)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Housing_Data_Total_Housing_Units), vjust = -0.5, size = 6, color = "black") +  # Add labels to each bar
      labs(title = "Total Housing Units by Economic Type", x = "Economic Type", y = "Total Housing Units") +
      theme(text = element_text(size = 18))
  })
  
  # Filtered data reactive expression
  filtered_data1 <- reactive({
    filtered <- merged_data
    if (input$state_filter1 != "All States") {
      filtered <- filtered[filtered$State == input$state_filter1, ]
    }
    if (input$county_filter1 != "All Counties") {
      filtered <- filtered[filtered$State == input$state_filter1 & filtered$County == input$county_filter1, ]
    }
    if (!is.null(input$economic_label_filter) && length(input$economic_label_filter) > 0) {
      filtered <- filtered[filtered$Economic_Type_Label %in% input$economic_label_filter, ]
    }
    return(filtered)
  })
  
  # Update county dropdown menu based on selected state
  observeEvent(input$state_filter1, {
    counties <- unique(merged_data$County[merged_data$State == input$state_filter1])
    updateSelectInput(session, "county_filter1", choices = c("All Counties", counties))
  })
  
  # Render scatter plot
  output$viz3_plot <- renderPlot({
    filtered_data1() %>%
      ggplot(aes(x = Population, y = Housing.Data.Total.Housing.Units, color = Economic_Type_Label)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = "Population vs Total Housing Units", x = "Population", y = "Total Housing Units") +
      theme(text = element_text(size = 18))
  })
  
  
}
