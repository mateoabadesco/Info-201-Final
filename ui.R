library(plotly)
library(bslib)
library(dplyr)

# 1. Customize your app via my_style
# 2. Publish your app

# Load data
df <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/NationalNames.csv?raw=true")

# Filter data in some way
subset_df <- df %>% 
  group_by(Name) %>% 
  summarize(total = sum(Count)) %>% 
  slice_max(n = 100, order_by = total)

# Manually Determine a BootSwatch Theme
my_theme <- bs_theme(bg = "#0b3d91", #background color
                  fg = "white", #foreground color
                  primary = "#FCC780", # primary color
) 
# Update BootSwatch Theme
my_theme <- bs_theme_update(my_theme, bootswatch = "cerulean") 

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

# Create sidebar panel for widget
select_widget <-
  selectInput(
    inputId = "name_selection",
    label = "Names",
    choices = subset_df$Name,
    selectize = TRUE,
    # True allows you to select multiple choices...
    multiple = TRUE,
    selected = "Michael"
  )

radio_widget <- radioButtons(
  inputId = "gender_selection",
  label = "Gender",
  choices = c("F", "M"),
  selected = "F")

# https://shiny.rstudio.com/gallery/widget-gallery.html
slider_widget <- sliderInput(
  inputId = "year_selection",
  label = "Year",
  min = min(df$Year),
  max = max(df$Year),
  value = c(1900, 2000),
  sep = "")

# Put a plot in the middle of the page
main_panel_plot <- mainPanel(
  # Make plot interactive
  plotlyOutput(outputId = "names_plot")
)

# Data viz tab  — combine sidebar panel and main panel
viz_tab <- tabPanel(
  "Data Viz",
  sidebarLayout(
    sidebarPanel(
    select_widget,
    radio_widget,
    slider_widget),
    main_panel_plot
  )
  )

ui <- navbarPage(
  # Select Theme
  theme = my_theme,
  # Home page title
  "Home Page",
  intro_tab,
  viz_tab
)

