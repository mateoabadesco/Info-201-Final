library(bslib)
#install.packages("htmlwidgets", type = "binary")
#install.packages("DT", type = "binary")

# Preview and Select a Bootswatch Theme
bs_theme_preview()

# Manually Determine a BootSwatch Theme
theme <- bs_theme(bg = "#0b3d91", #background color
                  fg = "white", #foreground color
                  primary = "#FCC780", # primary color
)
# Preview
bs_theme_preview(theme)
