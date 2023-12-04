library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)

library(tidyverse)

metadata <- read.csv("metadata.csv")
data <- read.csv("gsi_living_lab_data.csv")|>
  mutate(datetime = ymd_hms(datetime))

fulldata <- merge(metadata, data, by = c("device_sn", "sensor"))

ui <- page_navbar(
  title = "GSI Living Lab",
  # fillable = FALSE, # make scrollable.  Try with and without this
  sidebar = sidebar(
    value_box(
      title = "Data Last Updated",
      value = Sys.Date(),
      showcase = bs_icon("calendar")
    ),
    selectInput(
      inputId = "site",
      label = "Site",
      choices = unique(fulldata$site)
    ),
    airDatepickerInput(
      inputId = "daterange",
      label = "Date Range",
      range = TRUE,
      value = c(Sys.Date() - 7, Sys.Date()),
      dateFormat = "MM/dd/yy",
      maxDate = Sys.Date(),
      addon = "none"
    )
    
  ),
  nav_panel(
    "Timeseries",
    
    layout_columns(
      height = "20%",
      value_box(
        title = "Mean Air Temp.",
        value = "20 ºC",
        showcase = bs_icon("thermometer")
      ),
      value_box(
        title = "Mean Soil Temp.",
        value = "20 ºC",
        showcase = bs_icon("thermometer")
      ),
      value_box(
        title = "Total Precip.",
        value = "0 mm",
        showcase = bs_icon("cloud-rain")
      ),
    ),
    card(height = "27%",
         full_screen = TRUE,
         card_header("Soil Moisture"),
         card_body("plot goes here")
    ),
    card(height = "27%",
         full_screen = TRUE,
         card_header("Precipitation"),
         card_body("plot goes here")
    ),
    card(height = "27%",
         full_screen = TRUE,
         card_header("Soil Temperature"),
         card_body("plot goes here")
    )
  ),
  nav_panel(
    "Monthly Summary"
  ),
  nav_panel(
    "Explore"
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)