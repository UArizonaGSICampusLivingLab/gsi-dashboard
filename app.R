library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)

library(tidyverse)

metadata <- read.csv("metadata.csv")
data <- read.csv("gsi_living_lab_data.csv")|>
  mutate(datetime = ymd_hms(datetime))

fulldata <- merge(metadata, data, by = c("device_sn", "sensor"))

ui <- page_sidebar(
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
  card(
    full_screen = TRUE,
    card_header("Soil Moisture"),
    card_body("plot goes here")
  ),
  card(
    full_screen = TRUE,
    card_header("Precipitation"),
    card_body("plot goes here")
  ),
  card(
    full_screen = TRUE,
    card_header("Soil Temperature"),
    card_body("plot goes here")
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)