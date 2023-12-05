# Setup -------------------------------------------------------------------
# runs on app start up
library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)

library(tidyverse)
library(boxr)
library(glue)

# Read in metadata
site_info <- read_csv("metadata.csv")

# Download most recent data from Box
box_auth_service()
gsi_get_data()

# Read in data and join with site info
data_full <- 
  read_csv("data/gsi_living_lab_data.csv") |> 
  right_join(site_info)

# UI ----------------------------------------------------------------------
ui <- page_navbar(
  title = "GSI Living Lab",
  # fillable = FALSE, # make scrollable.  Try with and without this
  sidebar = sidebar(
    # This could be a value_box instead of just plain text
    paste("Data last updated ", 
          format(max(data_full$datetime, na.rm = TRUE),
                 "%Y/%m/%d %H:%M")),
    selectInput(
      inputId = "site",
      label = "Site",
      choices = unique(data_full$site)
    ),
    airDatepickerInput(
      inputId = "daterange",
      label = "Date Range",
      range = TRUE,
      # Default date range
      value = c(Sys.Date() - 7, Sys.Date()),
      dateFormat = "MM/dd/yy",
      maxDate = Sys.Date(),
      addon = "none",
      update_on = "close"
    )
    
  ),
  nav_panel(
    "Atmospheric",
    card(
      full_screen = TRUE,
      # I commented out the headers because i felt like they took up too much space.
      # card_header("Air Temperature"),
      plotOutput("plot_airtemp")
    ),
    card(
      full_screen = TRUE,
      # card_header("Precipitation"),
      plotOutput("plot_precip")
    ),
    card(
      full_screen = TRUE,
      # card_header("Vapor Pressure and VPD"),
      plotOutput("plot_vp")
    )
  ),
  nav_panel(
    "Soil"
  ),
  nav_panel(
    "Environmental Plots" #plant-available water, other calculated values
  ),

  nav_panel(
    "value box demo",

    layout_columns(
      height = "20%",
      uiOutput("stat_airtemp"),
      uiOutput("stat_soiltemp"),
      uiOutput("stat_precip")
    ),
    card(height = "27%",
         full_screen = TRUE,
         card_header("Soil Moisture"),
         card_body("")
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
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  data_filtered <- reactive({
    data_full |> 
      filter(site == input$site) |> 
      filter(datetime >= input$daterange[1], datetime <= input$daterange[2])
  })
  
  ## Plots --------
  output$plot_vp <- renderPlot({
    gsi_plot_vpd(data_filtered())
  })
  
  output$plot_airtemp <- renderPlot({
    gsi_plot_airtemp(data_filtered())
  })
  
  output$plot_precip <- renderPlot({
    gsi_plot_precip(data_filtered())
  })
  
  ##  Value boxes -------
  output$stat_airtemp <- renderUI({
    airtemp <- data_filtered()$air_temperature.value
    
    airtemp_vals <- 
      c(
        max(airtemp, na.rm = TRUE),
        mean(airtemp, na.rm = TRUE),
        min(airtemp, na.rm = TRUE)
      ) |> 
      round(2)
    
    value_box(
      title = "Air Temperature",
      value =  HTML(glue("
           H: {airtemp_vals[1]} ºC<br>
           M: {airtemp_vals[2]} ºC<br>
           L: {airtemp_vals[3]} ºC
           ")),
      showcase = bs_icon("thermometer")
    )
  })
  
  output$stat_soiltemp <- renderUI({
    soiltemp <- data_filtered()$soil_temperature.value
    soiltemp_vals <- 
      c(
        max(soiltemp, na.rm = TRUE),
        mean(soiltemp, na.rm = TRUE),
        min(soiltemp, na.rm = TRUE)
      ) |> 
      round(2)
    value_box(
      title = "Soil Temperature",
      value =  HTML(glue("
           H: {soiltemp_vals[1]} ºC<br>
           M: {soiltemp_vals[2]} ºC<br>
           L: {soiltemp_vals[3]} ºC
           ")),
      showcase = bs_icon("thermometer")
    )
  })
  
  output$stat_precip<- renderUI({
    
    precip_total <- 
      data_filtered()$precipitation.value |> 
      sum(na.rm = TRUE) |> 
      round(1)
    
    value_box(
      title = "Total Precipitation",
      #TODO: check that units are correct
      value = paste(precip_total, "mm"),
      showcase = bs_icon("cloud-rain")
    )
  })
}

shinyApp(ui, server)