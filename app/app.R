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
site_info <- read_csv("data/site_info.csv")

# Download most recent data from Box
box_auth_service(token_text = Sys.getenv("BOX_TOKEN_TEXT"))
gsi_get_data()

# Read in data and join with site info
data_full <- 
  read_csv("data/gsi_living_lab_data.csv") |> 
  right_join(site_info) |> 
  mutate(datetime = with_tz(datetime, "America/Phoenix"))
# legend <- make_legend(unique(data_full$site))
# UI ----------------------------------------------------------------------
ui <- page_navbar(
  title = "GSI Living Lab",
  # fillable = FALSE, # make scrollable.  Try with and without this
  sidebar = sidebar(
    # This could be a value_box instead of just plain text
    paste("Data last updated ", 
          format(max(data_full$datetime, na.rm = TRUE),
                 "%Y/%m/%d %H:%M")), #TODO check that timezone is AZ and not UTC
    checkboxGroupInput(
      inputId = "site",
      label = "Site",
      choices = unique(data_full$site),
      selected = unique(data_full$site)
    ),
    airDatepickerInput(
      inputId = "daterange",
      label = "Date Range",
      range = TRUE,
      # Default date range
      # TODO maybe have this depend on which tab.  If its in the soil tab it should show a full year by default.
      value = c(Sys.Date() - 60, Sys.Date()),
      dateFormat = "MM/dd/yy",
      maxDate = Sys.Date(),
      minDate = "2023-06-05",
      addon = "none",
      update_on = "close"
    )
    
  ),
  nav_panel(
    "Atmospheric",
    htmlOutput("legend1"),
    card(
      full_screen = TRUE,
      plotOutput("plot_airtemp")
    ),
    card(
      full_screen = TRUE,
      plotOutput("plot_precip")
    ),
    card(
      full_screen = TRUE,
      plotOutput("plot_vp")
    )
  ),
  nav_panel(
    "Soil",
    htmlOutput("legend2"),
    card(
      full_screen = TRUE,
      plotOutput("plot_soil_temp")
    ),
    card(
      full_screen = TRUE,
      plotOutput("plot_soil_wc")
    ),
    card(
      full_screen = TRUE,
      plotOutput("plot_soil_matric")
    )
  ),
  nav_panel(
    "Environmental Plots",
    htmlOutput("legend3"),
  ),

  nav_panel(
    "value box demo", 
    #TODO: try putting these in sidebar
    layout_columns(
      height = "20%",
      uiOutput("stat_airtemp"),
      uiOutput("stat_soiltemp"),
      uiOutput("stat_precip")
    )
  ),
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  data_filtered <- reactive({
    data_full |> 
      filter(site %in% input$site) |> 
      filter(datetime >= input$daterange[1], datetime <= input$daterange[2])
  })
  ## Legend -------
  #can't re-use output objects, so make one for each tab
  output$legend1 <- output$legend2 <- output$legend3 <- renderUI({
    make_legend(input$site)
  })
  
  ## Plots --------
  output$plot_vp <- renderPlot({
    gsi_plot_vpd(data_filtered())
  })
  
  output$plot_airtemp <- renderPlot({
    gsi_plot_airtemp(data_filtered())
    # daily summarized alternative:
    # gsi_plot_airtemp_daily(data_filtered())
    # Idea: hook this up to a switch in the card so you can switch between hourly and daily views?
  })
  
  output$plot_precip <- renderPlot({
    gsi_plot_precip(data_filtered())
  })
  
  output$plot_soil_temp <- renderPlot({
    gsi_plot_soil(data_filtered(), yvar = "soil_temperature.value") +
      labs(y = "Temperature (ºC)")
  })
  
  output$plot_soil_wc <- renderPlot({
    gsi_plot_soil(data_filtered(), yvar = "water_content.value") +
      labs(y = bquote("Water Content "(m^3/m^3)))
  })
  
  output$plot_soil_matric <- renderPlot({
    gsi_plot_soil(data_filtered(), yvar = "matric_potential.value") +
      labs(y = "Matric Potential (kPa)")
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