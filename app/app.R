# Setup -------------------------------------------------------------------
# runs on app start up
library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)

library(tidyverse)
library(boxr)
library(glue)
library(Hmisc)
# Create theme
theme <- bs_theme(preset = "shiny")

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


# Value boxes
value_test <- value_box(
  title = "Hello!", 
  showcase = bs_icon("calendar"),
  value = "20 ºC",
  "is the temperature",
  fill = FALSE
)

last_rain <- data_full |> 
  select(site, datetime, precipitation.value) |> 
  filter(precipitation.value > 0) |> 
  filter(datetime == max(datetime)) |> pull(datetime)
days_since_last_rain <- (now() - last_rain) |> round() |> as.numeric()

value_last_rain <- 
  value_box(
    "Last rainfall",
    showcase = bs_icon("calendar"),
    value = p(glue::glue("{days_since_last_rain} days ago")),
    glue::glue("On {format(last_rain, '%a, %b %d')}")
  )

airtemp <- latest_conditions |> 
  select(site, air_temperature.value) |> 
  mutate(air_temperature.value = paste(air_temperature.value, "ºC"))
latest_datetime <- format(max(latest_conditions$datetime), "%a, %b %d %I:%M %p")

value_airtemp <- value_box(
  glue::glue("{latest_datetime}"), #"hack" to remove <p> tag.  Probably a more correct way to do this
  showcase = bs_icon("thermometer-half"),
  value = markdown(
    knitr::kable(airtemp, col.names = c("", ""), format = "pipe")
  )
)

annual_precip <-
  data_full |> 
  group_by(site) |> 
  filter(datetime >= floor_date(today(), "year")) |> 
  dplyr::summarize(
    cum_precip = sum(precipitation.value, na.rm = TRUE)
  ) |> 
  mutate(site = if_else(site == "Physics and Atmospheric Sciences", "Phys & Atm Sci", site)) |> 
  mutate(cum_precip = paste(round(cum_precip, 0), "mm"))

value_precip <- value_box(
  glue::glue("Cumulative Precipitation ({year(Sys.Date())})"),
  showcase = bs_icon("cloud-rain"),
  value = markdown(
    knitr::kable(annual_precip, col.names = c("", ""), format = "pipe")
  )
)

# UI ----------------------------------------------------------------------
ui <- page_navbar(
  theme = bs_theme_update(theme, primary = "#81D3EB", font_scale = 1.2),
  title = "GSI Living Lab", 
  id = "navbar",
  # fillable = FALSE, # make scrollable.  Try with and without this
  sidebar = sidebar(
    id = "sidebar",
    open = FALSE, #sidebar initially closed on landing page
    # This could be a value_box instead of just plain text
    paste("Data last updated ", 
          format(max(data_full$datetime, na.rm = TRUE),
                 "%Y/%m/%d %H:%M")),
    checkboxGroupInput(
      inputId = "site",
      label = "Site",
      choices = unique(data_full$site),
      selected = unique(data_full$site)
    ),
    conditionalPanel(
      "input.navbar == 'Atmospheric'",
      airDatepickerInput(
        inputId = "daterange",
        label = "Date Range",
        range = TRUE,
        # Default date range
        value = c(Sys.Date() - 7, Sys.Date()),
        dateFormat = "MM/dd/yy",
        maxDate = Sys.Date(),
        minDate = "2023-06-05",
        addon = "none",
        update_on = "close"
      ),
      conditionalPanel(
        "input.navbar == 'Atmospheric'",
        input_switch("daily", "Daily Summary")
      )
    ),
    conditionalPanel(
      "input.navbar == 'Soil'",
      airDatepickerInput(
        inputId = "monthrange",
        label = "Date Range",
        range = TRUE,
        # Default date range is a year ago or to the earliest day of data, whichever is more recent
        value = c(max(Sys.Date() - 365, as.Date("2023-06-05")), Sys.Date()), 
        dateFormat = "MM/dd/yy",
        maxDate = Sys.Date(),
        minDate = "2023-06-05",
        view = "months",
        minView = "months",
        addon = "none",
        update_on = "close"
      )
    )
  ),
  nav_panel(
    "About",
    layout_column_wrap(
      width = 1/3,
      heights_equal = "all",
      fill = FALSE,
      # fillable = FALSE,
      value_airtemp,
      value_precip,
      value_last_rain,
    ),
    card(
      includeMarkdown("about.md"),
      fill = FALSE #change this to TRUE to have the row of value boxes "frozen" to the top
    )
  ),
  nav_spacer(),
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
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # bs_themer() #temporary! Remove before deploying


  ## Open sidebar on other tabs --------------------------------------------
  
  # sidebar starts hidden, but this opens it when you switch to any tab other
  # than "About"
  observe({
    sidebar_toggle(
      id = "sidebar",
      open = input$navbar != "About"
    )
  })  
  
  
  ## Get filtered data -----------------------------------------------------
  data_filtered_atm <- reactive({
    data_full |> 
      filter(site %in% input$site) |> 
      filter(datetime >= input$daterange[1], datetime <= input$daterange[2])
  })
  data_filtered_soil <- reactive({
    data_full |> 
      filter(site %in% input$site) |> 
      filter(datetime >= input$monthrange[1], datetime <= input$monthrange[2])
  })
  
  ## Legend -------
  #can't re-use output objects, so make one for each tab
  output$legend1 <- output$legend2 <- output$legend3 <- renderUI({
    make_legend(input$site)
  })
  
  ## Plots --------
  output$plot_airtemp <- renderPlot({
    gsi_plot_airtemp(data_filtered_atm(), daily = input$daily)
  })
  
  output$plot_precip <- renderPlot({
    gsi_plot_precip(data_filtered_atm(), daily = input$daily) |> ggplotly()
  })
  
  output$plot_vp <- renderPlot({
    gsi_plot_vpd(data_filtered_atm(), daily = input$daily)
  })
  
  output$plot_soil_temp <- renderPlot({
    gsi_plot_soil(data_filtered_soil(), yvar = "soil_temperature.value") +
      labs(y = "Temperature (ºC)")
  })
  
  output$plot_soil_wc <- renderPlot({
    gsi_plot_soil(data_filtered_soil(), yvar = "water_content.value") +
      labs(y = bquote("Water Content "(m^3/m^3)))
  })
  
  output$plot_soil_matric <- renderPlot({
    gsi_plot_soil(data_filtered_soil(), yvar = "matric_potential.value") +
      labs(y = "Matric Potential (kPa)")
  })
}

shinyApp(ui, server)