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
library(knitr)
library(markdown)

# Create theme
theme <- bs_theme(preset = "shiny")

# Download most recent data from Box
box_auth_service(token_text = Sys.getenv("BOX_TOKEN_TEXT"))
gsi_get_data()

# Read in metadata
site_info <- read_csv("data/site_info.csv")

# Read in data and join with site info
data_full <- 
  read_csv("data/gsi_living_lab_data.csv") |> 
  right_join(site_info) |> 
  mutate(datetime = with_tz(datetime, "America/Phoenix")) 

data_et <-
  read_csv("data/gsi_living_lab_ETo.csv") |> 
  left_join(site_info)


# UI ----------------------------------------------------------------------
ui <- page_navbar(
  includeCSS("custom.css"),
  theme = bs_theme_update(
    theme,
    primary = "#81D3EB",
    font_scale = 1.1,
    # Make everything a little tighter together
    `card-cap-padding-y` = "0.2rem", #padding around contents of card_header()
    `card-spacer-y` = "0.5rem", #padding around contents of card
    spacer = "0.7rem" #spacing between cards
  ),
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
        inputId = "daterange_atm",
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
        input_switch("daily", span(
          "Daily",
          tooltip(bs_icon("info-circle"),
                  "Display temperature and RH are as mean ± range and precipitaiton as daily totals.")
        ))
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
    ),
    conditionalPanel(
      "input.navbar == 'Environmental'",
      airDatepickerInput(
        inputId = "daterange_env",
        label = "Date Range",
        range = TRUE,
        # Default date range
        value = c(Sys.Date() - 7, Sys.Date()),
        dateFormat = "MM/dd/yy",
        maxDate = Sys.Date(),
        minDate = "2023-06-05",
        addon = "none",
        update_on = "close"
      )
    )
  ),
  nav_panel(
    "About",
    div(
      img(src = "bannerimage.png", align = "center", style = "width: 100%"),
      div(
        h2("Campus Living Lab"),
        h3("Green Stormwater Infrastructure"),
        class = "centered"
      ),
      class = "container"
    ),
    layout_column_wrap(
      width = 1/3,
      heights_equal = "all",
      fill = FALSE,
      make_value_latest(data_full),
      make_value_feelslike(data_full),
      make_value_precip(data_full)
    ),
    card(
      includeMarkdown("about.md"),
      fill = FALSE #change this to TRUE to have the row of value boxes "frozen" to the top
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
      plotOutput("plot_rh")
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
    "Environmental",
    htmlOutput("legend3"),
    card(
      full_screen = TRUE,
      card_header(
        "Adjusted temperature",
        #extra info in popover button
        popover(
          actionLink("link", label = bs_icon("question-circle")),
            "Lines indicate dry-bulb temperature and arrows pointing up or down from lines indicate either wind chill or heat index temperatures."
        ),
        class = "d-flex justify-content-between" #moves icon to right
      ),
      plotOutput("plot_temp_adj")
    ),
    card(
      full_screen = TRUE,
      card_header(
        "Plant Available Water",
        popover(
          actionLink("link", label = bs_icon("question-circle")),
          "Plant available water calculated using the ____ method."
        ),
        class = "d-flex justify-content-between" #moves icon to right
      ),
      plotOutput("plot_paw")
    ),
    card(
      full_screen = TRUE,
      card_header(
        "Potential Evapotranspiration",
        popover(
          actionLink("link", label = bs_icon("question-circle")),
          markdown("Calculated using the [Penman-Monteith](https://en.wikipedia.org/wiki/Penman–Monteith_equation) method")
        ),
        class = "d-flex justify-content-between" #moves icon to right
      ),
      plotOutput("plot_et")
    ),
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
      filter(date(datetime) >= input$daterange_atm[1],
             date(datetime) <= input$daterange_atm[2])
  })
  data_filtered_soil <- reactive({
    data_full |> 
      filter(site %in% input$site) |> 
      filter(date(datetime) >= input$monthrange[1],
             date(datetime) <= input$monthrange[2])
  })
  data_filtered_env <- reactive({
    data_full |> 
      filter(site %in% input$site) |> 
      filter(date(datetime) >= input$daterange_env[1], 
             date(datetime) <= input$daterange_env[2])
  })
  data_filtered_et <- reactive({
    data_et |> 
      filter(site %in% input$site) |> 
      filter(date(datetime) >= input$daterange_env[1], 
             date(datetime) <= input$daterange_env[2])
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
    gsi_plot_precip(data_filtered_atm(), daily = input$daily)
  })
  
  output$plot_rh <- renderPlot({
    gsi_plot_rh(data_filtered_atm(), daily = input$daily)
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
  
  output$plot_et <- renderPlot({
    gsi_plot_et(data_filtered_et())
  })
  
  output$plot_paw <- renderPlot({
    gsi_plot_paw(data_filtered_env())
  })
  
  output$plot_temp_adj <- renderPlot({
    gsi_plot_temp_adj(data_filtered_env())
  })
}

shinyApp(ui, server)