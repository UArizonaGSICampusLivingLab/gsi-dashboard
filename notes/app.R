library(shiny)
library(zentracloud)
library(fs)

# Shiny app to experiment with caching zentracloud downloads
# Published app: https://viz.datascience.arizona.edu/content/ece90348-8047-4f48-8f5f-ebea938c98ee/
# Unfortunately, even caching data separately for each session, the app gets held up when two requests are made at the same time by two different people (or two browser windows).  The second request waits until the first request is finished before starting.  This is because the two sessions are running off of the same server. This might not be a big deal if the requests are quick, but since they are currently taking *at least* 60 seconds, that means if you request data right after someone else did, then your wait is at least 2 minutes!!
# It might actually be useful to go back to a shared persistent cache among all sessions.  The *actual* problem might be that sessions are locked when another session requests data.

shinyApp(
  ui = fluidPage(
    p("currently cached files"),
    verbatimTextOutput("cache_files"),
    verbatimTextOutput("data"),
    tags$hr(),
    p("the first time you click this, it'll take ~60sec to get the data.  Subsequent button pushes should be nearly instantaneous"),
    actionButton("go", "Get Data")
  ),
  server = function(input, output, session) {
    
    ## hard-coding the cache dir like this works if only one person at a time uses
    ## the app.  When two people try to use it and request data at the same time,
    ## it makes one of the instances of the app unresponsive while the other
    ## finishes updating the data.
    # cache_dir <- "cache_dir"
    
    # Instead, each session gets its own separate cache using the unique session token
    cache_dir <- fs::path("cache", session$token)
    
    setZentracloudOptions(
      token = Sys.getenv("ZENTRACLOUD_TOKEN"),
      domain = "default",
      cache_dir = cache_dir
    )
    
    
    output$cache_files <- renderPrint({
      input$go
      if (fs::dir_exists(cache_dir)) {
        fs::dir_ls(cache_dir, recurse = TRUE, fail = FALSE)
      } else {
        cat("Cache will be created at ", cache_dir)
      }
    })
    # Show str(data)
    # (update with current response when Submit is clicked)
    output$data <- renderPrint({
      req(input$go)
      readings <- getReadings(
        device_sn = "z6-19484",
        start_time = "2023-8-01 00:00:00",
        end_time = "2023-8-01 12:00:00"
      )
      str(readings)
    })     
    
    session$onSessionEnded(function() {
      cat("Clearing cache")
      fs::dir_delete(fs::path(cache_dir))
    })
  }
)