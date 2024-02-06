#functions to make value boxes.  All of these start with data_full that is created at the top of app.R
#for testing purposes:
# site_info <- read_csv(here("app/data/site_info.csv"))
# data_full <-
#   read_csv(here("app/data/gsi_living_lab_data.csv")) |>
#   right_join(site_info) |>
#   mutate(datetime = with_tz(datetime, "America/Phoenix"))


make_value_test <- function() {
  value_box(
    title = "Hello!", 
    showcase = bs_icon("calendar"),
    value = "20 ºC",
    "is the temperature",
    fill = FALSE
  )
}

# Current temp and precip from all three sites
make_value_latest <- function(data_full) {
  #Summarize data
  latest_airtemp <-
    data_full |> 
    group_by(site) |> 
    select(datetime, air_temperature.value) |> 
    filter(!is.na(air_temperature.value)) |> 
    filter(datetime == max(datetime)) |> 
    slice(1)|>
    ungroup()
  
  latest_precip <-
    data_full |> 
    group_by(site) |> 
    select(datetime, precipitation.value) |> 
    filter(!is.na(precipitation.value)) |> 
    filter(datetime == max(datetime)) |> 
    slice(1)|>
    ungroup()
  
  latest_conditions <- 
    full_join(latest_airtemp, latest_precip) |> 
    #abbreviate PAS
    mutate(site = if_else(site == "Physics and Atmospheric Sciences", "Phys & Atm Sci", site)) |> 
    mutate(air_temperature.value = paste(air_temperature.value, "ºC"),
           precipitation.value = paste(round(precipitation.value), "mm"))
  
  # construct value box
  value_box(
    glue::glue("Current Conditions"), 
    showcase = bs_icon("calendar"),
    value = markdown(
      knitr::kable(latest_conditions |> select(-datetime), col.names = c("", "🌡", "🌧️"), format = "pipe")
    )
  )
}
#make_value_latest(data_full)

# Cumulative precip since Jan 1 of current year
make_value_precip <- function(data_full) {
  # Summarize data
  annual_precip <-
    data_full |> 
    group_by(site) |> 
    filter(datetime >= floor_date(today(), "year")) |> 
    dplyr::summarize(
      cum_precip = sum(precipitation.value, na.rm = TRUE)
    ) |> 
    mutate(site = if_else(site == "Physics and Atmospheric Sciences", "Phys & Atm Sci", site)) |> 
    mutate(cum_precip = paste(round(cum_precip, 0), "mm"))
  
  #Construct value box
  value_box(
    glue::glue("Total precipitaiton since Jan 1, {year(today())}"),
    showcase = bs_icon("cloud-rain"),
    value = markdown(
      knitr::kable(annual_precip, col.names = c("", ""), format = "pipe")
    )
  )
}
# make_value_precip(data_full)


make_value_lastrain <- function(data_full) {
  #summarize data
  last_rain <- 
    data_full |> 
    select(site, datetime, precipitation.value) |> 
    filter(precipitation.value > 0) |> 
    filter(datetime == max(datetime)) |> 
    pull(datetime) |> 
    unique()
  
  days_since_last_rain <- (now() - last_rain) |> round() |> as.numeric()
  value <- glue::glue("{days_since_last_rain} {ifelse(days_since_last_rain == 1,'day', 'days')} ago")
  
  #construct value box
  value_box(
    "Last rainfall",
    showcase = bs_icon("calendar"),
    value = p(value),
    glue::glue("On {format(last_rain, '%a, %b %d')}")
  )
}

# make_value_lastrain(data_full)
