library(highcharter)
library(tidyverse)

data_full <- read_csv("data/data_full_ex.csv")

#simulate some filters
data_filtered <- data_full |> 
  filter(datetime > ymd("2024-01-01")) |> 
  filter(site_code %in% c("om", "pas"))

data_vp <- data_filtered |> 
  filter(!is.na(vapor_pressure.value))

#need to filter the colors also
cols <- gsi_site_colors[unique(data_filtered$site)]

#VP/VPD plot example
highchart() |>
  hc_add_series(
    data = data_vp,
    "line",
    hcaes(
      x = datetime_to_timestamp(datetime),
      y = vapor_pressure.value,
      group = site
    ),
    opacity = 0.5
  ) |>
  hc_add_series(
    data = data_vp,
    "line",
    hcaes(
      x = datetime_to_timestamp(datetime),
      y = vpd.value,
      group = site
    ),
    opacity = 0.5,
    dashStyle = "dash",
    showInLegend = FALSE #to only show one of the legends
  ) |>
  hc_xAxis(title = list(text = ""), type = "datetime") |>
  hc_colors(unname(cols))
#TODO: make legend show linetype, not site

# temp summary plot

data_temp <- 
  data_filtered |> 
  mutate(date = as_date(datetime)) |> 
  group_by(site, date) |> 
  summarise(temp_mean = mean(air_temperature.value, na.rm = TRUE),
            temp_min = min(air_temperature.value, na.rm = TRUE),
            temp_max = max(air_temperature.value, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(across(starts_with("temp_"), \(x) ifelse(is.infinite(x), NA, x)))

data_temp

#y for polygon plot is a matrix of coords x,y
df <- data.frame(x = 1:5, y = 1:5)
poly <- matrix(c(2,1, 4,1, 4,4, 2,4, 2,1), ncol = 2, byrow = TRUE)
highchart() |> 
  hc_add_series(data = df, "line", hcaes(x = x, y = y)) |> 
  hc_add_series(data = poly)

#need a function that takes x, ymin, ymax and creates the points for a polygon as a matrix
df2 <- data.frame(
  x = 1:5,
  y = 1:5,
  ymin = 0:4,
  ymax = 2:6
)

ggplot(df2, aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
  geom_ribbon() +
  theme_bw()
#wonder if I can just get the coords for the ribbon out of the ggplot object...

#equivalent polygon would be...
tribble(
  ~x, ~y,
  1, 0,
  2, 1,
  3, 2,
  4, 3,
  5, 4,
  #move to top
  5, 6,
  4, 5,
  3, 4,
  2, 3,
  1, 2,
  #close it
  1, 0
)

#programmatically
poly_df <- df2 |> reframe(
  x = c(sort(x), sort(x, decreasing = TRUE), sort(x)[1]),
  y = c(sort(ymin), sort(ymax, decreasing = TRUE), sort(ymin)[1])
) 

poly_mat <- as.matrix(poly_df)

highchart() |> 
  hc_add_series(data = poly_df, "polygon")

#TODO would need to modify to deal with NAs probably
as_poly <- function(data, x, ymin, ymax, ...) {
  data |> reframe(
    x = c({{ x }}, rev({{ x }}), {{ x }}[1]),
    y = c({{ ymin }}, rev({{ ymax }}), {{ ymin }}[1]),
    ...
  ) 
}

highchart() |>
  hc_add_series(
    data = as_poly(
      data_temp,
      x = date,
      ymin = temp_min,
      ymax = temp_max,
      .by = site
    ),
    "polygon",
    hcaes(x = x, y = y, group = site),
    opacity = 0.4,
    color = cols,
    showInLegend = FALSE
  ) |> 
  hc_add_series(
    data = data_temp,
    "line",
    hcaes(x = date, y = temp_mean, group = site),
    tooltip = list(pointFormat = "{series.name} mean: {point.y}", valueSuffix = "ºC")
  ) |>
  hc_add_series(
    data = data_temp,
    "line",
    hcaes(x = date, y = temp_max, group = site),
    color = "#00000000",
    tooltip = list(pointFormat = "{series.name} high: {point.y}", valueSuffix = "ºC")
  ) |>
  hc_add_series(
    data = data_temp,
    "line",
    hcaes(x = date, y = temp_min, group = site),
    color = "#00000000",
    tooltip = list(pointFormat = "{series.name} low: {point.y}", valueSuffix = "ºC")
  ) |>
  hc_xAxis(title = list(text = ""), type = "datetime")
#TODO figure out better way to do tooltip.  Would be nice if tooltip along line showed mean, min, and max