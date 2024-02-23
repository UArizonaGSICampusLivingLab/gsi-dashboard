gsi_hchart_airtemp <- function(data, colors = gsi_site_colors) {
  #need to filter the colors to match the data
  selected_sites <- unique(data$site)
  selected_colors <- colors[selected_sites]
  
  #summarize data to daily
  data_temp <- 
    data |> 
    mutate(date = as_date(datetime)) |> 
    group_by(site, date) |> 
    summarise(temp_mean = mean(air_temperature.value, na.rm = TRUE),
              temp_min = min(air_temperature.value, na.rm = TRUE),
              temp_max = max(air_temperature.value, na.rm = TRUE)) |> 
    ungroup() |> 
    mutate(across(starts_with("temp_"), \(x) ifelse(is.infinite(x), NA, x)))
  
  #build chart
  highchart() |>
    #ribbon
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
      color = selected_colors,
      showInLegend = FALSE
    ) |> 
    #line with tooltips
    hc_add_series(
      data = data_temp,
      "line",
      hcaes(x = date, y = temp_mean, group = site, min = temp_min, max = temp_max),
      color = selected_colors,
      tooltip = list(
        pointFormat = "{series.name}<br>
                    - high: {point.max} ºC <br>
                    - mean: {point.y} ºC <br>
                    - low: {point.min} ºC <br>")
    ) |> 
    hc_xAxis(title = list(text = ""), type = "datetime")
  
  
}

#WARNING: this doesn't handle NAs!  Would probably need to be a lot more complicated to figure out where to split polygons.
#converts x, ymin, ymax to matrix of x and y points outlining ribbon
as_poly <- function(data, x, ymin, ymax, ...) {
  data |> reframe(
    x = c({{ x }}, rev({{ x }}), {{ x }}[1]),
    y = c({{ ymin }}, rev({{ ymax }}), {{ ymin }}[1]),
    ...
  ) 
}

# gsi_hchart_airtemp(data_full)
