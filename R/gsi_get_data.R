library(boxr)
library(jose)
library(dplyr)


gsi_get_data <- function(overwrite = TRUE) {
  dir_id <- "233031886906"
  box_setwd(dir_id)

  box_ls() |>
    as_tibble() |> 
    filter(name == "gsi_living_lab_data.csv") |> 
    pull(id) |> 
    box_dl(local_dir = "data", overwrite = overwrite)
}
