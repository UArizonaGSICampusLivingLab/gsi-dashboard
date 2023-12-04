library(boxr)
library(jose)
library(dplyr)
library(fs)


gsi_get_data <- function(path = "data", overwrite = TRUE) {
  dir_id <- "233031886906"
  box_setwd(dir_id)
  #create path if it doesn't exist
  dir_create(path)
  box_ls() |>
    as_tibble() |> 
    filter(name == "gsi_living_lab_data.csv") |> 
    pull(id) |> 
    box_dl(local_dir = path, overwrite = overwrite)
}
