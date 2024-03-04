library(boxr)
library(jose)
library(dplyr)
library(fs)


gsi_get_data <- function(path = "data", overwrite = TRUE) {
  dir_id <- "250527085917"
  box_setwd(dir_id)
  #create path if it doesn't exist
  dir_create(path)
  box_ls() |>
    as_tibble() |> 
    filter(name %in% c("gsi_living_lab_data.csv", "gsi_living_lab_ETo.csv")) |> 
    pull(id) |> 
    map(\(x) {
      box_dl(x, local_dir = path, overwrite = overwrite)
    })
}

