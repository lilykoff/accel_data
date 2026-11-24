library(tidyverse)
options(digits.secs = 3)
## convert for actilife - clemson
ped_files <- list.files(here::here("data/raw/clemson"), recursive = TRUE, full.names = TRUE)
acc_ped_files <- ped_files[-grep("steps.txt", ped_files)]


start = lubridate::floor_date(as.POSIXct("2023-10-23 10:00:00", tz = "UTC"), unit = "seconds")

write_to_csv = function(file){
  temp = readr::read_table(
    file,
    col_names = c(
      "wrist_x",
      "wrist_y",
      "wrist_z",
      "hip_x",
      "hip_y",
      "hip_z",
      "ankle_x",
      "ankle_y",
      "ankle_z"
    )) %>%
    select(1:3) %>%
    mutate(time_s = (row_number()-1)/15,
    time = start + as.period(time_s, unit = "seconds")) %>%
    select(HEADER_TIMESTAMP = time, X = wrist_x, Y = wrist_y, Z = wrist_z)
  fname = paste0(sub(".*\\/(.+).txt.*", "\\1", file), ".csv.gz")
  readr::write_csv(temp, here::here("data/processed/acc_data", fname))
}

dir.create(here::here("data/processed/acc_data"), showWarnings = FALSE, recursive = TRUE)
map(.x = acc_ped_files, .f = write_to_csv)

# convert for actilife- oxwalk
wrist_files_100 = list.files(here::here("data/raw/oxwalk/OxWalk_Dec2022/Wrist_100Hz"), recursive = TRUE, full.names = TRUE)
wrist_files_25 = list.files(here::here("data/raw/oxwalk/OxWalk_Dec2022/Wrist_25Hz"), recursive = TRUE, full.names = TRUE)


write_to_csv_ow = function(file){
  temp =
    readr::read_csv(file) %>%
    select(HEADER_TIMESTAMP = timestamp,  X = x, Y= y, Z =z)
  fname = paste0(sub(".*\\/(.+).csv.*", "\\1", file), ".csv.gz")
  readr::write_csv(temp, here::here("data/processed/acc_data", fname))
}

map(.x = c(wrist_files_100, wrist_files_25), .f = write_to_csv_ow)
