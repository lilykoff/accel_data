library(here)
library(purrr)
library(tidyverse)

url = "https://cecas.clemson.edu/tracking/Pedometer/Data.zip"

options(timeout=300)
options(digits.secs = 3)
dir.create(here::here("data/raw"), showWarnings = FALSE, recursive = TRUE)
downloader::download(url, dest=here::here("data/raw/clemsonped.zip"), mode="wb")
unzip(here::here("data/raw/clemsonped.zip"), exdir = here::here("data/raw/clemson"))


ped_files <- list.files(here::here("data/raw/clemson"), recursive = TRUE, full.names = TRUE)
acc_ped_files <- ped_files[-grep("steps.txt", ped_files)]
step_files <- ped_files[grep("steps.txt", ped_files)]
# 9 columns are wrist xyz, hip xyz and ankle xyz

get_and_join_files <-
  function(acc_ped_file){
    temp = readr::read_table(
      acc_ped_file,
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
      )
    ) %>%
      select(1:3) %>%
      mutate(
        id_type = sub("\\.txt.*", "", sub(".*\\/P", "", acc_ped_file)),
        id = str_split(id_type, "_", simplify = TRUE)[1],
        loc = str_split(id_type, "_", simplify = TRUE)[, 2],
        ind = row_number()
      )
    id = temp$id[1]
    loc = paste0("/", temp$loc[1]) # to distinguish regular and semi regular
    loc = ifelse(loc == "/Semiregular", "/SemiRegular", loc) # annoying difference in spelling
    step_file = step_files[grep(id, step_files)][grep(loc,step_files[grep(id, step_files)])]
    step_locs = read_table(step_file, col_names = c("ind", "foot"))

    temp %>%
      left_join(step_locs)
  }

ped_data <- map(.x = acc_ped_files,
                .f = get_and_join_files) %>%
  bind_rows()

start = lubridate::floor_date(as.POSIXct("2023-10-23 10:00:00", tz = "UTC"), unit = "seconds")

ped_data <-
  ped_data %>%
  mutate(
    step = ifelse(is.na(foot), 0, 1)
  ) %>%
  rename(X = wrist_x, Y = wrist_y, Z = wrist_z) %>%
  group_by(id_type) %>%
  mutate(time_s = (ind-1)/15,
         time = start + as.period(time_s, unit = "seconds")) %>%
  ungroup()

ped_data_proc <-
  ped_data %>%
  mutate(cat_activity = paste0("walk_", tolower(loc)),
         cat_step_type = ifelse(is.na(foot), "none", foot),
         id_study = "clemson_ped") %>%
  select(X, Y, Z,
         tm_dttm = time, id_subject = id, ind_step = step,
         cat_step_type,
         cat_activity,
         id_study)

dir.create(here::here("data/processed"), showWarnings = FALSE, recursive = TRUE)
write_csv(ped_data_proc, here::here("data/processed/clemson_ped.csv"))
R.utils::gzip("data/processed/clemson_ped.csv")

system("rm clemsonped.zip")

