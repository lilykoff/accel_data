library(readr)
library(purrr)
library(tidyverse)

options(digits.secs = 2)
# download raw data
url = "https://archive.ics.uci.edu/static/public/319/mhealth+dataset.zip"
dir.create(here::here("data/raw"), showWarnings = FALSE, recursive = TRUE)
downloader::download(url, dest=here::here("data/raw/mhealth.zip"), mode="wb")
unzip(here::here("data/raw/mhealth.zip"), exdir = here::here("data/raw/"))


# process
sub_files = list.files(here::here("data/raw/MHEALTHDATASET"), full.names = TRUE)
sub_files = sub_files[grepl("subject", sub_files)]

mhealth_dat = map(
  .x = sub_files,
  .f = function(file) {
    read_table(file, col_names = FALSE) %>%
      mutate(id = sub(".*subject", "", sub(".log.*", "", file)))
  }
) %>%
  bind_rows()

start_time = ymd_hms("2000-01-01 00:00:00", tz = "UTC") # make arbitrary start time

mhealth_dat_proc =
  mhealth_dat %>%
  select(X = X15,
         Y = X16,
         Z = X17,
         id_subject = id,
         cat_activity_raw = X24,
         ) %>%
  mutate(
    cat_activity_clean =
      case_when(
        cat_activity_raw == 1 ~ "stand",
        cat_activity_raw == 2 ~ "sit",
        cat_activity_raw == 3 ~ "lie_down",
        cat_activity_raw == 4 ~ "walk",
        cat_activity_raw == 5 ~ "climb_stairs",
        cat_activity_raw == 6 ~ "waist_bend_forward",
        cat_activity_raw == 7 ~ "front_elevation_arm",
        cat_activity_raw == 8 ~ "knee_bend_crouch",
        cat_activity_raw == 9 ~ "cycle",
        cat_activity_raw == 10 ~ "jog",
        cat_activity_raw == 11 ~ "run",
        cat_activity_raw == 12 ~ "jump_front_back",
        cat_activity_raw == 0 ~ "null"
      )
  ) %>%
  group_by(id_subject) %>%
  mutate(tm_sec = (row_number()-1)/50,
         tm_dttm = as.period(tm_sec) + start_time) %>%
  ungroup() %>%
  mutate(id_study = "mhealth",
         id_subject = as.character(id_subject)) %>%
  mutate(across(c("X", "Y", "Z"), ~ .x *  0.10197162129779)) %>% # convert from m/s^2 to g
  select(X, Y, Z, tm_dttm, id_subject, id_study, cat_activity = cat_activity_clean)

dir.create(here::here("data/processed"), showWarnings = FALSE, recursive = TRUE)
write_csv(mhealth_dat_proc, here::here("data/processed/mhealth.csv.gz"))
system("rm mhealth.zip")

