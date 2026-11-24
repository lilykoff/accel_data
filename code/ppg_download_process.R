library(readr)
library(purrr)
library(tidyverse)
library(magrittr)

options(digits.secs = 3)
source(here::here("code/waveform_helpers.R"))

url = "https://physionet.org/static/published-projects/wrist/wrist-ppg-during-exercise-1.0.0.zip"

options(timeout=300)
downloader::download(url, dest="ppg.zip", mode="wb")
unzip("ppg.zip", exdir = here::here("raw/"))

sub_files = list.files(here::here("raw/wrist-ppg-during-exercise-1.0.0"), full.names = TRUE)
sub_files = sub_files[grepl("hea", sub_files)] # get header files

ppg_dat = map(
  .x = sub_files,
  .f = function(file) {
    colnames = wfdbdesc(file)$descriptions
    rdsamp(file) %>%
      as_tibble() %>%
      magrittr::set_names(colnames) %>%
      mutate(id_long = sub(".*.0\\/(.+).hea.*", "\\1", file))
      }
  ) %>%
  bind_rows()

start_time = ymd_hms("2000-01-01 00:00:00", tz = "UTC")

ppg_dat_proc_low_noise <-
  ppg_dat %>%
  select(X = wrist_low_noise_accelerometer_x,
         Y = wrist_low_noise_accelerometer_y,
         Z = wrist_low_noise_accelerometer_z,
         tm_sec = sample_times_for_all_signals_apart_from_ecg,
         id_long) %>%
  mutate(across(c("X", "Y", "Z"), ~ .x *  0.10197162129779)) %>% # convert from m/s^2 to g
  mutate(cat_activity = sub(".*?\\_", "",  id_long),
         id_subject = sub("?\\_.*", "",  id_long)) %>%
  group_by(id_subject, cat_activity) %>%
  mutate(tm_dttm = start_time + as.period(tm_sec)) %>%
  ungroup() %>%
  mutate(id_study = "ppg_low_noise") %>%
  select(X, Y, Z,
         id_subject,
         cat_activity,
         tm_dttm,
         id_study)


write_csv(ppg_dat_proc_low_noise, here::here("processed/ppg_low_noise.csv.gz"))

ppg_dat_proc_wide_range <-
  ppg_dat %>%
  select(X = wrist_wide_range_accelerometer_x,
         Y = wrist_wide_range_accelerometer_y,
         Z = wrist_wide_range_accelerometer_z,
         tm_sec = sample_times_for_all_signals_apart_from_ecg,
         id_long) %>%
  mutate(across(c("X", "Y", "Z"), ~ .x *  0.10197162129779)) %>% # convert from m/s^2 to g
  mutate(cat_activity = sub(".*?\\_", "",  id_long),
         id_subject = sub("?\\_.*", "",  id_long)) %>%
  group_by(id_subject, cat_activity) %>%
  mutate(tm_dttm = start_time + as.period(tm_sec)) %>%
  ungroup() %>%
  mutate(id_study = "ppg_wide_range") %>%
  select(X, Y, Z,
         id_subject,
         cat_activity,
         tm_dttm,
         id_study)

write_csv(ppg_dat_proc_wide_range, here::here("processed/ppg_wide_range.csv.gz"))


system("rm ppg.zip")


