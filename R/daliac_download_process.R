library(here)
library(purrr)
library(tidyverse)

url = "https://faubox.rrze.uni-erlangen.de/dl/fiDdYhu9r5DDm1NbchkwS4/daliac.zip"
options(timeout=300)
options(digits.secs = 3)
downloader::download(url, dest="dailac.zip", mode="wb")
unzip("dailac.zip", exdir = here::here("raw/dailac"))


sub_files <- list.files(here::here("raw/dailac"), recursive = TRUE, full.names = TRUE)
acc_files <- sub_files[grep("dataset", sub_files)]


daliac_dat = map(
  .x = acc_files,
  .f = function(file) {
    acc = readr::read_csv(file,
                   col_names = FALSE) %>%
      select(X = X1,
             Y = X2,
             Z = X3,
             L = X25)  %>%
      mutate(
        id_subject = paste0("s", sub(".*\\_(.+).txt.*", "\\1", file)),
        cat_activity = case_when(
          L == 1 ~ "sit",
          L == 2 ~ "lie_down",
          L == 3 ~ "stand",
          L == 4 ~ "wash_dishes",
          L == 5 ~ "vacuum",
          L == 6 ~ "sweep",
          L == 7 ~ "walk",
          L == 8 ~ "climb_stairs",
          L == 9 ~ "down_stairs",
          L == 10 ~ "run",
          L == 11 ~ "cycle_50w",
          L == 12 ~ "cycle_100w",
          L == 13 ~ "jump_rope"
        )
      )
    start_time = lubridate::ymd_hms("2000-01-01 00:00:00", tz = "UTC")
    sample_rate = 204.8

    acc %>%
      group_by(cat_activity) %>%
      mutate(
        tm_sec = (row_number() - 1) / sample_rate,
        tm_dttm = as.period(tm_sec) + start_time,
        id_study = "daliac"
      ) %>%
      select(
        X, Y, Z,
        id_subject,
        tm_dttm,
        cat_activity,
        id_study
      ) %>%
      ungroup()
  }
) %>%
  bind_rows()

write_csv(daliac_dat, here::here("processed/daliac.csv.gz"))


system("rm dailac.zip")

