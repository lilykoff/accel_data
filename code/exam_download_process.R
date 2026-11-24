library(readr)
library(purrr)
library(tidyverse)
options(digits.secs = 3)
options(timeout=300)
# download raw data
url = "https://physionet.org/static/published-projects/wearable-exam-stress/a-wearable-exam-stress-dataset-for-predicting-cognitive-performance-in-real-world-settings-1.0.0.zip"
downloader::download(url, dest="exam.zip", mode="wb")
unzip("exam.zip", exdir = here::here("raw/"))


# process
unzip(here::here("raw/a-wearable-exam-stress-dataset-for-predicting-cognitive-performance-in-real-world-settings-1.0.0/Data.zip"),
      exdir = here::here("raw/a-wearable-exam-stress-dataset-for-predicting-cognitive-performance-in-real-world-settings-1.0.0/"))

sub_files = list.files(here::here("raw/a-wearable-exam-stress-dataset-for-predicting-cognitive-performance-in-real-world-settings-1.0.0/Data"),
                       recursive = TRUE,
                       full.names = TRUE)
acc_files = sub_files[grepl("ACC.csv", sub_files)]



exam_dat = map(
  .x = acc_files,
  .f = function(file) {
    acc = read_csv(file,
                   col_names = FALSE) %>%
      mutate(
        id_long = sub(".*.Data\\/(.+)\\/ACC.*", "\\1", file),
        id_exam = sub(".*\\/", "", id_long),
        id_subject = tolower(sub("\\/.*", "", id_long)),
        cat_activity = paste0("take_", tolower(gsub(" ", "", id_exam)))
      )
    start_time = acc %>% slice(1) %>% pull(X1) # get start time (unix utc) from first row
    start_time = lubridate::as_datetime(start_time, tz = "UTC") # convert to datetime
    sample_rate = acc %>% slice(2) %>% pull(X1) # get sample rate from second row
    acc <- acc %>% slice(-c(1:2)) # remove first two rows
    acc %>%
      mutate(
        tm_sec = (row_number() - 1) / sample_rate,
        tm_dttm = as.period(tm_sec) + start_time,
        id_study = "exam"
      ) %>%
      mutate(across(X1:X3, ~ .x / 64)) %>%
      select(
        X = X1,
        Y = X2,
        Z = X3,
        id_subject,
        tm_dttm,
        cat_activity,
        id_study
      )
  }
) %>%
  bind_rows()


write_csv(exam_dat, here::here("processed/exam.csv.gz"))
system("rm exam.zip")

