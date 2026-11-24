library(readr)
library(purrr)
library(tidyverse)

# download and unzip data https://archive.ics.uci.edu/dataset/455/simulated+falls+and+daily+living+activities+data+set

options(digits.secs = 2)
# process
sub_files = list.files(here::here("data/raw/simulated_falls_and_daily_living_activities_data_set/Tests"),
                       full.names = TRUE,
                       recursive = TRUE)
sub_files = sub_files[-grep("Bilgi", sub_files)]
sub_files_rw = sub_files[grepl("340537", sub_files)]

sub_files_rw = sub_files_rw[-grep("Fail", sub_files_rw)]



sfdla_dat = map(
  .x = sub_files_rw,
  .f = function(file) {
    subject = sub(".*Tests\\/(.+)\\/Testler.*", "\\1", file)
    activity = sub(".*Export\\/(.+)\\-.*", "\\1", file)
    test_num = sub(".*Test\\_(.+)\\/3.*", "\\1", file)

    dat <- read_table(file,
                      skip = 4)
    if (nrow(dat) == 0) {
      return(NULL)
    }
    else{
      sample_rate = readLines(file, n = 4)[2]
      sample_rate = as.numeric(stringr::str_trim(sub(
        ".*Update Rate\\:(.+)Hz.*", "\\1", sample_rate
      )))
      start_time = lubridate::floor_date(as.POSIXct("2023-10-23 10:00:00", tz = "UTC"), unit = "seconds")

      dat %>%
        mutate(
          id_subject = subject,
          cat_activity = activity,
          id_repetition = test_num,
          tm_sec = (row_number() - 1) / sample_rate,
          tm_dttm = lubridate::as.period(tm_sec) + start_time
        ) %>%
        select(
          X = Acc_X,
          Y = Acc_Y,
          Z = Acc_Z,
          tm_dttm,
          id_subject,
          cat_activity,
          id_repetition
        ) %>%
        mutate(across(X:Z, ~ .x * 0.101971621))
    }
  }
) %>%
  bind_rows()

falls = c("901", "902", "903", "904", "905", "906", "907", "908",
           "909", "910", "911", "912", "913", "914", "915", "916",
           "917", "918", "919", "920")
sfdla_proc =
  sfdla_dat %>%
  mutate(
    cat_activity_small =
      case_when(
        cat_activity %in% falls ~ "fall",
        cat_activity == "801" ~ "walk",
        cat_activity == "802" ~ "walk_backward",
        cat_activity == "803" ~ "run",
        cat_activity == "804" ~ "squat",
        cat_activity == "805" | cat_activity == "806" ~ "bend",
        cat_activity == "807" ~ "walk_limp",
        cat_activity == "808" | cat_activity == "809" ~ "stumble",
        cat_activity == "810" ~ "cough",
        cat_activity %in% c("811", "812", "813", "814") ~ "sit",
        cat_activity == "815" ~ "lie_down",
        cat_activity == "816" ~ "sit_up"),
    id_study = "sfdla",
    id_subject = as.character(id_subject),
    cat_activity = as.character(cat_activity),
    id_repetition = as.character(id_repetition)) %>%
  rename(cat_activity_orig = cat_activity,
         cat_activity = cat_activity_small)


dir.create(here::here("data/processed"), showWarnings = FALSE, recursive = TRUE)
write_csv(sfdla_proc, here::here("data/processed/sfdla.csv.gz"))

