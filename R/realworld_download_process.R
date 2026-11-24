library(readr)
library(purrr)
library(tidyverse)

options(timeout=1000)
options(digits.secs = 2)
# download raw data
url = "http://wifo5-14.informatik.uni-mannheim.de/sensor/dataset/realworld2016/realworld2016_dataset.zip"
downloader::download(url, dest="realworld.zip", mode="wb")
unzip("realworld.zip", exdir = here::here("raw/realworld2016_dataset"))



sub_files = list.files(here::here("raw/realworld2016_dataset"),
                       recursive = TRUE,
                       full.names = TRUE)
sub_files = sub_files[grepl("acc\\_.*csv", sub_files)]



realworld_dat = map(
  .x = sub_files,
  .f = function(file) {
    activity = sub(".*acc\\_(.+)\\_csv.*", "\\1", file) # get activity type
    filename = paste0("acc_", activity, "_forearm.csv") # get just forearm data
    subject = sub(".*dataset\\/(.+)\\/data.*", "\\1", file) # get subject
    data <- read_csv(unzip(file, filename))
    if(nrow(data) == 0){
      return(NULL)
    }
    data %>%
      mutate(
        id_subject = subject,
        tm_numeric = attr_time * 0.001, # time is in 0.001s per readme
        tm_dttm = lubridate::as_datetime(tm_numeric, tz = "UTC"),
        cat_activity = activity,
        id_study = "realworld"
      ) %>%
      select(X = attr_x, Y = attr_y, Z = attr_z,
             id_subject,
             id_study,
             tm_dttm,
             cat_activity) %>%
      mutate(across(X:Z, ~.x * 0.101971621)) # change to g instead of m/s^2
  }
) %>%
  bind_rows()

# manually deal with annoying files
sf = list.files(here::here("raw/realworld2016_dataset"),
           recursive = TRUE,
           full.names = TRUE)
sf = sf[grepl("acc\\_.*csv", sf)]
proband13_walk <- sf[grep("proband13/data/acc_walking_", sf)]

activity = sub(".*acc\\_(.+)\\_csv.*", "\\1", proband13_walk) # get activity type
filename = paste0("acc_", activity, "_2_forearm.csv") # get just forearm data
subject = sub(".*dataset\\/(.+)\\/data.*", "\\1", proband13_walk) # get subject

data <- read_csv(unzip(proband13_walk, filename)) %>%
  mutate(
    id_subject = subject,
    tm_numeric = attr_time * 0.001, # time is in 0.001s per readme
    tm_dttm = lubridate::as_datetime(tm_numeric, tz = "UTC"),
    cat_activity = activity,
    id_study = "realworld"
  ) %>%
  select(X = attr_x, Y = attr_y, Z = attr_z,
         id_subject,
         id_study,
         tm_dttm,
         cat_activity) %>%
  mutate(across(X:Z, ~.x * 0.101971621))




realworld_dat_proc <-
  realworld_dat %>%
  bind_rows(data) %>%
  mutate(
    cat_activity = case_when( # rename activities to match other data
      cat_activity == "climbingdown" ~ "down_stairs",
      cat_activity == "climbingup" ~ "up_stairs",
      cat_activity == "lying" ~ "lie_down",
      cat_activity == "running" ~ "run",
      cat_activity == "sitting" ~ "sit",
      cat_activity == "standing" ~ "stand",
      cat_activity == "walking" ~ "walk",
      cat_activity == "jumping" ~ "jump_up_down"
    )
  )



write_csv(realworld_dat_proc, here::here("processed/realworld.csv.gz"))
system("rm realworld.zip")

