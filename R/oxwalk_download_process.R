library(here)
library(purrr)
library(tidyverse)
library(readr)
options(digits.secs = 3)
# data download here https://ora.ox.ac.uk/objects/uuid:19d3cb34-e2b3-4177-91b6-1bad0e0163e7

url = "https://ora.ox.ac.uk/objects/uuid:19d3cb34-e2b3-4177-91b6-1bad0e0163e7/files/dcj82k7829"

options(timeout=300)
downloader::download(url, dest="oxwalk.zip", mode="wb")
unzip("oxwalk.zip", exdir = here::here("raw/oxwalk"))


wrist_files_100 = list.files(here::here("raw/oxwalk/OxWalk_Dec2022/Wrist_100Hz"), recursive = TRUE, full.names = TRUE)
wrist_files_25 = list.files(here::here("raw/oxwalk/OxWalk_Dec2022/Wrist_25Hz"), recursive = TRUE, full.names = TRUE)



get_and_join_files <-
  function(wrist_file){
    id = sub(".*Hz\\/(.+)\\_wrist.*", "\\1", wrist_file)
    sample_rate = as.numeric(sub(".*wrist(.+).csv.*", "\\1", wrist_file))
    temp =
      readr::read_csv(wrist_file) %>%
      select(tm_dttm = timestamp,  X = x, Y= y, Z =z, ind_step = annotation) %>%
      mutate(id_study = "oxwalk",
             id_subject = id,
             sample_rate = sample_rate)
  }

ox_data <- map(.x = c(wrist_files_100, wrist_files_25),
               .f = get_and_join_files) %>%
  bind_rows()


write_csv(ox_data, here::here("processed/ox_data.csv.gz"))

system("rm oxwalk.zip")

