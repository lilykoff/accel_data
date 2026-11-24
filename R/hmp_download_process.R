library(readr)
library(purrr)
library(tidyverse)


# download raw data
url = "https://github.com/wchill/HMP_Dataset/archive/refs/heads/master.zip"
downloader::download(url, dest="hmp.zip", mode="wb")
unzip("hmp.zip", exdir = here::here("raw/"))

options(digits.secs = 3)
# process
sub_files = list.files(here::here("raw/HMP_Dataset-master"), full.names = TRUE, recursive = TRUE)
sub_files = sub_files[grepl("Accelerometer", sub_files)]


hmp_dat = map(
  .x = sub_files,
  .f = function(file) {
    read_table(file,
               col_names = FALSE,
               col_types = cols(X1 = col_double(), X2 = col_double(), X3 = col_double())) %>%
      mutate(
        cat_activity = sub(".*master\\/(.+)\\/Accelerometer.*", "\\1", file),
        id_subject = sub(".*\\-(.+).txt.*", "\\1", file),
        tm_time = paste0(strsplit(
          sub(".*Accelerometer\\-", "", file), "-"
        )[[1]][4:6], collapse = ":"),
        tm_date = paste0(strsplit(
          sub(".*Accelerometer\\-", "", file), "-"
        )[[1]][1:3], collapse = "-"),
        tm_dttm = paste(tm_date, tm_time)
      )
  }
) %>%
  bind_rows()


hmp_dat_proc =
  hmp_dat %>%
  group_by(id_subject, cat_activity) %>%
  mutate(tm_sec = (row_number()-1)/32,
         id_repetition = paste0("start", as.character(tm_dttm)),
         tm_dttm = as.POSIXct(tm_dttm, tz = "UTC") + as.period(tm_sec),
         id_study = "hmp") %>%
  ungroup() %>%
  mutate(across(X1:X3, ~ -1.5 + (.x/63)*3)) %>%
  select(X = X1,
         Y = X2,
         Z = X3,
         id_subject,
         cat_activity,
         tm_dttm,
         id_study,
         id_repetition)

hmp_dat_proc <- hmp_dat_proc %>% mutate(cat_activity = tolower(cat_activity))

write_csv(hmp_dat_proc, here::here("processed/hmp.csv.gz"))

system("rm hmp.zip")
