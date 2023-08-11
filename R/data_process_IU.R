library(here)
library(purrr)
library(tidyverse)
system(
  "wget -r -N -c -np https://physionet.org/files/accelerometry-walk-climb-drive/1.0.0/"
)

# get names of files
files <-
  list.files(
    here(
      "physionet.org/files/accelerometry-walk-climb-drive/1.0.0/raw_accelerometry_data"
    ),
    full.names = T
  )


# function to read files and create ID column
read_files <- function(x) {
  readr::read_csv(x, show_col_types = F) %>% mutate(ID = sub(".csv.*", "", sub(".*id", "", x)))
}

# read in all files and row bind into df
df_all <-
  files %>%
  map_dfr(read_files) %>%
  dplyr::select(-`<html>`) 

### note this data has more than just walking - can filter by
write.csv(df_all, "df_all_IU.csv")

df_all_walking <-
  df_all %>%
  filter(
    activity == 1
  )

write.csv(df_all_walking, "df_walking_IU.csv")

rm(list = ls())
