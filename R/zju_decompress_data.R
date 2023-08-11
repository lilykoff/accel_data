library(dplyr)
library(tidyr)
library(vroom)
library(janitor)
# first download data after signing release agreement
# http://com.ytzhang.net/zju-gaitacc/agreement.php

# more documentation here https://www.ytzhang.net/datasets/zju-gaitacc/

data_dir = "data"
tarball_file = file.path(
  "data",
  "zju-gaitacc_data.tar.gz"
)

# get the file list (without unzipping)
file_list = untar(tarball_file, list = TRUE)
file_list = tibble::tibble(
  name = file_list
  )
file_list = file_list %>%
  dplyr::mutate(file = file.path(data_dir, name),
                fname = basename(name),
                gzip_file = paste0(file, ".gz"))
# remove directories
file_list = file_list %>%
  filter(grepl("[.]", fname))

# we will gzip all of these files later for storage
if (!all(file.exists(file_list$gzip_file))) {
  # unzip the file if needed
  if (!all(file.exists(file_list$file))) {
    untar(tarball_file, exdir = data_dir)
  }
}

