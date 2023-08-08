library(dplyr)
library(janitor)
zip_file = paste0(
  "labeled-raw-accelerometry-data-",
  "captured-during-walking-stair-climbing-",
  "and-driving-1.0.0.zip"
)
data_dir = "data"
file = file.path(data_dir, basename(zip_file))

# get the file list (without unzipping)
file_list = unzip(file, list = TRUE) %>%
  janitor::clean_names() %>%
  dplyr::as_tibble()
file_list = file_list %>%
  dplyr::mutate(file = file.path(data_dir, name),
                fname = basename(name),
                gzip_file = paste0(file, ".gz"))
# see what files need to be compressed (we're compressing them all)
file_list = file_list %>%
  dplyr::mutate(need_zip = file.exists(file) & !file.exists(gzip_file))
file_list = file_list %>%
  dplyr::filter(need_zip)
if (nrow(file_list) > 0) {
  for (ifile in file_list$file) {
    R.utils::gzip(ifile,
                  remove = TRUE,
                  compression = 9)
  }
}

