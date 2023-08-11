library(curl)
library(dplyr)
library(janitor)
library(digest)
fname = paste0(
  "labeled-raw-accelerometry-data-",
  "captured-during-walking-stair-climbing-",
  "and-driving-1.0.0.zip")
data_dir = "data"
file = file.path(data_dir, fname)

# get the file list (without unzipping)
file_list = unzip(file, list = TRUE) %>%
  janitor::clean_names() %>%
  dplyr::as_tibble()
file_list = file_list %>%
  dplyr::mutate(file = file.path(data_dir, name),
                fname = basename(name),
                gzip_file = paste0(file, ".gz"))
# we will gzip all of these files later for storage
if (!all(file.exists(file_list$gzip_file))) {
  # unzip the file if needed
  if (!all(file.exists(file_list$file))) {
    unzip(file, exdir = data_dir)
  }
  # make sure no duplicates - using merging later
  stopifnot(anyDuplicated(file_list$fname) == 0)

  # get the SHA256 file to make sure files match up with what was uploaded
  sha_file = file_list %>%
    filter(basename(name) == "SHA256SUMS.txt")
  dname = dirname(sha_file$name)
  sha_file = sha_file %>%
    pull(file)
  shas = readr::read_delim(sha_file, delim = " ",
                           col_names = c("sha256", "fname_with_dir"))
  shas = shas %>%
    mutate(fname = basename(fname_with_dir))

  shas = shas %>% left_join(file_list)

  # check the SHA256 with it's uploaded counterpart
  check_sha256 = function(x, sha) {
    digest::digest(x, algo = "sha256", file = TRUE) == sha
  }
  result = mapply(check_sha256, shas$file, shas$sha256)
  stopifnot(all(result))
}

