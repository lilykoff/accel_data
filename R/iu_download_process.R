library(curl)
library(dplyr)
library(janitor)
library(digest)
# download data
base_url = "https://physionet.org/static/published-projects/"
url = paste0(
  base_url,
  "accelerometry-walk-climb-drive/",
  "labeled-raw-accelerometry-data-captured-during-walking-stair-climbing-and-driving-1.0.0.zip"
)
data_dir = "data/raw/IU_data"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
file = file.path(data_dir, basename(url))
if (!file.exists(file)) {
  curl::curl_download(url, destfile = file, quiet = FALSE)
}
fname = paste0(
  "labeled-raw-accelerometry-data-",
  "captured-during-walking-stair-climbing-",
  "and-driving-1.0.0.zip")
data_dir = "data/raw/IU_data"
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

# compress files
zip_file = paste0(
  "labeled-raw-accelerometry-data-",
  "captured-during-walking-stair-climbing-",
  "and-driving-1.0.0.zip"
)
data_dir = "data/raw/IU_data"
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

data_dir = file.path(
  "data/raw/IU_data",
  paste0(
    "labeled-raw-accelerometry-data-",
    "captured-during-walking-stair-climbing-",
    "and-driving-1.0.0"),
  "raw_accelerometry_data"
)

# read the data
csv_file = paste0(data_dir, ".csv")
rds_file = paste0(data_dir, ".rds")
gz_file = paste0(csv_file, ".gz")
outfiles = c(gz_file, rds_file)
if (!all(file.exists(outfiles))) {

  files = list.files(data_dir, pattern = ".csv.gz",
                     full.names = TRUE)

  df = vroom::vroom(files, id = "file")
  df = df %>%
    dplyr::mutate(id = sub("[.]csv.*", "", basename(file))) %>%
    dplyr::select(-file)


  # Get Data Dictionary
  dictionary_file = paste0(data_dir, "_dict.csv.gz")
  dict = vroom::vroom(dictionary_file) %>%
    janitor::clean_names()
  # label each column by the plain name
  for (icol in colnames(df)) {
    label = dict %>%
      filter(variable_name == icol) %>%
      pull(definition)
    if (length(label) > 0) {
      attr(df[[icol]], "label") = label
    }
  }

  # Get the values for activity so it's labeled
  activity_dict = dict %>%
    filter(variable_name == "activity") %>%
    pull(values)
  activity_dict = strsplit(activity_dict, split = ";") %>%
    sapply(trimws) %>%
    unlist() %>%
    c()
  # replacing the label with the variable activity
  activity_dict = tibble::tibble(
    mapping = activity_dict
  ) %>%
    tidyr::separate(mapping,
                    into = c("activity", "activity_label"),
                    sep = "=")
  df = df %>%
    dplyr::mutate(activity = as.character(activity)) %>%
    dplyr::left_join(activity_dict)

  df = df %>%
    dplyr::select(-dplyr::any_of("activity")) %>%
    dplyr::select(activity = activity_label, dplyr::everything())

  readr::write_rds(df, file = rds_file,
                   compress = "gz",
                   compression = 9)


  if (!file.exists(gz_file)) {
    vroom::vroom_write(df,
                       file = csv_file, delim = ",")
    R.utils::gzip(csv_file,
                  remove = TRUE,
                  overwrite = TRUE,
                  compression = 9)
  }

}


file = readr::read_csv(here::here("data/raw/IU_data/labeled-raw-accelerometry-data-captured-during-walking-stair-climbing-and-driving-1.0.0/raw_accelerometry_data.csv"))
start_time = lubridate::floor_date(as.POSIXct("2023-10-23 10:00:00", tz = "UTC"), unit = "seconds")
options(digits.secs = 2)

file_proc <-
  file %>%
  mutate(tm_dttm = start_time + lubridate::as.period(time_s)) %>%
  ungroup() %>%
  mutate(id_study = "iu",
         cat_activity = case_when(
           activity == "non-study activity" ~ "non_study_activity",
           activity == "clapping" ~ "clap",
           activity == "walking" ~ "walk",
           activity == "descending stairs" ~ "down_stairs",
           activity == "ascending stairs" ~ "up_stairs",
           activity == "driving" ~ "drive"
         )) %>%
  select(X = lw_x,
         Y = lw_y,
         Z = lw_z,
         tm_dttm,
         cat_activity,
         id_subject = id,
         id_study)

dir.create(here::here("data/processed"), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(file_proc, here::here("data/processed/iu.csv.gz"))


