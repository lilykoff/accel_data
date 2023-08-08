library(curl)
library(dplyr)
library(janitor)
library(digest)
base_url = "https://physionet.org/static/published-projects/"
url = paste0(
  base_url,
  "accelerometry-walk-climb-drive/",
  "labeled-raw-accelerometry-data-captured-during-walking-stair-climbing-and-driving-1.0.0.zip"
)
data_dir = "data"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
file = file.path(data_dir, basename(url))
if (!file.exists(file)) {
  curl::curl_download(url, destfile = file, quiet = FALSE)
}
