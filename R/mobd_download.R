## mobilize D
# https://www.nature.com/articles/s41597-023-01930-9#Sec1
# https://zenodo.org/records/15861907?preview_file=CHF.zip


required_packages = c("here", "purrr", "dplyr", "R.matlab", "downloader", "glue")

install_and_load = function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}

install_and_load(required_packages)

# library(here)
# library(purrr)
# library(dplyr)
# library(R.matlab)
source(here::here("R", "mobd_utils.R"))
force = FALSE

options(timeout=10000)
options(digits.secs = 3)
dir.create(here::here("data/raw"), showWarnings = FALSE, recursive = TRUE)
dir.create(here::here("data", "raw", "mob_d"))
dir.create(here::here("data", "processed", "mob_d"), recursive = TRUE)


get_fold = function() {
  ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
  if (is.na(ifold)) {
    ifold = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  }
  print(paste0("fold is: ", ifold))
  ifold
}
ifold = get_fold()

conditions = c("CHF", "COPD", "HA", "MS", "PD", "PFF")
condition = conditions[ifold]

out_fl = here::here("data", "processed", "mob_d", paste0(condition, "_fl.csv.gz"))
out_lab = here::here("data", "processed", "mob_d", paste0(condition, "_lab.csv.gz"))

if (!all(file.exists(out_fl, out_lab)) || force) {
  destination = here::here("data", "raw", paste0(condition, ".zip"))
  if (!file.exists(destination)) {
    url = glue::glue("https://zenodo.org/records/15861907/files/{condition}.zip")
    # url = "https://zenodo.org/records/15861907/files/CHF.zip"

    downloader::download(url, dest = destination, mode="wb")
    unzip(destination, exdir = here::here("data/raw/mob_d/"))
  }

  mobd_files = list.files(here::here(glue::glue("data/raw/mob_d/{condition}")),
                          pattern = "^data\\.mat$",
                          recursive = TRUE, full.names = TRUE)

  lab_files = mobd_files[grepl("laboratory", mobd_files, ignore.case = TRUE)]
  fl_files = mobd_files[grepl("free-?living", mobd_files, ignore.case = TRUE)]

  # try one subject from each first
  # test_lab = get_mobd(lab_files[1], "laboratory")
  # test_lab %>% distinct(id_test, id_trial, cat_activity, cat_step_source)
  # test_lab %>% count(cat_activity, cat_step_source, ind_step)
  test_fl = get_mobd(fl_files[1], "free_living")
  # test_fl %>% count(cat_step_source, ind_step)
  #
  mobd_lab = map(lab_files, get_mobd, cat_setting = "laboratory") |>
    bind_rows()

  write_csv(mobd_lab, out_lab)

  mobd_fl = map(fl_files, get_mobd, cat_setting = "free_living") |>
    bind_rows()

  write_csv(mobd_fl, out_fl)

}
