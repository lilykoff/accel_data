library(dplyr)
library(tidyr)
library(vroom)
library(janitor)
data_dir = file.path(
  "data",
  paste0(
    "labeled-raw-accelerometry-data-",
    "captured-during-walking-stair-climbing-",
    "and-driving-1.0.0"),
  "raw_accelerometry_data"
)

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
