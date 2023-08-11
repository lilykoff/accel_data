library(purrr)
library(tidyverse)
library(here)
source("R/zju_helpers.R")
data_dir = "data"
data_dir = file.path("data", "zju-gaitacc")
csv_file = paste0(data_dir, ".csv")
rds_file = paste0(data_dir, ".rds")
gz_file = paste0(csv_file, ".gz")

# first download data after signing release agreement
# http://com.ytzhang.net/zju-gaitacc/agreement.php

# more documentation here https://www.ytzhang.net/datasets/zju-gaitacc/
outfiles = c(gz_file, rds_file)
if (!all(file.exists(outfiles))) {
  # get all files
  files <-
    c(
      list.files(
        here(data_dir, "session_0"),
        recursive = TRUE,
        full.names = TRUE
      ),
      list.files(
        here(data_dir, "session_1"),
        recursive = TRUE,
        full.names = TRUE
      ),
      list.files(
        here(data_dir, "session_2"),
        recursive = TRUE,
        full.names = TRUE
      )
    )

  # get useful annotations
  useful_vector <- grep("useful", files)
  useful <- files[useful_vector]

  # get rid of non accelerometry files from list
  other_vector <-
    c(grep("useful", files),
      grep("total", files),
      grep("cycles", files))


  files <- files[-other_vector]

  ## read in all files to data frame
  df <- pbapply::pblapply(files, function(x) {
    if (grepl("session_1/subj_039/rec_5/5.txt", x)) {
      return(NULL)
    }
    read_text(x)
  })
  df <- dplyr:::bind_rows(df)

  # get body locations and match IDs since session 0 individuals
  # different from others
  df <-
    df %>%
    dplyr::mutate(
      session = as.integer(session),
      # first session is a distinct set of people
      id = ifelse(session > 0, sprintf("%03.0f", as.numeric(id) + 22),
                  id)
    )

  df = df %>%
    dplyr::group_by(id, session, record, body_loc) %>%
    dplyr::mutate(
      row_index = 1:dplyr::n()
    ) %>%
    dplyr::ungroup()

  loc_df = tibble::tribble(
    ~body_loc, ~body_location,
    "1", "rwrist",
    "2", "larm",
    "3", "rpelvis",
    "4", "lthigh",
    "5", "rankle"
  )
  df = dplyr::left_join(df, loc_df) %>%
    dplyr::select(-body_loc)

  # get the nmber of records for each id/session/recording
  lengths = df %>%
    dplyr::count(id, session, record, body_location)

  # ensure that each body location has the same length
  # we will use these in useful
  lengths = lengths %>%
    dplyr::group_by(id, session, record) %>%
    dplyr::summarise(
      n = unique_it(n)
    ) %>%
    dplyr::ungroup()


  useful_key <- useful %>% purrr::map_dfr(read_useful)
  # now we have df with start and end of useful segments for
  # each person, session, recording

  # get IDS to match with the data
  useful_key <- useful_key %>%
    dplyr::mutate(
      session = as.integer(session),
      # first session is a distinct set of people
      id = ifelse(session > 0, sprintf("%03.0f", as.numeric(id) + 22),
                  id)
    )

  # get the length of each session recording
  useful_key = dplyr::left_join(useful_key, lengths)

  make_useful = function(start, stop, n) {
    x = rep(FALSE, length = n)
    ind = start:stop
    x[ind] = TRUE
    list(tibble::tibble(
      row_index = 1:n,
      useful = x
    ))
  }

  useful_df = useful_key %>%
    dplyr::group_by(id, session, record) %>%
    dplyr::summarise(
      data = make_useful(start, stop, n)
    ) %>%
    dplyr::ungroup()

  useful_df = tidyr::unnest(useful_df, data)

  # left join with original data
  df <- df %>%
    dplyr::left_join(useful_df)
  df = df %>%  # 100 Hz data
    dplyr::mutate(time_seconds = (row_index - 1) / 100) %>%
    dplyr::select(-row_index)

  if (!file.exists(gz_file)) {
    readr::write_csv(df, csv_file)
    R.utils::gzip(csv_file,
                  remove = TRUE,
                  overwrite = TRUE,
                  compression = 9)
  }
  readr::write_rds(df, file = rds_file,
                   compress = "gz",
                   compression = 9)


} else {
  df = readRDS(rds_file)
}

df = df %>%
  dplyr::mutate(
    vm = sqrt(X^2 + Y^2 + Z^2))

wide <- df %>%
  dplyr::select(-X, -Y, -Z) %>%
  tidyr::pivot_wider(names_from = body_location, values_from = vm)
