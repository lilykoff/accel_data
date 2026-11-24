library(curl)
library(dplyr)
library(janitor)
library(digest)
library(vroom)
options(digits.secs = 2)
# first download data after signing release agreement
# http://com.ytzhang.net/zju-gaitacc/agreement.php

# more documentation here https://www.ytzhang.net/datasets/zju-gaitacc/

data_dir  = "raw/zju_data/zju-gaitacc"
out_dir = "/dcs05/ciprian/smart/accel_data/processed"
read_text = function(x) {
  id = sub(".*subj_(.*)/rec.*", "\\1", x)
  session = sub(".*session_(.*)/sub.*", "\\1", x)
  record = sub(".*rec_(.*)/\\d.*", "\\1", x)
  body_location = sub("[.]txt.*", "", basename(x))
  xx = readLines(x)
  xx = stringr::str_split(xx, ",")
  xx = lapply(xx, as.numeric)
  xx = do.call(cbind, xx)
  xx = as.data.frame(xx)
  colnames(xx) = c("X", "Y", "Z")
  xx = xx %>%
    mutate(
      id = id,
      session = session,
      body_loc = body_location,
      record = record
    )
  xx
}
## function to read in text files
read_text_old <- function(x) {
  ID = sub("/rec.*", "", sub(".*subj_", "", x))
  session = sub("/subj.*", "", sub(".*gaitacc/", "", x))
  record = sub(".txt.*", "", sub(".*rec_", "", x))
  xx = read.table(x, sep = ",") %>%
    t() %>%
    as.data.frame() %>%
    mutate(
      ID = ID,
      session = session,
      record = record
    )
  rownames(xx) = NULL
  xx
}

unique_it = function(x) {
  ux = unique(x)
  stopifnot(length(ux) == 1)
  ux
}

# function to read in useful file annotations
read_useful = function(x) {
  id = sub(".*subj_(.*)/rec.*", "\\1", x)
  session = sub(".*session_(.*)/sub.*", "\\1", x)
  record = sub(".*rec_(.*)/useful.*", "\\1", x)
  xx = readLines(x)
  xx = stringr::str_split(xx, ",")[[1]]
  xx = tibble::tibble(
    start = xx[1],
    stop = xx[2],
    id = id,
    session = session,
    record = record
  )
  xx
}
read_cycle = function(x){
  id = sub(".*subj_(.*)/rec.*", "\\1", x)
  session = sub(".*session_(.*)/sub.*", "\\1", x)
  record = sub(".*rec_(.*)/cycles.*", "\\1", x)
  body_location = sub("[.]txt.*", "", basename(x))
  xx = readLines(x)
  xx = stringr::str_split(xx, ",")[[1]]
  xx = tibble::tibble(cycle_border = xx,
                      id = id,
                      session = session,
                      record = record)
  xx
}

files <-
  c(
    list.files(
      here::here(data_dir, "session_0"),
      recursive = TRUE,
      full.names = TRUE
    ),
    list.files(
      here::here(data_dir, "session_1"),
      recursive = TRUE,
      full.names = TRUE
    ),
    list.files(
      here::here(data_dir, "session_2"),
      recursive = TRUE,
      full.names = TRUE
    )
  )

# get useful annotations
useful_vector <- grep("useful", files)
useful <- files[useful_vector]

cycle = files[grep("cycles", files)]

# get rid of non accelerometry files from list
other_vector <-
  c(grep("useful", files),
    grep("total", files),
    grep("cycles", files))


files <- files[-other_vector]

## read in all files to data frame
df <- pbapply::pblapply(files, function(x) {
  # if (grepl("session_1/subj_039/rec_5/5.txt", x)) {
  #   return(NULL)
  # }
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

# filter to just right wrist

df = df %>%
  filter(body_location == "rwrist")

df = df %>% select(-body_location)


# get the nmber of records for each id/session/recording
# old - don't need to do this since we only use right wrist
lengths = df %>%
  dplyr::count(id, session, record)

# ensure that each body location has the same length
# we will use these in useful
lengths = lengths %>%
  dplyr::group_by(id, session, record) %>%
  dplyr::summarise(n = unique_it(n)) %>%
  dplyr::ungroup()


useful_key <- useful %>% purrr::map_dfr(read_useful)

cycles_key <- cycle %>% purrr::map_dfr(read_cycle)

cycles_key <- cycles_key %>%
  dplyr::mutate(
    session = as.integer(session),
    # first session is a distinct set of people
    id = ifelse(session > 0, sprintf("%03.0f", as.numeric(id) + 22),
                id),
  )

cycles_key = cycles_key %>%
  mutate(cycle_border = as.numeric(cycle_border))

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
# here
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

cycle_ind <- df %>%
  dplyr::right_join(cycles_key,
                   by = c("session" = "session",
                          "record" = "record",
                          "id" = "id",
                          "row_index" = "cycle_border")) %>%
  mutate(cycle_start = TRUE)


df_cycles <- df %>%
  dplyr::left_join(cycle_ind)

df_cycles <- df_cycles %>%
  mutate(ind_cycle = ifelse(cycle_start == TRUE & !is.na(cycle_start), 1, 0))


df_proc = df_cycles %>%  # 100 Hz data
  dplyr::mutate(time_seconds = (row_index ) / 100) %>%
  dplyr::select(-row_index)
start_time = lubridate::floor_date(as.POSIXct("2023-10-23 10:00:00", tz = "UTC"), unit = "seconds")


df_proc = df_proc %>%
  mutate(tm_dttm = start_time + lubridate::as.period(time_seconds),
         id_subject = id,
         cat_activity = ifelse(useful == TRUE, "walk", "pre_post_walk"),
         id_study = "zju",
         id_session = paste0("s_", session),
         id_recording = paste0("rec_", record)) %>%
  select(X, Y, Z, tm_dttm, starts_with("id"), cat_activity, ind_cycle, id_recording = record) %>%
  select(-id)

write_csv(df_proc, here::here(out_dir, "zju.csv.gz"))
