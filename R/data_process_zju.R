library(purrr)
library(tidyverse)
library(here)
data_dir = "data"

# first download data after signing release agreement
# http://com.ytzhang.net/zju-gaitacc/agreement.php

# more documentation here https://www.ytzhang.net/datasets/zju-gaitacc/


## function to read in text files
read_text <- function(x) {
  xx = read.table(x, sep = ",") %>%
    t() %>%
    as.data.frame() %>%
    mutate(
      ID = sub("/rec.*", "", sub(".*subj_", "", x)),
      session = sub("/subj.*", "", sub(".*gaitacc/", "", x)),
      record = sub(".txt.*", "", sub(".*rec_", "", x))
    )
  rownames(xx) = NULL
  xx
}

# get all files
files <-
  c(
    list.files(
      here(data_dir, "zju-gaitacc/session_0"),
      recursive = TRUE,
      full.names = TRUE
    ),
    list.files(
      here(data_dir, "zju-gaitacc/session_1"),
      recursive = TRUE,
      full.names = TRUE
    ),
    list.files(
      here(data_dir, "zju-gaitacc/session_2"),
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


files <- files[-vector]

## read in all files to data frame
all_data <- files %>% map_dfr(read_text)

# get body locations and match IDs since session 0 individuals different from others
# t is the row number
all_data <-
  all_data %>%
  mutate(
    body_loc = sub(".*/", "", record),
    rec = sub("/.*", "", record),
    ID = ifelse(session == "session_0", as.numeric(ID), as.numeric(ID) + 22)) %>%
  group_by(ID, session, rec, body_loc) %>%
  mutate(t = row_number()) %>%
  ungroup()


# function to read in useful file annotations
read_useful <- function(x) {
  read.table(x, sep = ",") %>%
    rename("start" = V1, "end" = V2) %>%
    mutate(
      session = sub("/subj.*", "", sub(".*gaitacc/", "", x)),
      ID = sub("/rec.*", "", sub(".*subj_", "", x)),
      record = sub("/useful.*", "", sub(".*rec_", "", x))
    )
}

useful_key <- useful %>% map_dfr(read_useful)
# now we have df with start and end of useful segments for each person, session, recording

# get IDS to match
useful_key <-
  useful_key %>%
  mutate(ID = ifelse(session == "session_0", as.numeric(ID), as.numeric(ID) + 22))


# left join with original data
joined <-
  all_data%>%
  left_join(useful_key,
            by = c(
              "session" = "session",
              "ID" = "ID",
              "rec" = "record"
            ))

# filter to just useful segment
filtered <-
  joined %>% filter(t >= start &
                      t <= end) %>%
  dplyr::select(-c(start, end))

rm(joined); rm(all_data)
# change body location name
filtered <-
  filtered %>%
  mutate(
    loc = case_when(
      body_loc == "1" ~ "rwrist",
      body_loc == "2"~ "larm",
      body_loc=="3"~"rpelvis",
      body_loc=="4" ~ "lthigh",
      body_loc=="5" ~ "rankle"
    ),
    signal = sqrt(V1^2 + V2^2 + V3^2)) %>%
  dplyr::select(-c(body_loc, t)) %>%
  group_by(ID, session, loc) %>%
  mutate(
    s_allrec = row_number(),
    time_allrec = floor(s_allrec/100)+1) %>%
  ungroup() %>%
  mutate(second = ceiling(s_allrec/100)) %>%
  rename(
    time = s_allrec
  ) %>%
  dplyr::select(ID, session, rec, loc, V1, V2, V3, signal, second, time)


filtered_wide <-
  filtered %>%
  pivot_wider(names_from = loc, values_from = c(signal, V1, V2, V3))

write.csv(filtered_wide, "df_all_zju.csv")

rm(list=ls())
