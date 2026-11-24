library(tidyverse)
library(purrr)
source(here::here("R/resample_helpers.R"))
dir.create(here::here("data/processed/resampled"), showWarnings = FALSE, recursive = TRUE)

sample_rate_target = 15 # we want to resample all data to 15 Hz

# clemson data already at 15 hz

# iu data
options(digits.secs = 3)
iu = readr::read_csv(here::here("data/processed/iu.csv.gz"))

# get key df that is activity and subject labels for each second, which we will join back eventually
key = iu %>%
  mutate(second = floor_date(tm_dttm, unit = "seconds")) %>%
  group_by(id_subject, cat_activity, second) %>%
  summarize(n = n()) %>%
  filter(n == 100) %>%
  select(-n)

iu_list = split(iu, iu$id_subject)

iu_resampled =
  purrr::map(
    .x = iu_list,
    .f = function(x) {
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        rename(tm_dttm = HEADER_TIMESTAMP) %>%
        mutate(
          second = lubridate::floor_date(tm_dttm, unit = "seconds"),
          id_subject = x$id_subject[1]
        )

      left_join(resamp,
                key,
                by = c("id_subject" = "id_subject",
                       "second" = "second"))
    }
  ) %>%
  bind_rows() %>%
  select(-second) %>%
  mutate(id_study = "iu")

fname = paste0("data/processed/resampled/iu_", sample_rate_target, "hz.csv.gz")
readr::write_csv(iu_resampled, here::here(fname))

# zju
zju = readr::read_csv(here::here("data/processed/zju.csv.gz"))
key = zju %>%
  mutate(second = floor_date(tm_dttm, unit = "seconds")) %>%
  group_by(id_subject, cat_activity, id_recording, second, id_session) %>%
  summarize(n = n(),
            num_borders = sum(ind_cycle)) %>%
  filter(n == 100) %>%
  select(-n)

zju_list = split(zju, list(zju$id_subject,
                           zju$id_recording,
                           zju$id_session))
zju_list = zju_list %>% vctrs::list_drop_empty()
zju_resampled =
  purrr::map(
    .x = zju_list,
    .f = function(x) {
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        rename(tm_dttm = HEADER_TIMESTAMP) %>%
        mutate(
          second = lubridate::floor_date(tm_dttm, unit = "seconds"),
          id_subject = x$id_subject[1],
          id_recording = x$id_recording[1],
          id_session = x$id_session[1]
        )

      left_join(resamp,
                key,
                by = c("id_subject" = "id_subject",
                       "second" = "second",
                       "id_session" = "id_session",
                       "id_recording" = "id_recording"))
    }
  ) %>%
  bind_rows() %>%
  select(-second) %>%
  mutate(id_study = "zju")
fname = paste0("data/processed/resampled/zju_", sample_rate_target, "hz.csv.gz")

readr::write_csv(zju_resampled, here::here(fname))

exam = readr::read_csv(here::here("data/processed/exam.csv.gz"))


exam_list = split(exam, list(exam$id_subject, exam$cat_activity))

exam_resampled =
  purrr::map(
    .x = exam_list,
    .f = function(x) {
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        rename(tm_dttm = HEADER_TIMESTAMP) %>%
        mutate(
          second = lubridate::floor_date(tm_dttm, unit = "seconds"),
          id_subject = x$id_subject[1],
          cat_activity = x$cat_activity[1]
        )
    }
  ) %>%
  bind_rows() %>%
  select(-second) %>%
  mutate(id_study = "exam")

fname = paste0("data/processed/resampled/exam_", sample_rate_target, "hz.csv.gz")

readr::write_csv(exam_resampled, here::here(fname))


ppg = readr::read_csv(here::here("data/processed/ppg_low_noise.csv.gz"))

ppg = ppg %>% distinct()

ppg_list = split(ppg, list(ppg$id_subject,
                           ppg$cat_activity))
ppg_list = ppg_list %>% vctrs::list_drop_empty()

ppg_resampled =
  purrr::map(
    .x = ppg_list,
    .f = function(x) {
      x <- x %>% distinct() %>% arrange(tm_dttm)
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        rename(tm_dttm = HEADER_TIMESTAMP) %>%
        mutate(
          second = lubridate::floor_date(tm_dttm, unit = "seconds"),
          id_subject = x$id_subject[1],
          cat_activity = x$cat_activity[1]
        )
    }
  ) %>%
  bind_rows() %>%
  select(-second) %>%
  mutate(id_study = "ppg_low_noise")


fname = paste0("data/processed/resampled/ppg_low_noise_", sample_rate_target, "hz.csv.gz")
readr::write_csv(ppg_resampled, here::here(fname))


ppg = readr::read_csv(here::here("data/processed/ppg_wide_range.csv.gz"))

ppg = ppg %>% distinct()

ppg_list = split(ppg, list(ppg$id_subject,
                           ppg$cat_activity))
ppg_list = ppg_list %>% vctrs::list_drop_empty()

ppg_resampled =
  purrr::map(
    .x = ppg_list,
    .f = function(x) {
      x <- x %>% distinct() %>% arrange(tm_dttm)
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        rename(tm_dttm = HEADER_TIMESTAMP) %>%
        mutate(
          second = lubridate::floor_date(tm_dttm, unit = "seconds"),
          id_subject = x$id_subject[1],
          cat_activity = x$cat_activity[1]
        )
    }
  ) %>%
  bind_rows() %>%
  select(-second) %>%
  mutate(id_study = "ppg_wide_range")

fname = paste0("data/processed/resampled/ppg_wide_range_", sample_rate_target, "hz.csv.gz")
readr::write_csv(ppg_resampled, here::here(fname))


sfdla = readr::read_csv(here::here("data/processed/sfdla.csv.gz"),
                        col_types = cols(id_subject = col_character(),
                                         cat_activity_orig = col_character(),
                                         id_repetition  = col_character()))

sfdla_list = split(sfdla, list(sfdla$id_subject,
                           sfdla$cat_activity_orig,
                           sfdla$id_repetition))
sfdla_list = sfdla_list %>% vctrs::list_drop_empty()

sfdla_resampled =
  purrr::map(
    .x = sfdla_list,
    .f = function(x) {
      x = x %>% drop_na() %>% arrange(tm_dttm)
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        rename(tm_dttm = HEADER_TIMESTAMP) %>%
        mutate(
          second = lubridate::floor_date(tm_dttm, unit = "seconds"),
          id_subject = x$id_subject[1],
          cat_activity = x$cat_activity[1],
          cat_activity_orig = x$cat_activity_orig[1],
          id_repetition = x$id_repetition[1]
        )

      # left_join(resamp,
      #           key,
      #           by = c("id_subject" = "id_subject",
      #                  "second" = "second"))
    }
  ) %>%
  bind_rows() %>%
  select(-second) %>%
  mutate(id_study  = "sfdla")
fname = paste0("data/processed/resampled/sfdla_", sample_rate_target, "hz.csv.gz")
readr::write_csv(sfdla_resampled, here::here(fname))


realworld = readr::read_csv(here::here("data/processed/realworld.csv.gz"))

realworld_list = split(realworld, list(realworld$id_subject,
                               realworld$cat_activity))
realworld_list = realworld_list %>% vctrs::list_drop_empty()

realworld_resampled =
  purrr::map(
    .x = realworld_list,
    .f = function(x) {
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        rename(tm_dttm = HEADER_TIMESTAMP) %>%
        mutate(
          second = lubridate::floor_date(tm_dttm, unit = "seconds"),
          id_subject = x$id_subject[1],
          cat_activity = x$cat_activity[1]
        )
    }
  ) %>%
  bind_rows() %>%
  select(-second) %>%
  mutate(id_study = "realworld")

fname = paste0("data/processed/resampled/realworld_", sample_rate_target, "hz.csv.gz")
readr::write_csv(realworld_resampled, here::here(fname))


hmp = readr::read_csv(here::here("data/processed/hmp.csv.gz"))

hmp_list = split(hmp,
                 list(hmp$id_subject,
                      hmp$cat_activity,
                      hmp$id_repetition))
hmp_list = hmp_list %>% vctrs::list_drop_empty()

hmp_resampled =
  purrr::map(
    .x = hmp_list,
    .f = function(x) {
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        rename(tm_dttm = HEADER_TIMESTAMP) %>%
        mutate(
          second = lubridate::floor_date(tm_dttm, unit = "seconds"),
          id_subject = x$id_subject[1],
          cat_activity = x$cat_activity[1],
          id_repetition = x$id_repetition[1]
        )
    }
  ) %>%
  bind_rows() %>%
  select(-second) %>%
  mutate(id_study = "hmp")

fname = paste0("data/processed/resampled/hmp_", sample_rate_target, "hz.csv.gz")
readr::write_csv(hmp_resampled, here::here(fname))



mhealth = readr::read_csv(here::here("data/processed/mhealth.csv.gz"),
                          col_types = cols(id_subject = col_character()))

mhealth_list = split(mhealth,
                 list(mhealth$id_subject,
                      mhealth$cat_activity))
mhealth_list = mhealth_list %>% vctrs::list_drop_empty()

mhealth_resampled =
  purrr::map(
    .x = mhealth_list,
    .f = function(x) {
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        rename(tm_dttm = HEADER_TIMESTAMP) %>%
        mutate(
          second = lubridate::floor_date(tm_dttm, unit = "seconds"),
          id_subject = x$id_subject[1],
          cat_activity = x$cat_activity[1]
        )

    }
  ) %>%
  bind_rows() %>%
  select(-second) %>%
  mutate(id_study = "mhealth")

fname = paste0("data/processed/resampled/mhealth_", sample_rate_target, "hz.csv.gz")
readr::write_csv(mhealth_resampled, here::here(fname))

daliac = readr::read_csv(here::here("data/processed/daliac.csv.gz"),
                          col_types = cols(id_subject = col_character()))

daliac_list = split(daliac,
                     list(daliac$id_subject,
                          daliac$cat_activity))
daliac_list = daliac_list %>% vctrs::list_drop_empty()

daliac_resampled =
  purrr::map(
    .x = daliac_list,
    .f = function(x) {
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        rename(tm_dttm = HEADER_TIMESTAMP) %>%
        mutate(
          second = lubridate::floor_date(tm_dttm, unit = "seconds"),
          id_subject = x$id_subject[1],
          cat_activity = x$cat_activity[1]
        )

    }
  ) %>%
  bind_rows() %>%
  select(-second) %>%
  mutate(id_study = "daliac")

fname = paste0("data/processed/resampled/daliac_", sample_rate_target, "hz.csv.gz")
readr::write_csv(daliac_resampled, here::here(fname))

marea = readr::read_csv(here::here("data/processed/marea.csv.gz"))

marea_list = split(marea,
                    list(marea$id_subject,
                         marea$cat_activity))
marea_list = marea_list %>% vctrs::list_drop_empty()

marea_resampled =
  purrr::map(
    .x = marea_list,
    .f = function(x) {
      resamp = resample_accel_data(x,
                                   sample_rate = sample_rate_target,
                                   method = "linear") %>%
        rename(tm_dttm = HEADER_TIMESTAMP) %>%
        mutate(
          id_subject = x$id_subject[1],
          cat_activity = x$cat_activity[1],
          cat_activity_large = x$cat_activity_large[1]
        )
    }
  ) %>%
  bind_rows() %>%
  mutate(id_study = "marea")

fname = paste0("data/processed/resampled/marea_", sample_rate_target, "hz.csv.gz")
readr::write_csv(marea_resampled, here::here(fname))

