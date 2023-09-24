library(tidyr)
library(magrittr)
library(dplyr)
library(walking)
library(tidyr)
Sys.setenv("SSQ_PARALLEL" = 0)

raw_IU_accel = readRDS("raw_accelerometry_data.rds")

# renames and reassigns
raw_IU_accel %<>%
  dplyr::select(X = lw_x, Y = lw_y, Z = lw_z, activity, time = time_s, id)
start_time <- as.POSIXct("2023-09-21 12:10:00.00", tz = "UTC") # choose arbitrary start time
raw_IU_accel <-
  raw_IU_accel %>%
  mutate(time = start_time + time)
walk_drive <-
  raw_IU_accel %>%
  filter(activity %in% c("walking", "driving"))


# split into list to do walking separately for each sub
walk_drive_list <-
  split(walk_drive, walk_drive$id)

check_output =
  walk_drive %>%
  mutate(time = lubridate::floor_date(time)) %>%
  group_by(time, id) %>%
  count(activity) %>%
  mutate(pct = n/sum(n)) %>%
  ungroup() %>%
  filter(pct == 1) %>%
  select(-n, -pct) %>%
  mutate(is_walking = activity == "walking") %>%
  select(-activity)

# accuracy function
get_accuracy = function(activity, steps) {
  steps <- steps %>%
    mutate(
      time = lubridate::floor_date(time, unit = "secs")
    )
  res = activity %>%
    left_join(steps %>% mutate(steps = steps > 0))
  res = res %>%
    count(steps, is_walking)
  res %>%
    summarise(acc = sum(n[steps == is_walking], na.rm = TRUE)/sum(n, na.rm = TRUE)) %>%
    pull(acc)
}


# parameter grid
params = expand_grid(
  min_amplitude = seq(0, 1, by = 0.1),
  step_frequency_min = seq(1, 2, by = 0.2),
  step_frequency_max = seq(1.5, 3, by = 0.2),
  min_duration_peak = seq(2L, 5L, by = 1L)
)
params = params %>%
  filter(step_frequency_max > step_frequency_min)

n <- 100
nr <- nrow(params)
params_list <- split(params, rep(1:ceiling(nr/n), each=n, length.out=nr))



get_fold = function() {
  ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
  if (is.na(ifold)) {
    ifold = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  }
  print(paste0("fold is: ", ifold))
  ifold
}



fold <- get_fold()
params <- params_list[[fold]]

res_oak =
  lapply(seq(nrow(params)), function(i) {
    x = params[i,]
    steps <-
      purrr::map_df(.f = walking::find_walking,
                    .x = walk_drive_list,
                    min_amplitude = x$min_amplitude,
                    step_frequency = c(x$step_frequency_min, x$step_frequency_max),
                    min_duration_peak = x$min_duration_peak,
                    .id = "id")
    get_accuracy(check_output, steps)
    # accuracy
  })

saveRDS(res_oak, paste0("res_oak", fold, ".rds"))

# read in results of optimization
files <-
  list.files(here::here("oak_results"), full.names = TRUE) %>%
  as_tibble() %>%
  mutate(
    num = as.numeric(gsub("\\D", "", value))
  ) %>%
  arrange(num) %>%
  pull(value)

file_list <- purrr::map(.x = files,
                        .f = readRDS)
# make into vector
all_accuracies <- file_list %>% unlist()
# parameter list, same length as vector which is good
params = expand_grid(
  min_amplitude = seq(0, 1, by = 0.1),
  step_frequency_min = seq(1, 2, by = 0.2),
  step_frequency_max = seq(1.5, 3, by = 0.2),
  min_duration_peak = seq(2L, 5L, by = 1L)
)
params = params %>%
  filter(step_frequency_max > step_frequency_min)
hist(all_accuracies) # just loking at distribution of accuracy

# max accuracy
all_accuracies[which.max(all_accuracies)]

# what are the best parameters
x <- params[which.max(all_accuracies),]; x
# fit model with best params
# raw_IU_accel = readRDS(here::here("data/labeled-raw-accelerometry-data-captured-during-walking-stair-climbing-and-driving-1.0.0/raw_accelerometry_data.rds"))

# renames and reassigns
raw_IU_accel %<>%
  dplyr::select(X = lw_x, Y = lw_y, Z = lw_z, activity, time = time_s, id)
start_time <- as.POSIXct("2023-09-21 12:10:00.00", tz = "UTC") # choose arbitrary start time
raw_IU_accel <-
  raw_IU_accel %>%
  mutate(time = start_time + time)
walk_drive <-
  raw_IU_accel %>%
  filter(activity %in% c("walking", "driving"))


# split into list to do walking separately for each sub

steps <-
  purrr::map_df(.f = walking::find_walking,
                .x = walk_drive_list,
                min_amplitude = x$min_amplitude,
                step_frequency = c(x$step_frequency_min, x$step_frequency_max),
                min_duration_peak = x$min_duration_peak,
                .id = "id")

# accuracy function
get_accuracy_onesub = function(activity, steps, sub) {
  steps <- steps %>%
    filter(id == sub) %>%
    mutate(
      time = lubridate::floor_date(time, unit = "secs")
    )
  res = activity %>%
    filter(id == sub) %>%
    left_join(steps %>% mutate(steps = steps > 0))
  res = res %>%
    count(steps, is_walking)
  res %>%
    summarise(acc = sum(n[steps == is_walking], na.rm = TRUE)/sum(n, na.rm = TRUE)) %>%
    pull(acc)
}

subs <- unique(steps$id)

res <- purrr::map_dbl(.x = subs,
                      .f = get_accuracy_onesub,
                      activity = check_output,
                      steps = steps)
mean(res)
hist(res)

get_accuracy(check_output, steps)

steps <- steps %>%
  mutate(
    time = lubridate::floor_date(time, unit = "secs")
  )
check_output %>%
  left_join(steps %>% mutate(steps = steps > 0)) %>%
  count(steps, is_walking)

# sens = tp / (tp + fn)
# spec = tn / (tn + fp)

sens <- 14047 / (14047 + 1876); sens
spec <- 37011 / (37011 + 428); spec

min(res)
max(res)

