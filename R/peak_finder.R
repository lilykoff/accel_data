library(tidyverse)
library(magrittr)

# read in accelerometry data
# smooth with 300 df/minute
# find peaks
# filter peaks
stopifnotcolumn <- function(data, string) {
  if (is.na(match(string, colnames(data)))) {
    stop(string, " was not found in your data frame.")
  }
  return(invisible(TRUE))
}

smooth <- function(data, sample_rate) {
  stopifnotcolumn(data, "n")
  stopifnot(length(unique(data$n))==1) # check that n is same for all entries
  stopifnotcolumn(data, "index")
  stopifnot(is.numeric(data$index))
  stopifnotcolumn(data, "vm")
  stopifnot(is.numeric(data$vm))
  # we want 300 degrees of freedom per minute
  # corresponds to 300/sample_rate*60
  # might not have full amt of observations per minute, correct for that
  deg_free = data$n[1] * (300 / (sample_rate * 60))
  data$smooth <-
    predict(lm(vm ~ splines::ns(index, df = deg_free), data = data))
  data
}

# input is data
# output is
find_steps_peaks <- function(acc_data, sample_rate) {
  # make row number index and split by minute
  stopifnotcolumn(acc_data, "time")
  stopifnot(is.POSIXct(acc_data$time))
  stopifnotcolumn(acc_data, "vm")
  acc_data <-
    acc_data %>%
    mutate(minute = lubridate::floor_date(time, unit = "minutes")) %>%
    group_by(minute) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    mutate(index = row_number())

  split <- split(acc_data, acc_data$minute)

  # smooth by minute (speeds up computation)
  smoothed_acc_data <-
    map_df(.x = split,
           .f = smooth,
           sample_rate = sample_rate)

  # find peaks where we allow max 3 peaks (steps) per second, peaks have to be at least 1.2 vm
  peaks <-
    pracma::findpeaks(
      smoothed_acc_data$smooth,
      nups = round(sample_rate / 6, 0),
      ndowns = round(sample_rate / 6, 0),
      minpeakheight = 1.2,
      minpeakdistance = round(sample_rate / 3, 0)
    ) %>%
    data.frame() %>%
    janitor::clean_names() %>%
    arrange(x3) # sort by start of peak, not by peak height which is default

  # filter peaks
  peaks %<>%
    mutate(
      diff = x3 - lag(x4),
      # difference between start and end of previous peak
      leftdist = x2 - x3,
      # left "height" of peak
      rightdist = x4 - x2,
      # right "height of peak"
      diff_in_dist = abs(leftdist - rightdist),
      # diff in left and right, measure of symmetry
      streak = ifelse(
        diff <= sample_rate &
          diff_in_dist <= round(0.075 * sample_rate, 0),
        1,
        0
      )
    )
  # count peak as step if symmetric and occurs less than 1 sec after previous

  # get run length encoding of the "streaks" of walkingm add to df
  rle_peaks = rle(peaks$streak)
  inv = rep(rle_peaks$lengths, times = rle_peaks$lengths)
  peaks$rle = inv

  # filter to 3 consecutive steps
  # turn into long df to merge back with acc data
  # get peak id so you can figure out how much of each second step is in
  walk_peaks <-
    peaks %>%
    filter(rle >= 3, streak == 1) %>%
    mutate(peakid = paste0("p", row_number()),
           length = x4 - x3) %>%
    select(peak_ind = x2,
           start = x3,
           end = x4,
           length,
           peakid) %>%
    mutate(index = map2(start, end, seq), .keep = "unused") %>%
    unnest(index)

  left_join(acc_data, walk_peaks) %>%
    mutate(time = floor_date(time, unit = "seconds")) %>%
    group_by(time, peakid) %>%
    summarize(n = n(), length = first(length)) %>%
    mutate(freq = n / length) %>%
    group_by(time) %>%
    summarize(steps_peak = sum(freq, na.rm = TRUE))

}

find_steps_peaks(acc_samp, sample_rate = 80) # output is
