library(here)
library(purrr)
library(tidyverse)
library(R.matlab)

## ---- wrist accelerometry + step annotations (laboratory and free-living) ----

# Laboratory data.mat: TimeMeasure -> Test -> Trial (INDIP and/or Stereophoto reference)
# Free-living data.mat: TimeMeasure -> Recording (only Recording4 is the real 2.5 h recording
#   and has INDIP reference; Recordings 1-3 are calibration)
# Both are handled by walking down the struct until we hit a node containing an SU* field.
# Zenodo docs: https://zenodo.org/records/13987963

# Lab tests 1-3 are non-walking calibration tests, so they are dropped.
test_labels = c(
  Test4 = "timed_up_and_go",
  Test5 = "straight_walk_comfortable",
  Test6 = "straight_walk_slow",
  Test7 = "straight_walk_fast",
  Test8 = "l_test",
  Test9 = "surface_test",
  Test10 = "hallway_test",
  Test11 = "simulated_daily_activities"
)
free_living_recording = "Recording4"

# order = priority for taking steps when more than one reference system is available
step_sources = c("Stereophoto", "INDIP")

# R.matlab returns each matlab struct as a list-array with the field names in dimnames
is_struct = function(x) is.list(x) && length(dim(x)) == 3
unwrap = function(x) if (is_struct(x)) x[, , 1] else x
mfields = function(x) if (!is.null(dimnames(x))) dimnames(x)[[1]] else names(x)
mget = function(x, nm) unwrap(x)[[nm]]

ic_field = "InitialContact.Event" # step (initial contact) times, in s from start of recording
lr_field = "InitialContact.LeftRight" # optional left/right label for each step

# nodes = unwrapped struct that directly holds SU* fields
find_recordings = function(node, path = character()) {
  if (any(grepl("^SU", names(node)))) {
    return(list(list(node = node, path = path)))
  }
  out = list()
  for (nm in names(node)) {
    child = node[[nm]]
    if (is_struct(child)) {
      out = c(out, find_recordings(unwrap(child), c(path, nm)))
    }
  }
  out
}

# keep only the recordings we want: lab tests 4-11 (last trial of each test),
# or the free-living Recording4
select_recordings = function(recs, cat_setting) {
  if (cat_setting == "free_living") {
    return(keep(recs, ~ .x$path[2] %in% free_living_recording))
  }
  recs = keep(recs, ~ length(.x$path) >= 3 && .x$path[2] %in% names(test_labels))
  if (length(recs) == 0) return(recs)
  key = map_chr(recs, ~ paste(.x$path[1:2], collapse = "/"))
  trial_num = map_int(recs, ~ as.integer(sub("\\D*", "", .x$path[3])))
  last_trial = tapply(trial_num, key, max)
  recs[trial_num == last_trial[key]]
}

# step (initial contact) annotations from the first available reference system
# returns list(steps = tibble(num_row, cat_step_type), source = "INDIP"/"Stereophoto"/"none")
get_steps = function(rec, fs) {
  none = list(steps = tibble(num_row = integer(), cat_step_type = character()), source = "none")
  if (!("Standards" %in% names(rec))) return(none)
  stds = rec[["Standards"]]
  for (gs in step_sources) {
    if (!(gs %in% mfields(stds))) next
    gs_node = mget(stds, gs)
    if (!("MicroWB" %in% mfields(gs_node))) next
    wb = mget(gs_node, "MicroWB") # 1xN struct array; one entry per walking bout
    if (!is_struct(wb)) next # empty: no walking bouts
    steps = tibble(num_row = integer(), cat_step_type = character())
    for (i in seq_len(dim(wb)[3])) {
      bout = wb[, , i]
      if (!(ic_field %in% names(bout))) {
        stop("Can't find '", ic_field, "'. Fields available: ",
             paste(names(bout), collapse = ", "))
      }
      ic = as.numeric(bout[[ic_field]])
      lr = if (lr_field %in% names(bout)) as.character(unlist(bout[[lr_field]])) else rep("step", length(ic))
      steps = bind_rows(
        steps,
        tibble(num_row = as.integer(round(ic * fs) + 1), cat_step_type = lr)
      )
    }
    if (nrow(steps) > 0) {
      return(list(steps = distinct(steps, num_row, .keep_all = TRUE), source = gs))
    }
  }
  none
}

get_wrist_recording = function(rec, id_subject, path, cat_setting, wrist_loc_pattern = "Wrist") {
  # find the sensor unit (e.g. SU_INDIP) that has a wrist location
  su_names = grep("^SU", names(rec), value = TRUE)
  su_name = NULL
  for (nm in su_names) {
    if (any(grepl(wrist_loc_pattern, mfields(rec[[nm]])))) {
      su_name = nm
      break
    }
  }
  if (is.null(su_name)) return(NULL) # no wrist sensor in this recording
  loc_name = grep(wrist_loc_pattern, mfields(rec[[su_name]]), value = TRUE)[1]
  loc = mget(rec[[su_name]], loc_name)

  acc = mget(loc, "Acc") # n x 3, units g, columns are V, ML, AP
  # Fs holds one rate per sensor (acc, gyr, mag...): either a struct with named fields
  # or a vector with acc first. We want the accelerometer's.
  fs_raw = mget(loc, "Fs")
  fs = if (is_struct(fs_raw)) as.numeric(unwrap(fs_raw)[["Acc"]]) else as.numeric(fs_raw)
  fs = fs[1]
  ts = as.numeric(mget(loc, "Timestamp")) # unix seconds, UTC

  is_lab = cat_setting == "laboratory"
  dat = tibble(
    X = acc[, 1], Y = acc[, 2], Z = acc[, 3],
    num_row = seq_len(nrow(acc)),
    tm_dttm = as.POSIXct(ts, origin = "1970-01-01", tz = "UTC"),
    id_subject = id_subject,
    id_recording = paste(path, collapse = "_"),
    id_test = path[2], # Test5 for lab, Recording4 for free-living
    id_trial = if (is_lab) path[3] else NA_character_,
    cat_activity = if (is_lab) unname(test_labels[path[2]]) else "free_living",
    cat_setting = cat_setting,
    id_sensor = paste(su_name, loc_name, sep = "/"),
    num_fs = fs
  )

  st = get_steps(rec, fs)
  dat %>%
    left_join(st$steps, by = "num_row") %>%
    mutate(cat_step_source = st$source, # which reference system the steps came from ("none" = no annotations)
           ind_step = as.integer(!is.na(cat_step_type)),
           cat_step_type = ifelse(is.na(cat_step_type), "none", cat_step_type))
}

get_mobd = function(file, cat_setting) {
  # path is <subject>/<Laboratory or Free-living>/data.mat
  id_subject = basename(dirname(dirname(file)))
  dat = unwrap(readMat(file)[["data"]])
  recs = find_recordings(dat) %>% select_recordings(cat_setting)
  map(recs, ~ get_wrist_recording(.x$node, id_subject, .x$path, cat_setting)) %>%
    bind_rows()
}
