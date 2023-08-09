
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
