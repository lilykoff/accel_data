library(readr)
library(purrr)
library(tidyverse)
library(magrittr)

options(digits.secs = 3)
# functions to read in WFDB files
#' RDSAMP
#'
#' This function reads the signal described in the header file for \emph{\strong{header.filename}}.
#' Inclusion of the .hea file extension is optional; both formats can be accepted by this function.
#' @param header.filename Record name or filename for the header file to be read.
#' @return
#' An array containing time-ordered values for the signals described by the header file
#' @export
rdsamp <- function(header.filename) {
  siginfo <- wfdbdesc(header.filename, FALSE)

  read.data <- function(data.filename, signal.format, num.signals, signal.length, adc.gains, adc.zeros) {
    data.file = file(data.filename, "rb")

    if (signal.format == 16) {
      signal.data <- t((array(readBin(data.file, integer(), n = file.size(data.filename)/2, size = 2,
                                      endian = "little"), dim=c(num.signals, signal.length)) - adc.zeros)/adc.gains)
    } else if (signal.format == 80) {
      raw.data <- readBin(data.file, integer(), n = file.size(data.filename), size = 1, endian = "little", signed = FALSE)
      signal.data <- t((array(raw.data - 128, dim=c(num.signals, signal.length)) - adc.zeros)/adc.gains)
    }

    close(data.file)
    return(signal.data)
  }

  if (siginfo$record.type == "single") {
    # If single-segment file, we can simply read and return the first segment.
    return (read.data(data.filename = sprintf("%s%s", siginfo$root.dir, siginfo$segment.names),
                      signal.format = siginfo$signal.format, num.signals = siginfo$num.signals,
                      signal.length = siginfo$record.length, adc.gains = siginfo$adc.gains, adc.zeros = siginfo$adc.zeros))
  } else {
    # TODO correctly allocate space

    # We have to allocate a table for the unified data, read each segment, and place it into its proper place
    signal.data <- array(NA, dim = c(siginfo$record.length, siginfo$num.signals))
    segment.starts <- c(cumsum(c(1,siginfo$segment.lengths[1:(length(siginfo$segment.lengths)-1)])))
    segment.ends <- c(segment.starts[-1]-1, siginfo$record.length)
    segment.starts <- segment.starts[siginfo$segment.names != "~" & siginfo$segment.lengths > 0]
    segment.ends <- segment.ends[siginfo$segment.names != "~" & siginfo$segment.lengths > 0]
    # Recursively read data from segments
    valid.segments = sapply(siginfo$segment.names[siginfo$segment.names != "~" & siginfo$segment.lengths > 0],
                            function(x) sprintf("%s%s", siginfo$root.dir, x), USE.NAMES = FALSE)

    # Need to check that there are any valid segments
    if(length(valid.segments) > 0) {
      segment.headers <- lapply(valid.segments, function(x) wfdbdesc(x, FALSE))
      segment.data <- lapply(valid.segments, rdsamp)
      # Yo dawg, I heard you like lambdas, so I put a lambda in my lambda so you can lambda while you lambda
      indices <- lapply(segment.headers, function(x) sapply(x$descriptions,
                                                            function (y) which(siginfo$descriptions == y)))
      # Unify the data
      for (c in 1:length(segment.data)) {
        signal.data[segment.starts[c]:segment.ends[c],indices[[c]]] = segment.data[[c]]
      }
    }

    return(signal.data)
  }
}
#' WFDBDESC
#'
#' This function reads specifications for the signal described in the header file for \emph{\strong{header.filename}}.
#' Inclusion of the .hea file extension is optional; both formats can be accepted by this function.
#' @param header.filename Record name or filename for the header file to be read.
#' @return
#' A list containing record name, record type, sampling frequency, record length, base datetime, number of signals,
#' channel descriptions, adc gain, zero, units for each channel, segment names, and lengths of segments for
#' multi-channel files
#' @export
wfdbdesc <- function(header.filename, read.datetime = TRUE) {
  # Get root directory of header file, as we may need to read other files using that path
  root.dir <- ""
  if (grepl("/", header.filename)) {
    root.dir <- substr(header.filename, 1, regexpr("/[^/]*$", header.filename))
    header.filename <- substr(header.filename, regexpr("/[^/]*$", header.filename)+1, nchar(header.filename))
  }
  # If filename lacks extension, add extension
  if (!grepl(".hea$", header.filename)) {
    header.filename <- sprintf("%s.hea", header.filename)
  }

  # Read in header file for parsing
  header.filepath <- sprintf("%s%s", root.dir, header.filename)
  header.file <- file(header.filepath)
  header.data <- trimws(strsplit(readChar(header.file,file.size(header.filepath)),"\n")[[1]])
  close(header.file)

  # Process the first line of the header (record line):
  record.line <- strsplit(header.data[1]," ")[[1]]
  # Determine what type of record this is: single segment or multi-segment, number of signals, sampling frequency,
  # and record length
  record.name <- record.line[1]
  num.signals <- as.numeric(record.line[2])
  if (grepl("/", record.line[3])) {
    sample.frequency <- as.numeric(substr(record.line[3],1,regexpr("/",record.line[3])-1))
  } else {
    sample.frequency <- as.numeric(record.line[3])
  }
  record.length <- as.numeric(record.line[4])

  # Single segment datetimes
  if (read.datetime) {
    base.datetime <- lubridate::dmy(record.line[6]) + lubridate::hms(record.line[5])
  } else {
    base.datetime <- NA
  }

  if (grepl("/", record.name)) {
    # Multi segment file
    record.type <- "multi"
    num.segments <- as.numeric(substr(record.name, regexpr("/",record.name)+1,nchar(record.name)))
    record.name <- substr(record.name, 1, regexpr("/",record.name)-1)
    # Read segment names and segment lengths
    segments <- strsplit(header.data[2:(num.segments+1)], " ")
    segment.lengths <- sapply(segments, function(x) as.numeric(x[2]))
    segment.names <- sapply(segments, function(x) x[1])

    # From first valid segment, read descriptions, adc gains, and adc zeros
    first.segment <- segment.names[min(which(segment.names != "~"))]
    layout.filepath <- sprintf("%s%s.hea", root.dir, first.segment)
    layout.file <- file(layout.filepath)
    layout.data <- trimws(strsplit(readChar(layout.file, file.size(layout.filepath)), "\n")[[1]])
    close(layout.file)

    signal.lines <- strsplit(layout.data[2:(num.signals+1)], " ") # TODO
    signal.format <- NULL
  } else {
    # Single segment, can proceed to read signal specification from current file
    record.type <- "single"
    signal.lines <- strsplit(header.data[2:(num.signals+1)], " ")
    segment.names <- signal.lines[[1]][1];
    segment.lengths <- record.length
    signal.format <- as.numeric(signal.lines[[1]][2])
  }

  # Read signal descriptions, adc gains, units of measure, and zeros from signal specification
  descriptions <- sapply(signal.lines, function(x) {
    if (length(x) > 9) {
      return(paste(x[-(1:8)],collapse=" "))
    } else {
      return(x[9])
    }
  })
  # This extra code determines if there is a baseline
  adc.gains <- sapply(signal.lines, function(x) {
    if (grepl("\\(.*\\)", x[3])) {
      return(as.numeric(substr(x[3], 1, regexpr("\\(", x[3])-1)))
    } else {
      return(as.numeric(substr(x[3], 1, regexpr("/", x[3])-1)))
    }

  })
  adc.units <- sapply(signal.lines, function(x) substr(x[3], regexpr("/", x[3])+1, nchar(x[3])))
  # If no baseline is present, zero is set to ADC zero. Otherwise it is equal to the baseline.
  adc.zeros <- sapply(signal.lines, function(x) {
    if (grepl("\\(.*\\)", x[3])) {
      return(as.numeric(substr(x[3], regexpr("\\(", x[3])+1, regexpr("\\)", x[3])-1)))
    } else {
      return(as.numeric(x[5]))
    }
  })

  return(list(root.dir = root.dir, record.name = record.name, record.type = record.type,
              sample.frequency = sample.frequency,  record.length = record.length, base.datetime = base.datetime,
              signal.format = signal.format, num.signals = num.signals, descriptions = descriptions, adc.gains = adc.gains,
              adc.zeros = adc.zeros, adc.units = adc.units, segment.names = segment.names,
              segment.lengths = segment.lengths))
}

