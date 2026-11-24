---
editor_options: 
  markdown: 
    wrap: 72
---

# PPG (Wrist PPG During Exercise)

## Dataset summary

-   Number of subjects: 8 (3 male)

-   Device: Shimmer3 GSR+

-   Sample rate: 256Hz

-   Device location: left wrist

## Experimental setup

The objective was to introduce a range of representative motion
artifacts into the collected heart signals, not to carry out a set
exercise routine. As such each participant was free to set the pace of
the treadmill and pedal rate on the bike so they were comfortable and
also to change these settings or stop the exercise at any time. Most
participants spent between 4 and 6 minutes on each activity. In all
cases the subject was starting from rest. All signals were sampled at
256 Hz. Records from 8 participants are present (3 male, 5 female), aged
22\--32 (mean 26.5).

## Activities

1.  walking on a treadmill at a normal pace for up to 10 minutes.

2.  light jog/run on a treadmill, at a pace set by the participant, for
    up to 10 minutes.

3.  pedal on an exercise bike set a low resistance for up to 10 minutes.

4.  pedal on an exercise bike set at a higher resistance for up to 10
    minutes.

## Data structure

PPG, motion, and ecg recordings are provided in WFDB format, with record
names reflecting the subject number and the activity. WFDB annotation
files of the reference ECG r-peaks are also provided.

Functions from
[`https://rdrr.io/github/Absox/wfdb.R/`](https://rdrr.io/github/Absox/wfdb.R/)
are used to read in the data

Data for each subject in separate `.dat` file named
`<SUBJECT ID>_<ACTIVITY>.dat`; description of the column names are in
`<SUBJECTID>_<ACTIVITY>.hea`

Activity labels correspond to numbers above

## Raw

`accel_data/raw/wrist-ppg-during-exercise-1.0.0`

## Processed

`accel_data/processed/ppg_low_noise.csv.gz` - low noise accelerometer
data

`accel_data/processed/ppg_wide_range.csv.gz` - wide range accelerometer
data

Column names:

-   `id_subject` subject ID, one of "s1", "s2", ..."s9"

-   `id_study` : `ppg_low_noise` for low noise data, `ppg_wide_range`
    for wide range data

-   `cat_activity` : `run`, `walk`, `high_resistance_bike`,
    `low_resistance_bike`

-   `tm_dttm` : time given for each recording added to a fake start time
    (2000-01-01 00:00:00)

-   `X`, `Y`, `Z`: acceleration in X, Y, and Z directions respectively,
    in `g`

## Citation

```         
Delaram Jarchi and Alexander J. Casson. Description of a Database Containing Wrist PPG Signals Recorded during Physical Exercise with Both Accelerometer and Gyroscope Measures of Motion. Data 2017, 2(1), 1; doi:10.3390/data2010001
```

