---
editor_options: 
  markdown: 
    wrap: 72
---

# MAREA (Movement Analysis in Real-World Environments Using Accelerometers) Database

## Dataset summary

-   Number of subjects: 20 (12 male); 33.4 $\pm$ 7 years

-   Device: Shimmer3 with accelerometer range $\pm$ 8g

-   Sample rate: 128

-   Device location: left wrist (also wait and both ankles)

## Experimental setup

11 subjects performed indoor activities: - 10 minutes of walking +
running, where treadmill increased from 4km/hr to 8 km/hr in steps of
0.4km/hr every minute and subjects could run whenever they wanted to -
12 minutes of walking at 5, 0, 10, 0, 15, 0 degree incline, 2 minutes at
each incline - 6 minutes of indoor flat space walk and run: 3 minutes of
walking and 3 minutes of running 9 subjects performed outdoor
activities: - 6 minutes of walk/run: 3 minutes of walking followed by 3
minutes of running

## Activities

1.  Treadmill walk
2.  Treadmill run
3.  Treadmill slope walk
4.  Outdoor walk
5.  Outdoor run

## Data structure

Separate acceleration files in folder `Subject Data_txt format` for each
individual labeled `Sub<SubjectID>_<Device_location>.txt`

There are 3 columns in each dataset:

-   Cols `accX`, `accY`, `accZ`

Start/stop time stamps by row index for activities in `Activity Timings`
folder

Step indicators in `GroundTruth.mat`, n.b. indices in this matrix
correspond to intra-activity indices.

## Raw

`accel_data/raw/MAREA_dataset`

## Processed

`accel_data/processed/marea.csv.gz`

Column names:

-   `id_subject` subject ID, one of 1, 2, 3, 5... 20 (subject 4 missing)

-   `id_study` : `marea`

-   `cat_activity` : one of `treadmill_walk`, `treadmill_run`
    `treadmill_slopewalk`, `indoor_walk` `indoor_run`, `outdoor_walk`,
    `outdoor_run`, or `NA`

-   `cat_activity_large` : one of `treadmill_walkrun`,
    `treadmill_slopewalk`, `indoor_walkrun`, `outdoor_walkrun`, or `NA`

-   `tm_dttm` : time based on a fake start time for each activity, row
    index, and sample rate of 128 Hz

-   `cat_steptype`: step indicator, either `left_heelstrike`,
    `right_heelstrike`, or NA if no step

-   `tm_time_s`: time in seconds

-   `num_row_subject` and `num_row_subject_activity`: row indices used
    to match activities and step events

-   `X`, `Y`, `Z`: acceleration in X, Y, and Z directions respectively,
    in `g`

## Citation

```         
Siddhartha Khandelwal, Nicholas Wickström, Evaluation of the performance of accelerometer-based gait event detection algorithms in different real-world scenarios using the MAREA gait database, Gait & Posture, Volume 51, January 2017, Pages 84-90, ISSN 0966-6362. 
```
