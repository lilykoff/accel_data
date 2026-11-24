---
editor_options: 
  markdown: 
    wrap: 72
---

# DaLiAc (Daily Life Activities) Database

## Dataset summary

-   Number of subjects: 19 (11 male)

-   Device: Shimmer with accelerometer range $\pm$ 6g

-   Sample rate: 204.8 Hz

-   Device location: right wrist

## Experimental setup

Subjects performed 13 activities.

Sitting, lying, standing, vacuuming, sweeping took 1 minute

Washing dishes, treadmill, and both cycling trials took 2 minutes

Walking took place from university campus building to another building

Stair climb/descent was up to the third floor and then back down

Rope jumping consisted of 5 trials with at least 5 jumps each

## Activities

1.  Sitting
2.  Lying
3.  Standing
4.  Washing dishes
5.  Vacuuming
6.  Sweeping
7.  Walking
8.  Ascending stairs
9.  Descending stairs
10. Treadmill running
11. Bicycling on ergometer at 50W
12. Bicycling on ergometer at 100W
13. Rope jumping

## Data structure

Separate files for each individual labeled `dataset<SUBJECTID>.txt`

There are 25 columns in each dataset:

-   Cols `1-6`: `A1, A2, A3, G1, G2, G3` for right wrist where `A1`,
    `A2`, `A3` are accelerometer axis 1, 2, and 3, respectively, and
    `G1, G2, G3` are gyroscope axes 1, 2, and 3

-   Cols `7-12`: `A1, A2, A3, G1, G2, G3` for chest

-   Cols `13-18`: `A1, A2, A3, G1, G2, G3` for hip

-   Cols `19-24`: `A1, A2, A3, G1, G2, G3` for ankle

-   Col `24`: Activity label `L`

## Raw

`accel_data/raw/clemson`

## Processed

`accel_data/processed/daliac.csv.gz`

Column names:

-   `id_subject` subject ID, one of "s1", "s2", ..., "s19"

-   `id_study` : `daliac`

-   `cat_activity` : one of `sit`, `lie_down` `stand` `wash_dishes`
    `vacuum` `sweep` `walk` `climb_stairs` `down_stairs` `run`
    `cycle_50w` `cycle_100w` `jump_rope`

-   `tm_dttm` : time based on a fake start time for each activity, row
    index, and sample rate of 204.8 Hz

-   `X`, `Y`, `Z`: acceleration in X, Y, and Z directions respectively,
    in `g`

## Citation

```         
Leutheuser, H., Schuldhaus, D., Eskofier, B. M. (2013) Hierarchical, multi-sensor based classification of daily life activities: comparison with state-of-the-art algorithms using a benchmark dataset, PLoS ONE, 8(10), e75196
```
