---
editor_options: 
  markdown: 
    wrap: 72
---

# Clemson Pedometer Evaluation Project

## Dataset summary

-   Number of subjects: 30 (15 male)

-   Device: Shimmer3

-   Sample rate: 15 Hz

-   Device location: non-dominant wrist

## Experimental setup

Subjects performed 3 trials:

1.  Regular walking, where participants walked two laps around a path at
    normal walking path
2.  Semi-regular walking, where participants performed a scavenger hunt,
    locating 4 objects in 4 different rooms in a building
3.  Unstructured gait, where participants built a small Lego toy by
    assembling pieces distributed among 12 small bins around a room.
    Participants were only allowed to gather one bin of pieces at a time
    and constructed the Lego at a central location. This pattern
    simulates preparing a meal in a kitchen.

They were recorded with an iPhone 4 camera by an experimenter and steps
were manually annotated by researchers from the video recordings after
the experiment.

## Activities

Unstructured, semi-regular, and regular walking as described above. each
activity took around 10 minutes.

## Data structure

Separate folders for each individual labeled `P<SUBJECT ID>`, within
each folder are `Irregular`, `Regular`, and `SemiRegular` for each of
the three respective trials, and within each of these folders an
acceleration file `P<SUBJECT ID>_<WALKING TYPE>.txt` and `steps.txt`
with the annotated steps.

In the accelerometery data files, the 9 columns are wrist xyz, hip xyz
and ankle xyz respectively. In the `steps.txt` ground truth files, the
number indicates the line index in the data file at which a step
occurred and the type of step

## Raw

`accel_data/raw/clemson`

## Processed

`accel_data/processed/clemson_ped.csv.gz`

Column names:

-   `id_subject` subject ID, one of "001", ... "030"

-   `id_study` : `clemson_ped`

-   `cat_activity` : one of `walk_irregular`, `walk_regular`, or
    `walk_semiregular`

-   `ind_step`: binary indicator of step

-   `cat_step_type` : one of `none`, `right`, `left`, `rightshift`,
    `leftshift`

-   `tm_dttm` : time based on a fake start time, row index, and sample
    rate of 15 Hz

-   `X`, `Y`, `Z`: acceleration in X, Y, and Z directions respectively,
    in `g`

## Citation

```         
R. Mattfeld, E. Jesch and A. Hoover, "A new dataset for evaluating pedometer performance", in the proc. of 2017 IEEE International Conference on Bioinformatics and Biomedicine, Kansas City, MO, pp. 865-869, 2017.

R. Mattfeld, "Evaluation of Pedometer Performance Across Multiple Gait Types Using Video for Ground Truth", PhD dissertation, Electrical and Computer Engineering Department, Clemson University, 2018.

R. Mattfeld, E. Jesch and A. Hoover, "Evaluating Pedometer Algorithms on Semi-Regular and Unstructured Gaits", in Sensors, vol. 21 no. 13, June 2021, doi: 10.3390/s21134260.
```
