---
editor_options: 
  markdown: 
    wrap: 72
---

# HMP (Human Movement Primitives)

## Dataset summary

-   Number of subjects: 16 (11 male)

-   Device: The tri-axial accelerometer is embedded in an ad-hoc sensing
    device (40mm x 22 mm x 12 mm)

-   Sample rate: 32Hz

-   Device location: right wrist

## Experimental setup

Unclear

## Activities

-   `brush_teeth`: to brush one's teeth with a toothbrush (complete
    gesture)

-   `climb_stairs`: to climb a number of steps of a staircase

-   `comb_hair`: to comb one's hair with a brush (complete gesture)

-   `descend_stairs`: to descend a number of steps of a staircase

-   `drink_glass`: to pick a glass from a table, drink and put it back
    on the table

-   `eat_meat`: to eat something using fork and knife (complete gesture)

-   `eat_soup`: to eat something using a spoon (complete gesture)

-   `getup_bed`: to get up from a lying position on a bed

-   `liedown_bed`: to lie down from a standing position on a bed

-   `pour_water`: to pick a bottle from a table, pour its content in a
    glass on the table and put it back on the table

-   `sitdown_chair`: to sit down on a chair

-   `standup_chair`: to stand up from a chair

-   `use_telephone`: to place a telephone call using a fixed telephone
    (complete gesture)

-   `walk`: to take a number of steps

## Data structure

Data for each subject and activity in separate txt file named
\`Accelerometer-\<YYYY-MM-DD-HH-MM-SS\>-\<ACTIVITY\>-\<SUBJECT_ID\>.txt

Rows are samples, columns are values from each sensor

Activity labels correspond to descriptions above

Acceleration in g coded as `real_val = -1.5 + (coded_val / 63) * 3`

## Raw

`accel_data/raw/HMP_Dataset-master`

## Processed

`accel_data/processed/hmp.csv.gz`

Column names:

-   `id_subject` subject ID, one of "f1", ... "f5", "m1", ..., "m10"
    where the "m" or "f" denote sex of participant

-   `id_study` : `hmp` for all rows

-   `cat_activity` : `null`, `stand`, `sit`, `lie_down`, `walk`,
    `waist_bend_forward`, `front_elevation_arm`, `knee_bend_crouch`,
    `cycle`, `jog`, `run`, `jump_front_back`, `climb_stairs`

-   `tm_dttm` : date-time in UTC of row recording

-   `X`, `Y`, `Z`: acceleration in X, Y, and Z directions respectively,
    in `g`

## Citation

```         
Bruno, B., Mastrogiovanni, F., Sgorbissa, A., Vernazza, T., Zaccaria, R.:
  Analysis of human behavior recognition algorithms based on acceleration data.
  In: IEEE Int Conf on Robotics and Automation (ICRA),
  pp. 1602--1607 (2013)

Bruno, B., Mastrogiovanni, F., Sgorbissa, A., Vernazza, T., Zaccaria, R.:
  Human motion modelling and recognition: A computational approach.
  In: IEEE Int Conf on Automation Science and Engineering (CASE),
  pp. 156--161 (2012)
```
