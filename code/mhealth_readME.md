# MHEALTH (Mobile HEALTH)

## Dataset summary

-   Number of subjects: 10

-   Device: Shimmer2

-   Sample rate: 50Hz

-   Device location: right wrist

## Experimental setup 

The activities were collected in an out-of-lab environment with no constraints on the way these must be executed, with the exception that the subject should try their best when executing them.

## Activities 

L1: Standing still (1 min)
L2: Sitting and relaxing (1 min)
L3: Lying down (1 min)
L4: Walking (1 min)
L5: Climbing stairs (1 min)
L6: Waist bends forward (20x)
L7: Frontal elevation of arms (20x)
L8: Knees bending (crouching) (20x)
L9: Cycling (1 min)
L10: Jogging (1 min)
L11: Running (1 min)
L12: Jump front & back (20x)

## Data structure

Data for each subject in separate log file named `mHealth_subject<SUBJECT_ID>.log`

Rows are samples, columns are values from each sensor

Activity labels correspond to numbers above

## Raw

`accel_data/raw/MHEALTHDATASET`

## Processed

`accel_data/processed/mhealth.csv.gz`

Column names:

-   `id_subject` subject ID, one of "1", "2", ..., "10",

-   `id_study` : `mhealth` for all rows

-   `cat_activity` : `null`, `stand`, `sit`, `lie_down`, `walk`, `waist_bend_forward`, `front_elevation_arm`, `knee_bend_crouch`, `cycle`, `jog`, `run`, `jump_front_back`, `climb_stairs`

-   `tm_dttm` : date-time in UTC of row recording. No time stamps were available in original data so this is seconds since `2000-01-01 00:00:00` for each subject

-   `X`, `Y`, `Z`: acceleration in X, Y, and Z directions respectively, in `g`

## Citation 

```         
Banos,Oresti, Garcia,Rafael, and Saez,Alejandro. (2014). MHEALTH Dataset. UCI Machine Learning Repository. https://doi.org/10.24432/C5TW22.
```
