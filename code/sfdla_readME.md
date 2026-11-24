---
editor_options: 
  markdown: 
    wrap: 72
---

# SFDLA (Simulated Falls and Daily Living Activities Dataset)

## Dataset summary

-   Number of subjects: 17

-   Device: Xsens MTw Motion Tracking Kit

-   Sample rate: 25 Hz

-   Device location: right wrist

## Experimental setup

The activities were collected in an out-of-lab environment with no
constraints on the way these must be executed, with the exception that
the subject should try their best when executing them.

## Activities

### Fall actions

901 front-lying, from vertical falling forward to the floor 

902 front-protecting-lying, from vertical falling forward to the floor
with arm protection 

903 front-knees, from vertical falling down on the knees 

904 front-knees-lying, from vertical falling down on the knees and then
lying on the floor 

905 front-quick-recovery, from vertical falling on the floor and quick
recovery 

906 front-slow-recovery, from vertical falling on the floor and slow
recovery 

907 front-right, from vertical falling down on the floor, ending in
right lateral position 

908 front-left, from vertical falling down on the floor, ending in left
lateral position 

909 back-sitting, from vertical falling on the floor, ending sitting 

910 back-lying, from vertical falling on the floor, ending lying 

911 back-right, from vertical falling on the floor, ending lying in
right lateral position 

912 back-left, from vertical falling on the floor, ending lying in left
lateral position 

913 right-sideway, from vertical falling on the floor, ending lying 

914 right-recovery, from vertical falling on the floor with subsequent
recovery 

915 left-sideway, from vertical falling on the floor, ending lying 

916 left-recovery, from vertical falling on the floor with subsequent
recovery 

917 rolling-out-bed, from lying, rolling out of bed and going on the
floor 

918 podium, from vertical standing on a podium going on the floor 

919 syncope, from standing falling on the floor following a vertical
trajectory 

920 syncope-wall, from standing falling down slowly slipping on a wall 

### ALDS

801 walking-fw, walking forward 

802 walking-bw, walking backward 

803 jogging, running 

804 squatting-down, squatting, then standing up 

805 bending, bending about 90 degrees 

806 bending-pick-up, bending to pick up an object on the floor 

807 limp, walking with a limp 

808 stumble, stumbling with recovery 

809 trip-over, bending while walking and then continuing walking 

810 coughing-sneezing, coughing or sneezing 

811 sit-chair from vertical, to sitting with a certain acceleration onto
a chair (hard surface) 

812 sit-sofa from vertical, to sitting with a certain acceleration onto
a sofa (soft surface) 

813 sit-air from vertical, to sitting in the air exploiting the muscles
of legs 

814 sit-bed from vertical, to sitting with a certain acceleration onto a
bed (soft surface) 

815 lying-bed, from vertical lying on the bed 

816 rising-bed, from lying to sitting 

## Data structure

Each folder `101`, `102`, et.c contains 36 classes of tests; 20 of which
are falls and 16 of which are daily living activities, see codebook

Each activity contains 5 or 6 tests `Test_<TEST NUM>`

Each test has data from 6 sensors, we use `340537` right wrist sensor

Each sensor record \*.txt data file contains 23 columns are Counter,
Temperature, VelInc_X, VelInc_Y, VelInc_Z, OriInc_w, OriInc_x, OriInc_y,
OriInc_z, Acc_X, Acc_Y, Acc_Z, Gyr_X, Gyr_Y, Gyr_Z, Mag_X, Mag_Y, Mag_Z,
Pressure, Roll, Pitch, Yaw, RSSI respectively. 

## Raw

`accel_data/raw/simulated-falls-and-daily-living-activities`

## Processed

`accel_data/processed/mhealth.csv.gz`

Column names:

-   `id_subject` subject ID, one of `101`, `102`, and so on

-   `id_study` : `sfdla` for all rows

-   `id_repetition`: whichever rep of the activity from
    `Test_<REPETITION>`

-   `cat_activity_orig` : number codes for specific activity shown above

-   `cat_activity`: one of `fall`, `walk`, `walk_backward`, `run`,
    `squat`, `bend`, `walk_limp`, `stumble`, `cough`, `sit`, `lie_down`,
    `sit_up` according to following rules:

    ``` r
    grep("90|91", cat_activity) ~ "fall",
            cat_activity == "801" ~ "walk",
            cat_activity == "802" ~ "walk_backward",
            cat_activity == "803" ~ "run",
            cat_activity == "804" ~ "squat",
            cat_activity == "805" | cat_activity == "806" ~ "bend",
            cat_activity == "807" ~ "walk_limp",
            cat_activity == "808" | cat_activity == "809" ~ "stumble",
            cat_activity == "810" ~ "cough",
            cat_activity %in% c("811", "812", "813", "814") ~ "sit",
            cat_activity == "815" ~ "lie_down",
            cat_activity == "816" ~ "sit_up")
    ```

-   `tm_dttm` : date-time in UTC of row recording, based on
    `sample_rate` from `.txt` files, row index, and made up start date

-   `X`, `Y`, `Z`: acceleration in X, Y, and Z directions respectively,
    in `g`

## Citation

```         
Zdemir,Ahmet and Barshan,Billur. (2018). Simulated Falls and Daily Living Activities Data Set. UCI Machine Learning Repository. https://doi.org/10.24432/C52028.
```
