---
editor_options: 
  markdown: 
    wrap: 72
---

# RealWorld (DataSet -- RealWorld (HAR) (2016))

## Dataset summary

-   Number of subjects: 15 (8 male)

-   Device: LG G Watch R

-   Sample rate: 50 Hz

-   Device location: wrist/forearm

## Experimental setup

For each activity, we recorded simultaneously the acceleration of the
body positions chest, forearm, head, shin, thigh, upper arm, and waist.
Each subject performed each activity roughly 10 minutes except for
jumping due to the physical exertion (\~1.7 minutes).

The devices were synchronized with the time service of the network
provider and the accelerometer was sensed with a sampling rate of 50 Hz
where the data was stored on a local SD card. The sampling rate was
chosen with consideration of battery life as well as with reference to
previous studies [7], [14]. The recording of the data was performed
using a self-developed sensor data collector and labeling framework. The
framework consists of a *Wear* (1) and *Hand* (2) application (see
Figure 2) which interact with each other via Bluetooth. This application
provides the possibility to control the built-in sensors, specify the
sampling rate, and record several sensors simultaneously. The binary^3^
and the source code^4^ of this application are publicly available.

## Activities

1.  Descending stairs

2.  Climbing stairs

3.  Jumping

4.  Lying

5.  Sanding

6.  Sitting

7.  Running/jogging

8.  Walking

## Data structure

enter here

## Raw

`accel_data/raw/realworld2016_dataset`

## Processed

`accel_data/processed/realworld.csv.gz`

Column names:

-   `id_subject` : one of `proband1`, ..., `proband15`

-   `id_study` : `realworld`

-   `cat_activity` : `up_stairs`, `down_stairs`, `sit`, `lie_down`,
    `run`, `walk`, `stand`, `jump_up_down`

-   `tm_dttm` : time given for each recording

-   `X`, `Y`, `Z`: acceleration in X, Y, and Z directions respectively,
    in `g`

## Citation

```         
T. Sztyler and H. Stuckenschmidt, "On-body localization of wearable devices: An investigation of position-aware activity recognition," 2016 IEEE International Conference on Pervasive Computing and Communications (PerCom), Sydney, NSW, Australia, 2016, pp. 1-9, doi: 10.1109/PERCOM.2016.7456521.
```
