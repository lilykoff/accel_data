---
editor_options: 
  markdown: 
    wrap: 72
---

# ZJU (ZJU-GaitAcc)

## Dataset summary

-   Number of subjects: 175 subjects (2/3 male). 153 of them are present
    in 2 sessions (Session 1 & 2), and 22 of them are in only 1 session
    (Session 0)

-   Device: Wii remote

-   Sample rate: 100 Hz

-   Device location: right wrist

## Experimental setup

We asked the participants to naturally and straightly walk through a
level floor of 20 m length. The recording began before they started to
walk and ended after they stopped. Nonetheless, only the middle segments
that corresponds to the walking periods are taken to be useful. We
manually annotated the starting and ending points of the useful
segments, where the badly collected cycles caused by unstable wireless
communication at the two ends were eliminated in the meanwhile.
Normally, the useful length of a record are 7--15 s.

## Activities

1.  Walking

## Data structure

The folder structure of this dataset:

```         
data_dir/session_(session id)/subj_(subject id)/rec_(record id)/
```

Inside a fold: 

-   (body location id).txt

-   cycles.txt

-   useful.txt

Body location id indicates:

-   1 - the right wrist

-   2 - the left upper arm

-   3 - the right side of pelvis

-   4 - the left thigh

-   5 - the right ankle

Format of (body location id).txt:

-   3-by-*N* matrix in ASCII-delimited format of MATLAB.

-   one line for one row

-   elements in the same row are separated by commas

-   3 rows for the *x*,*y*,*z* components of acceleration

-   *N* columns for *N* acceleration samples

-   frequency: 100Hz

-   unit: g \-- the gravitational constant

Format of cycles.txt:

-   1-by-*C* matrix in ASCII-delimited format of MATLAB.

-   elements are separated by commas

-   each element indicates one cycle borders

-   *C* elements for *C* cycle borders in sequential order, where the
    cycle number is (*C* − 1)

Format of useful.txt:

-   1-by-2 matrix in ASCII-delimited format of MATLAB.

-   elements are separated by commas

-   the first element indicates the start point of the useful segment

-   the second element indicates the end point of the useful segment

## Raw

`accel_data/raw/zju_data/zju-gaitacc`

## Processed

`accel_data/processed/zju.csv.gz`

Column names:

-   `id_subject` subject id

-   `id_session` one of 0, 1, 2 for the recording session

-   `id_study` : `zju`

-   `cat_activity` : `walk`

-   `tm_dttm` : time given for each recording based on row index and
    fake start time

-   `ind_cycle` indicator for border of step cycle

-   `cat_activity` either `walk` or `pre_post_walk` based on
    `useful.txt` annotations

-   `X`, `Y`, `Z`: acceleration in X, Y, and Z directions respectively,
    in `g`

## Citation

```         
Yuting Zhang, Gang Pan, Kui Jia, Minlong Lu, Yueming Wang, Zhaohui Wu, “Accelerometer-based Gait Recognition by Sparse Representation of Signature Points with Clusters”, IEEE Transactions on Cybernetics, vol. 45, no. 9, pp. 1864-1875, September 2015.
```
