---
editor_options: 
  markdown: 
    wrap: 72
---

# IU (Labeled raw accelerometry data captured during walking, stair climbing and driving)

## Dataset summary

-   Number of subjects: 32 (13 male), ages 23 to 52

-   Device: ActiGraph GT3X+

-   Sample rate: 100 Hz

-   Device location: left wrist (non-dominant for all)

## Experimental setup

The study protocol included a walking pathway (approx. 0.66 miles)
followed by a driving trail (approx. 12.8 miles). Data were downloaded
immediately following each participant's session.

The walking component consisted of 5 periods of walking on level ground,
6 periods of descending stairs, and 6 periods of ascending the stairs;
the part lasted between 9.0 and 13.5 minutes. Participants were asked to
walk at their usual pace along a predefined course to imitate a
free-living activity. One participant briefly forgot the instructions
and had an additional period of walking on the level ground before
turning around to ascend the stairs.

Right after the walking part of the experiment, participants were
accompanied to their vehicle, and they then drove on a predefined route
for between 18 and 30 min, depending on traffic. The route included both
highway and city driving.

To ensure accuracy of identifying the start and stop times of different
activities, participants were asked to clap three times at the beginning
and end of each activity. The clapping movement generated three spikes
of magnitude in the raw accelerometry data signal, allowing to mark the
beginning and end of each activity and to accurately assign activity
labels for each section of the protocol in a data preprocessing stage.
Data corresponding to a few seconds before/after the first/last activity
are included and labeled as "non-study activity".

## Activities

1.  Descending stairs

2.  Climbing stairs

3.  Walking

4.  Driving

## Data structure

Participant data in
`raw/IU_data/labeled-raw-accelerometery-data-captured-during-walking-stair-climbing-and-driving-1.0.0/particpant_demog.csv.gz`

Individual acceleration data in
`raw/IU_data/labeled-raw-accelerometery-data-captured-during-walking-stair-climbing-and-driving-1.0.0/raw_accelerometry_data`
, named `id<SUBJECT ID>.csv.gz`

## Raw

`accel_data/raw/IU_data` ;
`raw/IU_data/labeled-raw-accelerometry-data-captured-during-walking-stair-climbing-and-driving-1.0.0/raw_accelerometry_data.csv`

## Processed

`accel_data/processed/iu.csv.gz`

Column names:

-   `id_subject` : subject id, `id<alphanumericcode>`

-   `id_study` : iu

-   `cat_activity` : `non_study_activity`, `up_stairs`, `down_stairs`,
    `walk`, `drive`, `clap`

-   `tm_dttm` : time for each recording based on fake start time and
    `time_s` in original data

-   `X`, `Y`, `Z`: acceleration in X, Y, and Z directions respectively,
    in `g`

## Citation

```         
Fadel, W. F., Urbanek, J. K., Albertson, S. R., Li, X., Chomistek, A. K., & Harezlak, J. (2019). Differentiating Between Walking and Stair Climbing Using Raw Accelerometry Data. Statistics in Biosciences, 11(2), 334–354. doi: https://doi.org/10.1007/s12561-019-09241-7

Straczkiewicz, M., Urbanek, J., Fadel, W., Crainiceanu, C., & Harezlak, J. (2017). Automatic Car Driving Detection Using Raw Accelerometry Data. Innovation in Aging, 1(suppl_1), 1239–1239. doi: 10.1093/geroni/igx004.4499

Karas, M., Bai, J., Strączkiewicz, M., Harezlak, J., Glynn, N. W., Harris, T., … Urbanek, J. K. (2019). Accelerometry Data in Health Research: Challenges and Opportunities. Review and Examples. Statistics in Biosciences, 11, 210–237. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6874221/
```
