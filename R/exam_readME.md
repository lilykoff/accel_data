---
editor_options: 
  markdown: 
    wrap: 72
---

# Exam (Wearable Exam Stress Dataset)

## Dataset summary

-   Number of subjects: 10 subjects (8 male) during 3 exam sessions each
    (midterm 1, midterm 2, final)

-   Device: Empatica E4 wristband

-   Sample rate: 32 Hz

-   Device location: wrist

## Experimental setup

Subjects were required to wear the FDA-approved Empatica E4 wristband
while they take their midterm (Exam 1 and 2) and final exams. The E4
recorded skin conductance, heart rate, body temperature and movement
(accelerometer).

Each E4 device has a tag number. On coming to the exam, each participant
picked up an E4 device and wrote their name on the document
corresponding to the E4 device they selected. After data collection the
participants returned their E4 wristbands to the study team. Finally,
the course instructor provided the other members of the study team with
the grades corresponding to the device number.

## Activities

1.  Midterm exams 1 and 2 (90 min each)

2.  Final exam (180 min)

## Data structure

`csv` files for various metrics collected from the wristbands are
located in subject-specific and exam-specific folders. The first two
rows of each `csv` list the start time of the activity and sample rate,
respectively

## Raw

`accel_data/raw/a-wearable-exam-stress-dataset-for-predicting-cognitive-performance-in-real-world-settings-1.0.0/Data`

## Processed

`accel_data/processed/exam.csv.gz`

Column names:

-   `id_subject` subject ID, one of "S1-Final", ... "S10-Final",
    "S1-Midterm 1", ..., "S10-Midterm 1", "S1 Midterm 2", ... "S10
    Midterm 2"

-   `id_study` : `exam`

-   `cat_activity` : `take_exam`

-   `tm_dttm` : time given for each recording based on start time and
    sample rate

-   `X`, `Y`, `Z`: acceleration in X, Y, and Z directions respectively,
    in `g`

## Citation

```         
When using this resource, please cite: (show more options)
Amin, M. R., Wickramasuriya, D., & Faghih, R. T. (2022). A Wearable Exam Stress Dataset for Predicting Cognitive Performance in Real-World Settings (version 1.0.0). PhysioNet. https://doi.org/10.13026/kvkb-aj90.

Amin, Md. Rafiul, Dilranjan S. Wickramasuriya, and Rose T. Faghih. "A Wearable Exam Stress Dataset for Predicting Grades using Physiological Signals." 2022 IEEE Healthcare Innovations and Point of Care Technologies (HI-POCT). IEEE, 2022.

Goldberger, A., Amaral, L., Glass, L., Hausdorff, J., Ivanov, P. C., Mark, R., ... & Stanley, H. E. (2000). PhysioBank, PhysioToolkit, and PhysioNet: Components of a new research resource for complex physiologic signals. Circulation [Online]. 101 (23), pp. e215–e220.
```
