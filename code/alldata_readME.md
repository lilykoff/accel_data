## Accelerometry Data

Collated from various sources

Raw data in `accel_data/raw`

Processed data in `accel_data/processed`

Resampled at 15Hz data in `accel_data/processed/<datasetname>_resampled.csv.gz`

Code for downloading and processing in `accel_data/code/<datasetname>_download_process.R`

Individual readME for each dataset in `accel_data/code/<datsetname>_readME.md`

Code for resampling all data to common sample rate in `accel_data/code/run_resamples.R`

Helper functions for resampling in `accel_data/code/resample_helpers.R`

## Data sources

-   [ZJU-GaitAcc](https://www.ytzhang.net/datasets/zju-gaitacc/#citation)

    -   `procesed/zju.csv.gz`

    -   Walking data only, 175 subjects

    -   100 Hz

-   [IU Labeled raw accelerometry data captured during walking, stair climbing and driving](https://physionet.org/content/accelerometry-walk-climb-drive/1.0.0/)

    -   `processed/iu.csv.gz`

    -   Walking, driving, stair climbing/descending, driving, 32 subjects

    -   100 Hz

-   [MHEALTH](https://archive.ics.uci.edu/dataset/319/mhealth+dataset)

    -   `processed/mhealth.csv.gz`

    -   Various activities, 10 subjects

    -   50 Hz

-   [SFDLA Simulated Falls and Daily Living Activities Data Set](https://archive.ics.uci.edu/dataset/455/simulated+falls+and+daily+living+activities+data+set)

    -   `processed/sfdla.csv.gz`

    -   Simulated falls and other activities, 17 subjects

    -   25 Hz

-   [RealWorld Dataset](https://www.uni-mannheim.de/dws/research/projects/activity-recognition/dataset/dataset-realworld/)

    -   `processed/realworld.csv.gz`

    -   Walking, Running, Sitting, Standing, lying, Stairs Up, Stairs Down, Jumping, 15 subjects

    -   50 Hz

-   [HMP Human Motion Primitives](https://github.com/wchill/HMP_Dataset)

    -   `processed/hmp.csv.gz`

    -   Human motion primitive activities, 16 subjects

    -   32 Hz

-   [Wrist PPG during exercise](https://physionet.org/content/wrist/1.0.0/)

    -   `processed/ppg_wide_range.csv.gz`

    -   `processed/ppg_low_noise.csv.gz`

    -   Walking, running, bike riding, 8 subjects

    -   256 Hz

-   [Exam - A Wearable Exam Stress Dataset for Predicting Cognitive Performance in Real-World Settings](https://physionet.org/content/wearable-exam-stress/1.0.0/)

    -   `processed/exam.csv.gz`

    -   Exam taking, 10 subjects

    -   32 Hz

-   [Clemson Pedometer Evaluation](https://cecas.clemson.edu/~ahoover/pedometer/)

    -   `processed/clemson_ped.csv.gz`

    -   Regular, semi-regular, and irregular, walking, 30 subjects

    -   15 Hz

-   [Daily Life Activities (DaLiAc)](https://www.mad.tf.fau.de/research/activitynet/daliac-daily-life-activities/)

    -   `processed/daliac.csv.gz`

    -   Sitting, lying, standing, washing dishes, vacuuming, running, sweeping, walking (outside), up and down stairs, cycling, jumping rope, 19 subjects

    -   204.8 Hz

-   [Movement Analysis in Real-World Environments Using Accelerometers (MAREA)](https://wiki.hh.se/caisr/index.php/Gait_database)

    -   `processed/marea.csv.gz`

    -   Treadmill walking/running, indoor walking/running, outdoor walking/running, 20 subjects

    -   128 Hz

## Data structure

Each of the files in the `processed` folder have the following columns (at a minimum, some have other columns as well which are described in the individual data readme for that dataset)

-   `id_subject` the study-specific subject ID

-   `id_study` the study id, matches the filename

-   `tm_dttm` the timestamp of the row in POSIXct, UTC time zone

-   `cat_activity` the activity being performed at the time

-   `X`, `Y`, `Z` acceleration in g in the x, y, and z directions respectively
