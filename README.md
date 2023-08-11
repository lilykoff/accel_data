
<!-- README.md is generated from README.Rmd. Please edit that file -->

# accel_data

<!-- badges: start -->
<!-- badges: end -->

The goal of `accel_data` is to provide code to download and organize
open source accelerometry data.

## IU Walking Data

In `code/R`, there are files to analyze data from the IU walking study
(<https://physionet.org/content/accelerometry-walk-climb-drive/1.0.0/>).
The data were provided by Goldberger et al [^1]. All functions should be
prefixed by `iu_*`.

To cite this data please use the Physionet citation[^2].

## ZJU Walking Data

In `code/R`, there are files to analyze data from the ZJU Gait-Acc
study. The data were provided by Zhang et al[^3]. All functions should
be prefixed by `zju_*`.

### Known issues

The file located in `session_1/subj_039/rec_5/5.txt` seems to be
corrupted. The code has a hard-coded section to remove that file from
the main data.

## References

[^1]: Goldberger, A., Amaral, L., Glass, L., Hausdorff, J., Ivanov, P.
    C., Mark, R., … & Stanley, H. E. (2000). PhysioBank, PhysioToolkit,
    and PhysioNet: Components of a new research resource for complex
    physiologic signals. Circulation \[Online\]. 101 (23),
    pp. e215–e220.

[^2]: Karas, M., Urbanek, J., Crainiceanu, C., Harezlak, J., & Fadel, W.
    (2021). Labeled raw accelerometry data captured during walking,
    stair climbing and driving (version 1.0.0). PhysioNet.
    <https://doi.org/10.13026/51h0-a262>.

[^3]: Yuting Zhang, Gang Pan, Kui Jia, Minlong Lu, Yueming Wang, Zhaohui
    Wu, “Accelerometer-based Gait Recognition by Sparse Representation
    of Signature Points with Clusters”, IEEE Transactions on
    Cybernetics, vol. 45, no. 9, pp. 1864-1875, September 2015.
