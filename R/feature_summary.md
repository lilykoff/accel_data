Let $a_x$ denote acceleration in $x$ direction

Let $|a|$ denote magnitude of acceleration

Let $a$ denote sums of squared acceleration

Let $f$ denote FFT

+--------------+------------------------------------------------------------------------+
| Paper        | Features                                                               |
+==============+========================================================================+
| Vandermeeren | s.d. $sd(a_x)$                                                         |
|              |                                                                        |
|              | mean abs. deviation $mad(a_x)$                                         |
|              |                                                                        |
|              | number samples of $|a|$ above threshold                                |
|              |                                                                        |
|              | number maxima in $|a|$                                                 |
|              |                                                                        |
|              | mean of maxima in $|a|$                                                |
|              |                                                                        |
|              | s.d. of maxima(peaks) in $|a|$                                         |
|              |                                                                        |
|              | frequency at max $f$ of $a_y$ and $a_z$                                |
|              |                                                                        |
|              | max $f$ of $a_z$                                                       |
|              |                                                                        |
|              | sd, skewness, and kurtosis of $f(a_z)$                                 |
|              |                                                                        |
|              | energe of $f(a_z)$ between 1 and 5Hz                                   |
|              |                                                                        |
|              | Energy of $f(a_z)$ and $f(|a|)$                                        |
|              |                                                                        |
|              | signal magnitude area of FFT of acceleration                           |
+--------------+------------------------------------------------------------------------+
| Preece       | SMA (area under high pass fitered acceleration curve)                  |
|              |                                                                        |
|              | Median frequency from FFT                                              |
|              |                                                                        |
|              | Subset of FFT coefficients                                             |
|              |                                                                        |
|              | Spectral energy                                                        |
|              |                                                                        |
|              | Frequency domain entropy                                               |
|              |                                                                        |
|              | Wavelet coefficients - sums of squares or RMS of specific coefficients |
+--------------+------------------------------------------------------------------------+
| Luu, Zou     | Neural network on raw acceleration data                                |
+--------------+------------------------------------------------------------------------+

Options: dimension reduction on the features, or feature selection

Time frequency domain (wavelet analysis)

Frequency domain (FFT)

Time domain

Autocorrelation distribution
