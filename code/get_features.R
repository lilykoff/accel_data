# file to calculate sample features

temp = ped_data_proc %>% filter(id_subject == "001", cat_activity == "walk_regular")
window_size = 1  # decide which size window to use

temp = temp %>% mutate(
  vm = sqrt(X^2 + Y^2 + Z^2)
)

get_peak_dat <- function(vm){
  res = pracma::findpeaks(vm, nups = 2)
  if(is.null(res)){
    num_peaks = sd_peaks = mean_peaks = 0
  }
  else{
    num_peaks = nrow(res)
    mean_peaks = mean(res[, 1])
    sd_peaks = ifelse(nrow(res) > 1, sd(res[, 1]), 0)}
  return(list(num_peaks = num_peaks, sd_peaks = sd_peaks, mean_peaks = mean_peaks))
}


get_crossings <- function(vm, thresh){
  length(vm[vm>thresh])
}

autocor <- function(vm, lag){
  res = acf(vm, lag.max = lag, pl = FALSE)
  res$acf[[lag+1]]
}

get_peak_dat(temp$vm[1:100])
 pracma::findpeaks(temp$vm[1:15], nups = 2)

 plot(temp$vm[1:15], type = "l")
 acf(temp$vm, lag.max = 16)


temp_summ = temp %>%
  group_by(second = floor_date(tm_dttm, unit = "seconds")) %>%
  mutate(n=n()) %>%
  filter(n==15) %>%
  summarize(
    sd = sd(vm),
    mean = mean(vm),
    max = max(vm),
    min = min(vm),
    mad = mad(vm),
    npeak = get_peak_dat(vm)$num_peaks,
    meanpeak = get_peak_dat(vm)$mean_peaks,
    sdpeak = get_peak_dat(vm)$sd_peaks,
    thresh = get_crossings(vm, 1.5),
    ac1 = autocor(vm, 1),
    ac5 = autocor(vm, 5),
    ac10 = autocor(vm, 10)
  )
res = acf(temp$vm, lag = 2)
# fft and cwt next
