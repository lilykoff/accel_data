options(digits.secs = 3)
sample_rate = 15
clemson = readr::read_csv("data/processed/clemson_ped.csv.gz")
clemson %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(n = n()) %>%
  mutate(num_minutes = (n / sample_rate)/60) %>%
  select(-n)

set.seed(123)
clemson_samp <-
  clemson %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(start = sample(tm_dttm, 1)) %>%
  mutate(end = start + lubridate::as.period(10, "seconds")) %>%
  right_join(clemson) %>%
  filter(between(tm_dttm, start, end)) %>%
  select(-c(start, end)) %>%
  ungroup()

iu = readr::read_csv("data/processed/iu.csv.gz")
sample_rate = 100
iu %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(n = n()) %>%
  mutate(num_minutes = (n / sample_rate)/60) %>%
  select(-n)

iu_samp <-
  iu %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(start = sample(tm_dttm, 1)) %>%
  mutate(end = start + lubridate::as.period(10, "seconds")) %>%
  right_join(iu) %>%
  filter(between(tm_dttm, start, end)) %>%
  select(-c(start, end)) %>%
  ungroup()

exam = readr::read_csv("data/processed/exam.csv.gz")
exam = exam %>% mutate(id_study = "exam")
sample_rate = 32
exam %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(n = n()) %>%
  mutate(num_minutes = (n / sample_rate)/60) %>%
  select(-n)

exam_samp <-
  exam %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(start = sample(tm_dttm, 1)) %>%
  mutate(end = start + lubridate::as.period(10, "seconds")) %>%
  right_join(exam) %>%
  filter(between(tm_dttm, start, end)) %>%
  select(-c(start, end)) %>%
  ungroup()


hmp = readr::read_csv("data/processed/hmp.csv.gz")
sample_rate = 32
hmp %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(n = n()) %>%
  mutate(num_minutes = (n / sample_rate)/60) %>%
  select(-n)

hmp_samp <-
  hmp %>%
  group_by(id_subject, cat_activity, id_study, id_repetition) %>%
  summarize(start = sample(tm_dttm, 1)) %>%
  mutate(end = start + lubridate::as.period(10, "seconds")) %>%
  right_join(hmp) %>%
  filter(between(tm_dttm, start, end)) %>%
  select(-c(start, end)) %>%
  ungroup()



mhealth = readr::read_csv("data/processed/mhealth.csv.gz")
mhealth = mhealth %>% mutate(id_subject = as.character(id_subject))
sample_rate = 50
mhealth %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(n = n()) %>%
  mutate(num_minutes = (n / sample_rate)/60) %>%
  select(-n)

mhealth_samp <-
  mhealth %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(start = sample(tm_dttm, 1)) %>%
  mutate(end = start + lubridate::as.period(10, "seconds")) %>%
  right_join(mhealth) %>%
  filter(between(tm_dttm, start, end)) %>%
  select(-c(start, end)) %>%
  ungroup()


ppg_l = readr::read_csv("data/processed/ppg_low_noise.csv.gz")
sample_rate = 256
ppg_l  %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(n = n()) %>%
  mutate(num_minutes = (n / sample_rate)/60) %>%
  select(-n)

ppg_l_samp <-
  ppg_l %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(start = sample(tm_dttm, 1)) %>%
  mutate(end = start + lubridate::as.period(10, "seconds")) %>%
  right_join(ppg_l) %>%
  filter(between(tm_dttm, start, end)) %>%
  select(-c(start, end)) %>%
  ungroup()

ppg_w = readr::read_csv("data/processed/ppg_wide_range.csv.gz")
sample_rate = 256
ppg_w  %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(n = n()) %>%
  mutate(num_minutes = (n / sample_rate)/60) %>%
  select(-n)

ppg_w_samp <-
  ppg_w %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(start = sample(tm_dttm, 1)) %>%
  mutate(end = start + lubridate::as.period(10, "seconds")) %>%
  right_join(ppg_w) %>%
  filter(between(tm_dttm, start, end)) %>%
  select(-c(start, end)) %>%
  ungroup()


rw = readr::read_csv("data/processed/realworld.csv.gz")
sample_rate = 50
rw %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(n = n()) %>%
  mutate(num_minutes = (n / sample_rate)/60) %>%
  select(-n)

rw_samp <-
  rw %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(start = sample(tm_dttm, 1)) %>%
  mutate(end = start + lubridate::as.period(10, "seconds")) %>%
  right_join(rw) %>%
  filter(between(tm_dttm, start, end)) %>%
  select(-c(start, end)) %>%
  ungroup()



sfdla = readr::read_csv("data/processed/sfdla.csv.gz")
sfdla = sfdla %>%
  mutate(id_repetition = as.character(id_repetition),
         id_subject = as.character(id_subject),
         cat_activity_orig = as.character(cat_activity_orig))
sample_rate = 25
sfdla %>%
  group_by(id_subject, cat_activity, id_study) %>%
  summarize(n = n()) %>%
  mutate(num_minutes = (n / sample_rate)/60) %>%
  select(-n)

sfdla_samp <-
  sfdla %>%
  group_by(id_subject, cat_activity_orig, id_study, id_repetition) %>%
  summarize(start = sample(tm_dttm, 1)) %>%
  mutate(end = start + lubridate::as.period(10, "seconds")) %>%
  right_join(sfdla) %>%
  filter(between(tm_dttm, start, end)) %>%
  select(-c(start, end)) %>%
  ungroup()


zju = readr::read_csv("data/processed/zju.csv.gz")
# zju = zju %>% mutate(id_session = as.character(id_session), id_recording = as.character(id_recording))

sample_rate = 100
zju %>%
  group_by(id_subject, id_session, cat_activity, id_study) %>%
  summarize(n = n()) %>%
  mutate(num_minutes = (n / sample_rate)/60) %>%
  select(-n)
zju_small = zju %>% filter(id_recording == "2")
zju_samp <-
  zju %>%
  filter(id_recording == "2") %>%
  group_by(id_subject, cat_activity, id_study, id_session) %>%
  summarize(start = sample(tm_dttm, 1)) %>%
  mutate(end = start + lubridate::as.period(10, "seconds")) %>%
  right_join(zju_small) %>%
  filter(between(tm_dttm, start, end)) %>%
  select(-c(start, end)) %>%
  ungroup()

# for eda, get 10s of data from each id, activity

all_samples =
  bind_rows(
    sfdla_samp, rw_samp, zju_samp, iu_samp,
    hmp_samp, clemson_samp, mhealth_samp,
    exam_samp, ppg_l_samp, ppg_w_samp
  )
