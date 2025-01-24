
# What happened?
# - discovered that chrysene were partly given as determinand = "KRYSEN" and partly "CHR"
# - selected the rows with "KRYSEN" and replaced thevalue of 'determinand' with "CHR"
# But when rewriting the file as text, censoring values wihth no value 

fn <- "data/full_OSPAR_2023/raw_data.csv"
df <- readr::read_csv(fn)

xtabs(~station_code + determinand, 
      subset(df, 
             determinand %in% c("CHR", "BAP") & 
               station_code %in% c("4954", "5022")))

xtabs(~station_code + determinand, 
      subset(biota_data_1$data, 
             determinand %in% c("CHR", "BAP") & 
               station_code %in% c("4954", "5022")))

xtabs(~station_code + determinand, 
      subset(biota_data$data, 
             determinand %in% c("CHR", "BAP") & 
               station_code %in% c("4954", "5022")))

xtabs(~station_code + determinand, 
      subset(biota_timeseries$data, 
             determinand %in% c("CHR", "BAP") & 
               station_code %in% c("4954", "5022")))

# by year

xtabs(~year + determinand, 
      subset(biota_data$data, 
             determinand %in% c("CHR", "BAP") & 
               station_code %in% c("5030") & year >= 2016))
# determinand
# year   BAP CHR
# 2016   3   3
# 2017   3   3
# 2018   3   3
# 2019   3   3
# 2020   3   3
# 2021   3   3
# 2022   3   3
# 2023   3   3

xtabs(~addNA(censoring) + determinand, 
      subset(biota_data$data, 
             determinand %in% c("CHR", "BAP") & 
               station_code %in% c("5030") & year >= 2016))

#              determinand
# addNA(censoring) BAP CHR
#                   18  24
#             Q      6   0
#             <NA>   0   0

# 
# after create_timeseries, without fixing 'censoring' -----------------------------------
#

xtabs(~year + determinand, 
      subset(biota_timeseries$data, 
             determinand %in% c("CHR", "BAP") & 
               station_code %in% c("5030") & year >= 2016))

# determinand
# year   BAP
# 2016   3
# 2019   3

# 
# after create_timeseries, and fixing 'censoring' -----------------------------------
# changing censoring = "NA" to censoring = ""
# 
# (biota_timeseries2)

xtabs(~year + determinand, 
      subset(biota_timeseries2$data, 
             determinand %in% c("CHR", "BAP") & 
               station_code %in% c("5030") & year >= 2016))

#    determinand
# year   BAP CHR
# 2016   3   3
# 2017   3   3
# 2018   3   3
# 2019   3   3
# 2020   3   3
# 2021   3   3
# 2022   3   3
# 2023   3   3


xtabs(~year + determinand, 
      subset(biota_timeseries$data, 
             determinand %in% c("CHR", "BAP") & 
               station_code %in% c("5030")))



df1 <- subset(biota_data$data, 
       determinand %in% c("CHR", "BAP") & 
         station_code %in% c("5030") & year == 2010)

View(df1)

# during debug
xtabs(~year + determinand, 
      subset(data, 
             determinand %in% c("CHR", "BAP") & 
               station_code %in% c("5030") & year >= 2016))


param <- "CHR"
param <- "BAP"

#
# Raw csv -------------------------------------------------------------------
#
df_summ <- df %>%
  filter(determinand == param & is.na(censoring)) %>%
  distinct(year, station_name) %>%
  count(station_name)

df_sel <- df %>%
  filter(determinand == param) %>%
  filter(station_name %in% subset(df_summ, n >= 5)$station_name)

ggplot(df_sel, aes(year, value, shape = is.na(censoring), color = is.na(censoring))) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(vars(station_name))

# by station_code instead
ggplot(df_sel, aes(year, value, shape = is.na(censoring), color = is.na(censoring))) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(vars(station_code))

#
# After reading (biota_data) ---------------------------------------------------
#

df2 <- biota_data_1$data

df2_sel <- df2 %>%
  filter(determinand == param) %>%
  filter(station_code %in% unique(df_sel$station_code))
ggplot(df2_sel, aes(year, value, shape = censoring, color = censoring)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(vars(station_code))

xtabs(~addNA(censoring) + station_code, df2_sel)

     4954 5022 5030 5031
NA     40   18   69   14
Q      49   68   20   72
<NA>    0    0    0    0

xtabs(~addNA(censoring) + station_code, df2_sel)
# FALSE

#
# After tidying (also biota_data! ) ---------------------------------------------------
#

df3 <- biota_data$data

df3_sel <- df3 %>%
  filter(determinand == param) %>%
  filter(station_code %in% unique(df_sel$station_code))
ggplot(df3_sel, aes(year, value, shape = censoring, color = censoring)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(vars(station_code))

xtabs(~addNA(censoring), df3_sel)

#
# After creating time series  (biota_timeseries  ) ---------------------------------------------------
#

df4 <- biota_timeseries$data

df4_sel <- df4 %>%
  filter(determinand == param) %>%
  filter(station_code %in% unique(df_sel$station_code))
ggplot(df4_sel, aes(year, value, shape = censoring, color = censoring)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(vars(station_code))

xtabs(~addNA(censoring), df4_sel)






# View(biota_timeseries$stations)
# 4954 = 30A
str(biota_timeseries, 1)
param <- "CHR"
sel <- biota_timeseries$timeSeries$determinand == param
sum(sel)
sel2 <- sel & biota_timeseries$timeSeries$station_code == 4954
sum(sel2)
View(biota_timeseries$data)

df %>%
  filter(determinand == param & sta) %>%
  filter(station_name %in% subset(df_summ, n >= 5)$station_name) %>%
  ggplot(aes(year, value, shape = is.na(censoring), color = is.na(censoring))) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(vars(station_name))
