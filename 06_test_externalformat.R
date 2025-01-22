

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# 
# This is a script which performs HARSAT analysis of four time series in cod and blue mussel
# from the inner Oslo fjord. It goes through all steps of HARSAT, and takes only a 
# few minutes to run.
#
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


# 1. Packages -------------------------------------------

library(harsat)
library(dplyr)
library(readr)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#
# 2. Make subset data and station files ----
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# NOTE: Part 2 needs only to be done once!
# Therefore commented out

# 2a. copy Milkys data from project 'milkys4'  
# file.copy("../milkys4/harsat_data/raw_data.csv", "data/example_OSPAR/milkys4_raw_data.csv")
# file.copy("../milkys4/harsat_data/ICES_DOME_STATIONS_20230829_NO.csv", "data/example_OSPAR/ICES_DOME_STATIONS_20230829_NO.csv")

# raw_data <- read_csv("data/example_OSPAR/milkys4_raw_data.csv")
# 
# # 2b. Replace ICES station codes with our own
# stations_original_code <- raw_data %>% 
#   distinct(station_code, station_name)
# stations_new_code <- stations_original_code %>%
#   mutate(station_code = stringr::str_extract(station_name, "([^[[:blank:]]]+)"))
# 
# # check that 'stations_new_code' is OK
# check <- stations_new_code %>%
#   count(station_name) %>%
#   filter(n > 1)
# if (nrow(check) > 0) {
#   stop("More than one station_code per station_name. Inspect 'check'.")}
# 
# # 2c. make new raw data file
# raw_data_new <- raw_data %>%
#   select(-station_code) %>%
#   left_join(stations_new_code, by = join_by(station_name))
# 
# write_csv(raw_data_new, "data/example_external/rawdata_test.csv", 
#           # IMPORTANT (otherwise only <LOQ data will be kept):
#           na = "")
# 
# # 2d. make new station file
# 
# station_orig <- read_csv("data/example_OSPAR/ICES_DOME_STATIONS_20230829_NO.csv")
# 
# station_new <- station_orig %>%
#   filter(is.na(station_activeuntildate) & !station_deprecated) %>%
#   select(country, station_name, 
#          station_latitude, station_longitude) %>%
#   left_join(stations_new_code, by = join_by(station_name)) %>%
#   filter(!is.na(station_code))
# 
# write_csv(station_new, "data/example_external/stations_test.csv", 
#           # IMPORTANT (otherwise only <LOQ data will be kept):
#           na = "")

#
# 3. Read data --------------------------------------------
#

# NOTE: data_format = "external"

biota_data <- read_data(
  compartment = "biota", 
  purpose = "AMAP", 
  contaminants = "rawdata_test.csv", 
  data_format = "external", 
  stations = "stations_test.csv", 
  data_dir = file.path("data", "example_external"),         ## i.e., C:\Users\test\ospar\data
  info_dir = file.path("information", "OSPAR_2022"),  ## i.e., C:\Users\test\ospar\information
  extraction = "2023/08/23"
)

# check which data that 'survived':
# xtabs(~station_name, biota_data$data)
# xtabs(~determinand, biota_data$data)

# View data:
# View(biota_data$data)


#
# 4. Tidy data ----------------------------------------
#

biota_data <- tidy_data(biota_data)

#
# 5. Construct timeseries ---------------------------------
#

# Code ide
# For each timeseries, use the basis which is reported most often in the data

biota_timeseries <- create_timeseries(
  biota_data,
  determinands = ctsm_get_determinands(biota_data$info),
  determinands.control = NULL,
  oddity_path = "oddities",
  return_early = FALSE,
  print_code_warnings = FALSE,
  get_basis = get_basis_most_common,
  normalise = FALSE,
  normalise.control = list()
)

# Object resulting from this is a list with 6 items:
str(biota_timeseries, 1)

# ...including 'timeSeries' which is a data frame with one row per time series.
# Number of time series:
nrow(biota_timeseries$timeSeries)
# Show only the top of this file:
head(biota_timeseries$timeSeries)

# 'data' is the raw data:
str(biota_timeseries$data, 1)

# ... 'stations' is still the list of stations, but filtered:
head(biota_timeseries$stations)

# ... and 'info' which is a list of different stuff, including 
# ... 'determinand' (the substances)
str(biota_timeseries$info$determinand, 1)
# ... and 'species' which contains values for converting between wet-weight and dry-weight etc.
str(biota_timeseries$info$species, 1)

# check them out using 
# View(biota_timeseries$info$determinand)
# View(biota_timeseries$info$species)


#
# 6. Assessment -------------------------------------
#
# Runs the time series analyses itself
#

# We will run just a few time series by specifying "subset" when we run "run_assessment"  
# We do this by making a boolean variable (a "TRUE/FALSE" variable) based on "biota_timeseries$timeSeries"

# 1. find station codes for selected stations 
selected_station_name <- c("10A2 Skallneset", "10B Varangerfjorden")  # 'station_name' in biota_timeseries$stations
selected_station_code <- biota_timeseries$stations %>%
  filter(station_name %in% selected_station_name) %>%
  pull(station_code)

# 2. select time series for guiven substances and stations  
timeseries_selected <- biota_timeseries$timeSeries$determinand %in% c("CB118", "DDEPP") & 
  biota_timeseries$timeSeries$station_code %in% selected_station_code

# check
length(timeseries_selected)  # expected to be the same as the total number of time series
sum(timeseries_selected)     # the number of time series selected 

# this will take 2-3 minutes
biota_assessment <- run_assessment(
  biota_timeseries,
  subset = timeseries_selected,
  AC = NULL,
  get_AC_fn = NULL,
  recent_trend = 20L,
  parallel = FALSE, 
  extra_data = NULL
)


# . check assessment result object -------------------------------

# 'biota_assessment' is a list of six items, where most are
#    the same as 'biota_timeseries':
str(biota_assessment, 1)

# the new item is 'assessment', which is a list with one item
#   per time series
# NOTE: even if we just ran assessment for a few time series,
#   'assessment' has the same length as the total number of
#   time series, but almost all of them will be NULL
#
# example: the 3 first assessment objects are all NULL
str(biota_assessment$assessment[1:3], 1)

# ...but we can use 'timeseries_selected' that we made above to find the 
#   assessments that actually were made
str(biota_assessment$assessment[timeseries_selected], 1)

# Note: list items can be referred to either with their number:
#   str(biota_assessment$assessment[[i]], 1)
# or their name:
#   str(biota_assessment$assessment[["4684 ALAD Gadus morhua BL]], 1)

# let's find the first time series that actually was assessed:
i <- which(timeseries_selected)[1]

# which data series is this?
biota_timeseries$timeSeries[i,]

# let's check its assessment object - it is also a list, 
#   with 13 items in this case
str(biota_assessment$assessment[[i]], 1)

# We will look into some of them.
# 'fullData' and 'data' are the raw data, including the 'cencoring' column (<LOQ or not)
# 'data' are the data that goes into trend analysis 
str(biota_assessment$assessment[[i]]$fullData, 1)
str(biota_assessment$assessment[[i]]$data, 1)

# 'annualIndex' is the annual observed values. Note: numbers are on log scale   
biota_assessment$assessment[[i]]$annualIndex

# 'pred' is the trend line. Note: numbers are on log scale  
str(biota_assessment$assessment[[i]]$pred, 1)

# 'anova' is the fit of the different time series models that has been tried  
biota_assessment$assessment[[i]]$anova

# 'contrasts' are the two tests for whether concentrations have gone up 
#   or down over the whole period or recent period. Used only if the 
#   best trend is a non-linear trend. Otherwise one looks at whether the
#   linear trend is significant or not
biota_assessment$assessment[[i]]$contrasts

#
# Simple plot of one series:
library(ggplot2)
ggplot(biota_assessment$assessment[[i]]$annualIndex, 
       aes(year, exp(index), shape = censoring)) +
  scale_shape_manual(values = c(16,6)) +
  geom_point()

# Simple plot of one series, with trend line:
series <- "10A2 CB118 Mytilus edulis SB"
gg <- ggplot(
  data = biota_assessment$assessment[[series]]$pred, aes(x = year)) +  
  # trend line, confidence interval (plotted first so it is the bottom layer of the plot):
  geom_ribbon(aes(ymin = exp(ci.lower), ymax = exp(ci.upper)), fill = "lightblue") +
  # trend line, the line itself:
  geom_line(aes(y = exp(fit))) +
  # data (different data set, so we must specify 'data'):
  geom_point(
    data = biota_assessment$assessment[[series]]$data,
    aes(y = concentration, shape = censoring)
  ) +
  scale_shape_manual(values = c(16,6))

# show plot on ordinary scale:
gg
# show plot on log scale:
gg + scale_y_log10()



#
# 7. Check convergence ---------------------------------------------
#

check_assessment(biota_assessment, save_result = FALSE)

#
# 8. Create summary table ----------------------------------------------------------------------
#

# This writes the summary data to a file in output/example_external_data. The argument 
#   extra_output = "power" ensures that the power metrics for lognormally distributed 
#   data will be exported.

# Create a folder for the summaries, if it has not already been created:
summary.dir <- file.path("output", "ospar")
if (!dir.exists(summary.dir)) {
  dir.create(summary.dir, recursive = TRUE)
} 

write_summary_table(
  biota_assessment,
  output_file = "biota-FO-PW-test-output.csv",   # NB, file will be overwritten so change name as appropriate to retain results
  output_dir = summary.dir,
  export = TRUE,
  determinandGroups = NULL,
  symbology = NULL,
  collapse_AC = NULL, 
  extra_output = "power"
)

res <- read.csv(paste0(summary.dir, "/biota-FO-PW-test-output.csv"))

#
# 9. Create plot files ---------------------------------------------------------------------------
#

# Create a folder for the plots, if it has not already been created:
plot.dir <- file.path("plots", "ospar", "biota")
if (!dir.exists(plot.dir)) {
  dir.create(plot.dir, recursive = TRUE)
} 

plot_assessment(
  biota_assessment,
  output_dir = plot.dir,
  file_type = c("data", "index"),
  file_format = "png"
)

#
# 10. Create reports ---------------------------------------------------------------------------
#

report.dir <- file.path("reports", "milkys")
if (!dir.exists(report.dir)) {
  dir.create(report.dir, recursive = TRUE)
} 

report_assessment(
  biota_assessment,
  output_dir = report.dir
)
# note: you can use "subset = " to make only some of the reports 


#
# 11. APPENDIX. Test contents of station file -------------------------------------
#

# loads the 'test_assessment' function
source("06_test_externalformat_functions.R")


# - o - o - o - o - o - o - o - o - o - o - o - o - o - o - o 
#
# .  change raw data ----
#
# - o - o - o - o - o - o - o - o - o - o - o - o - o - o - o 

# create smaller rawdata files for testing
#

read_csv("data/example_OSPAR/milkys4_raw_data.csv") %>% 
  filter(station_name %in% "10A2 Skallneset") %>%              # str()
  write_csv("data/example_OSPAR/rawdata_test.csv", 
            # IMPORTANT (otherwise only <LOQ data will be kept):
            na = "")

check_data <- read_csv("data/example_OSPAR/rawdata_test.csv")

read_csv("data/example_OSPAR/milkys4_raw_data.csv") %>% 
  filter(station_name %in% "10B Varangerfjorden") %>%              # str()
  write_csv("data/example_OSPAR/rawdata_test_cod.csv", 
            # IMPORTANT:
            na = "")


# debugonce(test_assessment)
test <- test_assessment(filename_rawdata = "rawdata_test.csv",
                        filename_stations = "ICES_DOME_STATIONS_20230829_NO.csv", 
                        selected_station_names = "10A2 Skallneset",
                        selected_params = "CB118")
test <- test_assessment(filename_rawdata = "rawdata_test_cod.csv",
                        filename_stations = "ICES_DOME_STATIONS_20230829_NO.csv", 
                        selected_params = "CB118")

# - o - o - o - o - o - o - o - o - o - o - o - o - o - o - o 
#
# .  change station data ----
#
# - o - o - o - o - o - o - o - o - o - o - o - o - o - o - o 

# stations_test <- read_csv("data/example_OSPAR/ICES_DOME_STATIONS_20230829_NO.csv")
# names(stations_test)

read_csv("data/example_OSPAR/milkys4_raw_data.csv") %>% 
  filter(station_name %in% "10A2 Skallneset") %>%  
  mutate(station_code = 1) %>%
  write_csv("data/example_OSPAR/rawdata_test_stationcode1.csv", 
            # IMPORTANT (otherwise only <LOQ data will be kept):
            na = "")

# read_csv("data/example_OSPAR/ICES_DOME_STATIONS_20230829_NO.csv") %>%
#   filter(station_name %in% "10A2 Skallneset") %>%
#   mutate(station_code = 1) %>% names() %>% paste(collapse = ", ")

read_csv("data/example_OSPAR/ICES_DOME_STATIONS_20230829_NO.csv") %>% 
  filter(station_name %in% "10A2 Skallneset") %>%  
  mutate(station_code = 1,
         station_latitude = 58, station_longitude = 0) %>% 
  select(station_code, country, station_name, 
         station_latitude, station_longitude) %>%
  write_csv("data/example_OSPAR/stationdata_test_stationcode1.csv", 
            # IMPORTANT (otherwise only <LOQ data will be kept):
            na = "")

  
read_csv("data/example_OSPAR/stationdata_test_stationcode1.csv")
  
test <- test_assessment(filename_rawdata = "rawdata_test_stationcode1.csv",
                        filename_stations = "stationdata_test_stationcode1.csv", 
                        selected_station_names = "10A2 Skallneset",
                        selected_params = "CB118")



