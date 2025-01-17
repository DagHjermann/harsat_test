# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# 
# This is a script which performs HARSAT analysis of four time series in cod and blue mussel
# from the inner Oslo fjord. It goes through all steps of HARSAT, and takes only a 
# few minutes to run.
#
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


# 1. Packages -------------------------------------------

# The next lines are different options for installing HARSAT:
# install.packages(c("glue", "cli", "rlang", "utf8", "fansi", "Rcpp", "stringi", "digest", "purrr"))
# devtools::install("../harsat")
# remotes::install_github("osparcomm/HARSAT@main")
# remotes::install_local("~/Downloads/harsat_0.1.2.tar")

# help(package = "")

library(harsat)
library(dplyr)
library(readr)

#
# 2. Make subset data and station files ----
#
# For testing purposes  
# NOTE: not needed - these files are already made (therefor you can proceed to 3)  
#

# Read big file
# dat_all <- read.csv("data/example_OSPAR/biota.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
# xtabs(~country, dat_all)
# xtabs(~statn, dat_all %>% filter(country == "Norway"))
# xtabs(~statn, dat_all %>% filter(grepl("30", statn)))

# # Creating subset 1
# dat_sel <- dat_all %>% filter(grepl("30A Gressholmen", statn))
# xtabs(~param, dat_sel)
# write.table(dat_sel, "data/example_OSPAR/biota_30A_Gressholmen.txt", sep = "\t", row.names = FALSE)

# Creating subset 2
# dat_sel <- dat_all %>%
#   filter(statn %in% c("30A Gressholmen", "30B Oslo City area"),
#          param %in% c("ACNE", "ALAD", "DDEPP", "DRYWT%", "EXLIP%", "PFOS"))
# xtabs(~statn + param, dat_sel)
# write.table(dat_sel, "data/example_OSPAR/biota_30A_30B_Oslo.txt", sep = "\t", row.names = FALSE)

# # Creating subset of stations file
# readLines("data/example_OSPAR/stations.txt", 2)
# station_dictionary <- read_delim("data/example_OSPAR/stations.txt", guess_max = 13000)
# sel <- station_dictionary$station_name %in% c("30A Gressholmen", "30B Oslo City area")
# sum(sel)   # 6 rows
# station_dictionary_sel <- station_dictionary[sel,] %>% 
#   filter(is.na(station_activeuntildate)) %>%
#   select(-station_geometry)
# xtabs(~station_name + station_code, station_dictionary_sel)
# write_delim(station_dictionary_sel, "data/example_OSPAR/stations_30A_30B.txt", delim = "\t")
# readLines("data/example_OSPAR/stations_30A_30B.txt")

#
# 3. Read data --------------------------------------------
#

biota_data <- read_data(
  compartment = "biota", 
  purpose = "OSPAR",                               
  contaminants = "biota_30A_30B_Oslo.txt", 
  stations = "stations_30A_30B.txt", 
  data_dir = file.path("data", "example_OSPAR"),         ## i.e., C:\Users\test\ospar\data
  info_dir = file.path("information", "OSPAR_2022"),  ## i.e., C:\Users\test\ospar\information
  extraction = "2023/08/23"
)
# this creates some warnings, such as 
#   'not all columns named in 'colClasses' exist'
# and 
#   'NAs introduced by coercion' 
# which can be ignored in this case

# check which data that 'survived':
xtabs(~station_submitted + determinand, biota_data$data)

# View data:
# View(biota_data$data)


#
# 4. Tidy data ----------------------------------------
#

biota_data <- tidy_data(biota_data)

# all the data 'survived':
xtabs(~station_code + determinand, biota_data$data)

#
# 5. Construct timeseries ---------------------------------
#
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
# Note that only 4 time series were made, as DRYWT% and EXLIP% are not contaminants,
#   but may be needed for converting to different bases.
biota_timeseries$timeSeries

# 'data' is the raw data:
str(biota_timeseries$data, 1)

# ... 'stations' is still the list of stations:
biota_timeseries$stations

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

biota_assessment <- run_assessment(
  biota_timeseries,
  subset = NULL,
  AC = NULL,
  get_AC_fn = NULL,
  recent_trend = 20L,
  parallel = FALSE, 
  extra_data = NULL,
  control = list(power = list(target_power = 80, target_trend = 10)) 
)


# . check assessment result object -------------------------------

# 'biota_assessment' is a list of six items, where most are
#    the same as 'biota_timeseries':
str(biota_assessment, 1)

# the new item is 'assessment', which is a list with one item
#   per time series:
str(biota_assessment$assessment, 1)

# Note: list items can be referred to either with their number:
#   str(biota_assessment$assessment[[1]], 1)
# or their name:
#   str(biota_assessment$assessment[["4684 ALAD Gadus morhua BL]], 1)

# let's check time series number 1 - it is also a list, 
#   with 13 items in this case
str(biota_assessment$assessment[[1]], 1)

# We will look into some of them.
# 'fullData' and 'data' are the raw data, including the 'cencoring' column (<LOQ or not)
# 'data' are the data that goes into trend analysis 
str(biota_assessment$assessment[[1]]$fullData, 1)
str(biota_assessment$assessment[[1]]$data, 1)

# 'annualIndex' is the annual observed values. Note: numbers are on log scale   
biota_assessment$assessment[[1]]$annualIndex

# 'pred' is the trend line. Note: numbers are on log scale  
str(biota_assessment$assessment[[1]]$pred, 1)

# 'anova' is the fit of the different time series models that has been tried  
biota_assessment$assessment[[1]]$anova

# 'contrasts' are the two tests for whether concentrations have gone up 
#   or down over the whole period or recent period. Used only if the 
#   best trend is a non-linear trend. Otherwise one looks at whether the
#   linear trend is significant or not
biota_assessment$assessment[[1]]$contrasts

#
# Little test plot:
ggplot(biota_assessment$assessment[[1]]$annualIndex, 
       aes(year, exp(index), shape = censoring)) +
  geom_point()

# Simple plot of one series
library(ggplot2)
series <- "4954 DDEPP Mytilus edulis SB"
ggplot(
  data = biota_assessment$assessment[[series]]$pred, aes(x = year)) +  
  # trend line, confidence interval (plotted first so it is the bottom layer of the plot):
  geom_ribbon(aes(ymin = exp(ci.lower), ymax = exp(ci.upper)), fill = "lightblue") +
  # trend line, the line itself:
  geom_line(aes(y = exp(fit))) +
  # data (different data set, so we must specify 'data'):
  geom_point(
    data = biota_assessment$assessment[[series]]$data,
    aes(y = concentration, shape = censoring)
  )



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


