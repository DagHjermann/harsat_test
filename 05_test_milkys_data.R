

# Packages -------------------------------------------

# install.packages(c("glue", "cli", "rlang", "utf8", "fansi", "Rcpp", "stringi", "digest", "purrr"))
# devtools::install("../harsat")
# remotes::install_github("osparcomm/HARSAT@main")
# remotes::install_local("~/Downloads/harsat_0.1.2.tar")

# help(package = "harsat")

library(harsat)
library(dplyr)
library(readr)

#
# Prepare Norwegian data (from Milkys) ---------------------------------------------------------------------------
#

#
# . Read concentrations (dat_orig_1) ----------
#
dat_orig_1 <- readRDS("../Milkys2_pc/Files_from_Jupyterhub_2022/Raw_data/105_data_with_uncertainty_2023-09-12.rds")
names(dat_orig_1)

#
# . Read stations ----------
# ICES station dictionary

#

# Note: Need 'read_tsv' from the readr package to read this properly  
dat_stations <- readr::read_tsv("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", guess_max = 7000)

# Norwegian stations  
tab <- table(subset(dat_stations, station_country == "Norway")$station_name)
length(tab)

#
# . Lookup data for ICES station names (STATN / station_name) -------
# for joining onto concentration data  
#

# The stuff commented out below is not needed, as we have an RDS file  
# source("../Milkys2_pc//844_ICES_submission_check_functions.R")
# data_ices <- read_ices_file(fn)
# data_ices <- add_field_codes(data_ices)

# Data submitted to ICES  
# - used to make 'lookup_statn', plus to check station names etc
fn <- "../Milkys2_pc/Files_to_ICES/2022/Rdata/842_NIVA2022CF_08.rds"
data_ices <- readRDS(fn)
length(data_ices)
str(data_ices, 1)
data_91 <- data_ices[["91"]]
# table(data_91$STNNO)
# table(data_91$STATN)

# readLines("data/example_OSPAR/stations.txt", n = 2)
dat_stations <- readr::read_tsv("data/example_OSPAR/stations.txt", guess_max = 15000)

# Are all ICES station names from table 91 (which also contains NIVA codes) also found in the station dictionary.?
statn_1 <- unique(data_91$STATN) 
statn_2 <- unique(dat_stations$station_name)
check <- statn_1 %in% statn_2

if (mean(check) < 1){
  stop("Some stations in data_91 are not found in the ICES station dictionary")
} else {
  message("All stations in data_91 are found in the ICES station dictionary")
}

lookup_statn <- bind_rows(
  data_91 %>% select(STNNO, STATN),
  data.frame(STNNO = "19N", STATN = "19N Breøyane")
)

#
# . Create dat_orig_2 -----------------
#
# Add column STATN to data by left join    
dat_orig_2 <- dat_orig_1 %>%
  left_join(
    lookup_statn,
    by = c("STATION_CODE" = "STNNO"), relationship = "many-to-one")

# 28% of the stations were not found, we show below that this is stations ended in 2021 or
# or before
mean(is.na(dat_orig_2$STATN)) 

# Summarize for each station the last year, 
check <- dat_orig_2 %>%
  group_by(STATION_CODE, STATN) %>%
  summarize(
    n_year = length(unique(MYEAR)),
    last_year = max(MYEAR), .groups = "drop") %>% 
  arrange(desc(last_year))

check2 <- check %>% 
  filter(last_year >= 2015 & is.na(STATN)) %>%
  group_by(last_year) %>%
  summarize(STATION_CODEs = paste(STATION_CODE, collapse = ", "))
check2

# Of the stations in use in 2022, only the industry station I969 and I969 are not found  
# Should probably also add some stations ended the later years: 36A (Tjøme), 33F (flatfish Sande) ++

#
# . Create dat_orig_3 -----------------
#
# We drop stations lacking STATN, plus some parameters    
dat_orig_3 <- dat_orig_2 %>% 
  filter(!is.na(STATN)) %>%
  # also drop the following determinands, as we at the moment lack have sex, n_individual  
  filter(!PARAM %in% c("EROD", "VDSI"))  

#
# . Lookup data for ICES station code (station_code) -------
# - For joining onto concentration data  
# - Taken from ICES station dictionary  
#

lookup_ices_station_code_1 <- dat_stations %>% 
  select(station_name, station_code, station_activefromdate, station_activeuntildate)

# 12 stations have several rows in the station dictionary
#   because they have moved one or several times
check <- lookup_ices_station_code_1 %>%
  filter(station_name %in% dat_orig_3$STATN) %>%
  add_count(station_name) %>%
  filter(n > 1)
check$station_name %>% unique %>% length

# Thus, we select those rows which has no value for 
# 'station_activeuntildate', i.e. that are still active
lookup_ices_station_code_2 <- lookup_ices_station_code_1 %>%
  filter(is.na(station_activeuntildate))  

#
# . Lookup data for 'sample' (sample ID), 'matrix', and 'unit' -------
#
# 'sample'
#   could/should use SAMPLE_ID from Nivabasen, now we just make unique numbers
#   based on STATION_CODE, MYEAR, LATIN_NAME, and SAMPLE_NO2
lookup_sampleid <- dat_orig_3 %>%
  distinct(STATION_CODE, MYEAR, LATIN_NAME, SAMPLE_NO2) %>%
  arrange(STATION_CODE, MYEAR, LATIN_NAME, SAMPLE_NO2)
lookup_sampleid$sample <- seq_len(nrow(lookup_sampleid))

# 'matrix'
# based on code in '842_ICES_submission_2022data.Rmd' 
lookup_matrix <- tibble(
  TISSUE_NAME = c("Muskel", "Blod", "Galle", "Liver - microsome", "Lever", "Whole soft body"),
  matrix = c("MU", "BL", "BI", "LIMIC", "LI", "SB")
)
# table(dat_orig_4$TISSUE_NAME)
# table(data_ices[["10"]]$MATRX)

# 'unit'
# based on code in '842_ICES_submission_2022data.Rmd' 
lookup_unit <- data.frame(
  UNIT = c("MG_P_KG", "NG_P_G", "UG_P_KG", 
           "%", "PERCENT",
           "ng/min/mg protein", "pmol/min/mg protein", 
           "Index", "idx"),
  unit = c("mg/kg", "ng/kg", "ug/kg", 
           "%", "%",
           "ng/min/mg protein", "pmol/min/mg protein", 
           "idx", "idx"), 
  stringsAsFactors = FALSE)


#
# . Create dat_orig_4 -----------------
#

# Add 3 columns by left join: 
# - station_code (for ICES, a number)
# - 'sample' (sample ID)
# - 'matrix'     
# Also filter away (for now) some "difficult" units
dat_orig_4 <- dat_orig_3 %>% 
  left_join(
    lookup_ices_station_code_2, 
    by = c("STATN" = "station_name"), relationship = "many-to-one") %>%
  left_join(
    lookup_sampleid, 
    by = c("STATION_CODE", "MYEAR", "LATIN_NAME", "SAMPLE_NO2"), relationship = "many-to-one") %>%
  left_join(
    lookup_matrix, 
    by = c("TISSUE_NAME"), relationship = "many-to-one") %>%
  left_join(
    lookup_unit, 
    by = c("UNIT"), relationship = "many-to-one") %>%
  # remove rows where we couldn't find the corresponding maxtrix or unit  
  filter(
    !is.na(matrix) & !is.na(unit))

cat("----------------------------------------------\nOriginal units:\n----------------------------------------------\n")
table(dat_orig_3$UNIT)
cat("----------------------------------------------\nDeleted data with the following units:\n----------------------------------------------\n")
setdiff(unique(dat_orig_3$UNIT), unique(dat_orig_4$UNIT))


#
# . Create dat_formatted -----------------
#

# Need to do later:
# 1. fix sex (for EROD stations at least)
# 2. check units
# 3. add relativeøly recently used stations  

dat_formatted <- dat_orig_4 %>% 
  mutate(
    country = "Norway",
    sample_latitude = NA, 
    sample_longitude = NA,
    date = case_when(
      # !is.na(SAMPLE_DATE) ~ lubridate::ymd(SAMPLE_DATE), # lubridate::floor_date?
      !is.na(SAMPLE_DATE) ~ as.character(SAMPLE_DATE), 
      TRUE ~ paste0(MYEAR, "-10-15")),
    sex = "",
    n_individual = 1,
    subseries = NA,
    determinand = toupper(PARAM),
    basis = "W",
    censoring = case_when(
      FLAG1 %in% "<" ~ "Q",
      TRUE ~ ""),
    limit_detection = NA, 
    limit_quantification = case_when(
        FLAG1 %in% "<" ~ VALUE_WW,
        TRUE ~ NA),
    method_pretreatment = "",
    method_analysis = "",
    method_extraction = "") %>%
  rename(
    station_name = STATN,
    year = MYEAR,
    species = LATIN_NAME,
    value = VALUE_WW,
    uncertainty = UNCRT,
    unit_uncertainty = METCU) %>%
  select(
    country, station_code, station_name, sample_latitude, sample_longitude,
    year, date, species, sex, n_individual, subseries, sample, 
    determinand, matrix, basis, unit, value, censoring,
    limit_detection, limit_quantification, uncertainty, unit_uncertainty,
    method_pretreatment, method_analysis, method_extraction) 

#
# . Write data to files ----- 
#

# Write data to csv file  
readr::write_csv(
  dat_formatted, "data/example_external_data/OSPAR_NO_2022.csv")

# Write stations to csv file  
readr::write_csv(
  subset(dat_stations, station_country == "Norway") %>% rename(country = station_country),
  "data/example_external_data/ICES_DOME_STATIONS_20230829_NO.csv")

#
# read_data ------------------------------------------------------------------------
#

biota_data <- read_data(
  compartment = "biota",
  purpose = "OSPAR",
  contaminants = "OSPAR_NO_2022.csv",   # NB, replace biota data filename above, as appropriate 
  stations = "ICES_DOME_STATIONS_20230829_NO.csv",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path("data", "example_external_data"),
  data_format = "external",
  info_dir = file.path("information", "OSPAR_2022")
)

str(biota_data, 1)
str(biota_data$info, 1)
str(biota_data$info$thresholds, 1)

#
# tidy_data  ------------------------------------------------------
#

biota_data <- tidy_data(biota_data)

#
# create_timeseries --------------------------------------------
#

oddities.dir <- file.path("oddities", "milkys")
if (!dir.exists(oddities.dir)) {
  dir.create(oddities.dir, recursive = TRUE)
} 

# debugonce(create_timeseries)


# biota_data$data should not include country and station_name  
# - will lead to an error in left_join on station_code
# IMPROVEMENT: deleting these variable this should happen automatically, with a warning 

if ("country" %in% names(biota_data$data))
  biota_data$data$country <- NULL
if ("station_name" %in% names(biota_data$data))
  biota_data$data$station_name <- NULL

biota_timeseries <- create_timeseries(
  biota_data,
  determinands = ctsm_get_determinands(biota_data$info),
  determinands.control = NULL,
  oddity_path = oddities.dir,   # this doesn't seem to be respected, files are written to oddities/biota
  return_early = FALSE,
  print_code_warnings = FALSE,
  get_basis = get_basis_most_common,
  normalise = FALSE,
  normalise.control = list()
)

# IMPROVEMENT: Could be added at the end of create_time series
cat("Number of time series created:", nrow(biota_timeseries$timeSeries), "\n")

dir("oddities/biota")
check <- read.csv("oddities/biota/method_analysis_queries.csv")
head(check, 3)
table(check$determinand)

check <- read.csv("oddities/biota/species_group_queries.csv")
nrow(check)
head(check, 3)
table(check$species_group)

check <- read.csv("oddities/biota/value_queries.csv")
nrow(check)
head(check, 3)
table(addNA(check$value))
check %>% filter(value == 120)  # drywt% over 100  

str(biota_timeseries, 1)
str(biota_timeseries$info$thresholds, 1)

#
# run_assessment ------------------------------------------------------
#

# str(biota_timeseries, 1)
# nrow(biota_timeseries$timeSeries)
# View( biota_timeseries$timeSeries)

#
# We pick just a few, using the 'subset' option in 'biota_assessment'  
# IMPROVEMENT (documentation):
# The 'subset' option takes a boolean vector (TRUE/FALSE) the same length as 'biota_timeseries$timeSeries'  
#

params <- c("CB118", "CD")
sel_series <- biota_timeseries$timeSeries$determinand %in% params
length(sel_series)
sum(sel_series)

rerun_assessment <- FALSE

if (rerun_assessment){
  # Takes 10 minutes appx.
  
  biota_assessment <- run_assessment(
    biota_timeseries,
    subset = sel_series,
    AC = NULL,
    get_AC_fn = NULL,
    recent_trend = 20,
    parallel = FALSE, 
    extra_data = NULL,
    control = list(power = list(target_power = 80, target_trend = 10)) 
  )
  
  # saveRDS(biota_assessment, "data/example_external_data/OSPAR_NO_2022_assessment.rds")
  
} else {
  biota_assessment <- readRDS("data/example_external_data/OSPAR_NO_2022_assessment.rds")
}

# the original time series:
head(biota_assessment$timeSeries, 2)

# NOTE: returns assessment object with length equal to the entire data set  
length(biota_assessment$assessment)
lacking <- purrr::map_lgl(biota_assessment$assessment, is.null)
cat(100*mean(lacking), "percent of the", length(lacking), "time series were not assessed")
cat(sum(!lacking), "time series were assessed")

# check the assessment object of the first time series
i <- 1
str(biota_assessment$assessment[[i]], 1)

# check the assessment object of the first assessed time series
i <- which(!lacking)[1]
i
str(biota_assessment$assessment[[i]], 1)



#
# check_assessment (checks convergence) ---------------------------------------------
#

check_assessment(biota_assessment, save_result = FALSE)
# Only one!

#
# write_summary_table ----------------------------------------------------------------------
#
# This writes the summary data to a file in output/example_external_data. The argument 
#   extra_output = "power" ensures that the power metrics for lognormally distributed 
#   data will be exported.

summary.dir <- file.path("output", "external")
if (!dir.exists(summary.dir)) {
  dir.create(summary.dir, recursive = TRUE)
} 

# fn <- "external_amap_output.csv"
fn <- "external_ospar_no_output.csv"
write_summary_table(
  biota_assessment,
  output_file = fn,   # NB, file will be overwritten so change name as appropriate to retain results
  output_dir = summary.dir,
  export = TRUE,
  determinandGroups = NULL,
  symbology = NULL,
  collapse_AC = NULL, 
  extra_output = "power"
)

res_entire_dataset <- read.csv(paste0(summary.dir, "/", fn))
nrow(res_entire_dataset)
res <- res_entire_dataset %>% filter(shape != "") 
nrow(res)

table(addNA(res$shape))


#
# plot_assessment ---------------------------------------------------------------------------
#

plot.dir <- file.path("plots", "milkys")
if (!dir.exists(plot.dir)) {
  dir.create(plot.dir, recursive = TRUE)
} 

# IMPROVEMEMT:
# need to specify 'subset = sel_series' where 'sel_series' 
#   are assessments you have already done
# otherwise the error will be hard to understand  

# MAke 'sel_series2', a subset of 'sel_series'
which(sel_series)
sel_series2 <- rep(FALSE, length(sel_series))
sel_series2[which(sel_series)[1:3]] <- TRUE
which(sel_series2)

# debugonce(plot_assessment)
# debugonce(harsat:::plot.data)
#   harsat:::plot.data contains the plotting itself  
# plot.scales
# plot.AC
plot_assessment(
  biota_assessment,
  subset = sel_series2,     # need to specify subset here as well
  output_dir = plot.dir,
  file_type = c("data", "index"),
  file_format = "png"
)
length(dir(plot.dir))  # 77 - one too much!!
nrow(res)*2            # 76 (expected one 'data' and one 'index' plot per series)

#
# What is the extra plot?
#

nm1a <- with(res, 
             paste(station_code, country, station_name, determinand,
                   species, matrix, "NA index"))
nm1b <- with(res, 
             paste(station_code, country, station_name, determinand,
                   species, matrix, "NA data"))
nm1 <- c(nm1a, nm1b)
nm2 <- sub(".png", "", dir(plot.dir), fixed = TRUE)
setdiff(nm2, nm1)

# the extra plot is:
#   10921 Norway 24B Bergen harbour CB101 Gadus morhua LI NA index.png
# so, for some reason, one CB101 series is also included :-D 


#
# get_assessment_data ----------------------------------------------------------------------------
#

plotdat <- get_assessment_data(
  biota_assessment,
  subset = sel_series2)

str(plotdat[[i]], 1)
str(plotdat[[i]]$assessment, 1)
str(plotdat[[i]]$assessment$contrasts, 1)
str(plotdat[[i]]$info, 1)

library(ggplot2)

i <- 2
plotdat[[i]]$assessment$fullData
ggplot(plotdat[[i]]$assessment$fullData, aes(year)) +
  geom_ribbon(
    data = plotdat[[i]]$assessment$pred, 
    aes(ymin = exp(ci.lower), ymax = exp(ci.upper)),  # note; hard-coded exp
    fill = "lightblue") + 
  geom_path(
    data = plotdat[[i]]$assessment$pred, 
    aes(y = exp(fit))) + 
  geom_point(
    aes(y = concentration, color = censoring),
    color = "darkred") +
  scale_y_log10() +
  labs(title = plotdat[[i]]$output_id)

str(plotdat[[i]]$info, 1)


#
# Report ----------------------------------------------------------------------------
#


report.dir <- file.path("reports", "milkys")
if (!dir.exists(report.dir)) {
  dir.create(report.dir, recursive = TRUE)
} 

# Only CD, and only the 5 first stations  
params <- c("CD")
sts <- biota_timeseries$timeSeries %>%
  filter(determinand %in% params) %>%
  pull(station_code) %>%
  head(5)
sel_series2 <- with(biota_timeseries$timeSeries,
                    determinand %in% params & station_code %in% sts)
length(sel_series2)
sum(sel_series2)

# debugonce(report_assessment)

# the actual stuff going on here is found in 'report_assessment.Rmd'  
# in C:\R\Library\harsat\markdown

report_assessment(
  biota_assessment,
  subset = sel_series2,
  output_dir = report.dir
)





#
# FOR TESTING --------------------------------------
#

# debugonce(plot_assessment2)
plot_assessment2(
  biota_assessment,
  output_dir = plot.dir,
  file_type = c("data", "index"),
  file_format = "png"
)


