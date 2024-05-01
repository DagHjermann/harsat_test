

# Packages -------------------------------------------

# install.packages(c("glue", "cli", "rlang", "utf8", "fansi", "Rcpp", "stringi", "digest", "purrr"))
# devtools::install("../harsat")
# remotes::install_github("osparcomm/HARSAT@main")
# remotes::install_local("~/Downloads/harsat_0.1.2.tar")

# help(package = "")

library(harsat)
library(dplyr)
library(readr)


#
# Check data  ----
#

# readLines(("data/example_OSPAR/biota.txt"), 3)
fn <- "data/example_external_data/EXTERNAL_FO_PW_DATA.csv"
readLines(fn)
dat_all <- read.csv(fn, sep = ",", header = TRUE, stringsAsFactors = FALSE)
xtabs(~station_code, dat_all)
xtabs(~species, dat_all)
xtabs(~determinand, dat_all)


#
# Read data  ---------------------------------------------------------------------------------
#

#
# . AMAP data -------------------------
#

biota_data <- read_data(
  compartment = "biota",
  purpose = "AMAP",
  contaminants = "EXTERNAL_FO_PW_DATA.csv",   # NB, replace biota data filename above, as appropriate 
  stations = "EXTERNAL_AMAP_STATIONS.csv",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path("data", "example_external_data"),
  data_format = "external",
  info_dir = file.path("information", "AMAP"),
)


#
# . German data (from Christoph) -------------------------
#

# debugonce(read_data)
# debugonce(read_contaminants)
# debugonce(report_file_digest)
# debugonce(safe_read_file)
#   enc <- readr::guess_encoding(file)
biota_data <- read_data(
  compartment = "biota",
  purpose = "AMAP",
  contaminants = "UBA_bio.csv",   # NB, replace biota data filename above, as appropriate 
  stations = "DE_stations_utf8.csv",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path("data", "example_external_data"),
  data_format = "external",
  info_dir = file.path("information", "AMAP"),
)

#
# . Norwegian data (from Milkys) -------------------------
#

dat_orig_1 <- readRDS("../Milkys2_pc/Files_from_Jupyterhub_2022/Raw_data/105_data_with_uncertainty_2023-09-12.rds")
names(dat_orig_1)

# Need 'read_tsv' from the rear package to read this properly  
dat_stations <- readr::read_tsv("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", guess_max = 7000)
table(subset(dat_stations, station_country == "Norway")$station_name)

# Not needed as we have the RDS file  
# source("../Milkys2_pc//844_ICES_submission_check_functions.R")
# data_ices <- read_ices_file(fn)

fn <- "../Milkys2_pc/Files_to_ICES/2022/Rdata/842_NIVA2022CF_08.rds"
data_ices <- readRDS(fn)
length(data_ices)
data_ices <- add_field_codes(data_ices)
str(data_ices, 1)
data_91 <- data_ices[["91"]]
# table(data_91$STNNO)
# table(data_91$STATN)

# Can ICES stattio nnames from table 91 (which also contains NIVA codes) be found in the stattion dict.?
statn_1 <- unique(data_91$STATN) 
statn_2 <- unique(dat_stations$station_name)
check <- statn_1 %in% statn_2
mean(check)

lookup_statn <- bind_rows(
  data_91 %>% select(STNNO, STATN),
  data.frame(STNNO = "19N", STATN = "19N Breøyane")
)

lookup_ices_station_code_1 <- dat_stations %>% 
  select(station_name, station_code, station_activefromdate, station_activeuntildate)

dat_orig_2 <- dat_orig_1 %>%
  left_join(
    lookup_statn,
    by = c("STATION_CODE" = "STNNO"), relationship = "many-to-one")
mean(is.na(dat_orig_2$STATN))

check <- dat_orig_2 %>%
  group_by(STATION_CODE, STATN) %>%
  summarize(
    n_year = length(unique(MYEAR)),
    last_year = max(MYEAR), .groups = "drop") %>% 
  arrange(desc(last_year))

# Of the stations in use in 2022, only the industry station I969 and I969 are not found 
xtabs(~STATION_CODE, check %>% filter(last_year == 2022 & is.na(STATN)))

# We drop the rest of the stations 
dat_orig_3 <- dat_orig_2 %>% 
  filter(!is.na(STATN)) %>%
  filter(!PARAM %in% c("EROD", "VDSI"))  # as we don't have sex, n_individual  

check <- lookup_ices_station_code_1 %>%
  filter(station_name %in% dat_orig_3$STATN) %>%
  add_count(station_name) %>%
  filter(n > 1)

lookup_ices_station_code_2 <- lookup_ices_station_code_1 %>%
  filter(is.na(station_activeuntildate))  

lookup_sampleid <- dat_orig_3 %>%
  distinct(STATION_CODE, MYEAR, LATIN_NAME, SAMPLE_NO2) %>%
  arrange(STATION_CODE, MYEAR, LATIN_NAME, SAMPLE_NO2)
lookup_sampleid$sample <- seq_len(nrow(lookup_sampleid))

lookup_matrix <- tibble(
  TISSUE_NAME = c("Muskel", "Blod", "Galle", "Liver - microsome", "Lever"),
  matrix = c("MU", "BL", "BI", "LIMIC", "LI")
)
# table(dat_orig_4$TISSUE_NAME)
# table(data_ices[["10"]]$MATRX)

table(dat_orig_4$UNIT)

# Ass ICES station_code  
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
  filter(
    !is.na(matrix), UNIT %in% c("%", "MG_P_KG", "UG_P_KG"))

# fix sex (for EROD stations at least)
# check units


table(is.na(dat_orig_4$SAMPLE_DATE))
head(table(dat_orig_4$SAMPLE_DATE))
head(table(as.character(dat_orig_4$SAMPLE_DATE)))

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
    unit = UNIT,
    value = VALUE_WW,
    uncertainty = UNCRT,
    unit_uncertainty = METCU) %>%
  select(
    country, station_code, station_name, sample_latitude, sample_longitude,
    year, date, species, sex, n_individual, subseries, sample, 
    determinand, matrix, basis, unit, value, censoring,
    limit_detection, limit_quantification, uncertainty, unit_uncertainty,
    method_pretreatment, method_analysis, method_extraction) 

# Write data to csv file  
readr::write_csv(
  dat_formatted, "data/example_external_data/OSPAR_NO_2022.csv")

# Write stations to csv file  
readr::write_csv(
  subset(dat_stations, station_country == "Norway") %>% rename(country = station_country),
  "data/example_external_data/ICES_DOME_STATIONS_20230829_NO.csv")


biota_data <- read_data(
  compartment = "biota",
  purpose = "OSPAR",
  contaminants = "OSPAR_NO_2022.csv",   # NB, replace biota data filename above, as appropriate 
  stations = "ICES_DOME_STATIONS_20230829_NO.csv",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path("data", "example_external_data"),
  data_format = "external",
  info_dir = file.path("information", "OSPAR_2022")
)




#
# Prepare data for next stage -------------------------------------
#

biota_data <- tidy_data(biota_data)

#
# Construct timeseries --------------------------------------------
#

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


#
# Assessment ------------------------------------------------------
#

# str(biota_timeseries, 1)
# nrow(biota_timeseries$timeSeries)
# View( biota_timeseries$timeSeries)

params <- c("CB118", "CB28", "CD", "CD", "HG")
sel_series <- biota_timeseries$timeSeries$determinand %in% params

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


#
# Check convergence ---------------------------------------------
#

check_assessment(biota_assessment, save_result = FALSE)

#
# Summary files ----------------------------------------------------------------------
#
# This writes the summary data to a file in output/example_external_data. The argument 
#   extra_output = "power" ensures that the power metrics for lognormally distributed 
#   data will be exported.

summary.dir <- file.path("output", "external")
if (!dir.exists(summary.dir)) {
  dir.create(summary.dir, recursive = TRUE)
} 

write_summary_table(
  biota_assessment,
  output_file = "external_amap_output.csv",   # NB, file will be overwritten so change name as appropriate to retain results
  output_dir = summary.dir,
  export = TRUE,
  determinandGroups = NULL,
  symbology = NULL,
  collapse_AC = NULL, 
  extra_output = "power"
)

res <- read.csv(paste0(summary.dir, "/external_amap_output.csv"))


#
# Plots ---------------------------------------------------------------------------
#

plot.dir <- file.path("plots", "external")
if (!dir.exists(plot.dir)) {
  dir.create(plot.dir, recursive = TRUE)
} 


# debugonce(plot_assessment)
plot_assessment(
  biota_assessment,
  output_dir = plot.dir,
  file_type = c("data", "index"),
  file_format = "png"
)

# debugonce(plot_assessment)
plot_assessment2(
  biota_assessment,
  output_dir = plot.dir,
  file_type = c("data", "index"),
  file_format = "png"
)




#
# Appendix 1: ICES DOME files -------------------------------------------------------------------------
#

# These are in tab-separated, but it seems that comma-separated is what should be used when using 
# external files  

# dir("data/example_external_data/")
readLines("data/example_external_data/ICES_FO_PW_DATA_20230829.txt", n = 2)
read_tsv("data/example_external_data/ICES_FO_PW_DATA_20230829.txt", n_max = 2)
test <- read_tsv("data/example_external_data/ICES_FO_PW_DATA_20230829.txt")

# This one is OK  
test <- read.table("data/example_external_data/ICES_FO_PW_DATA_20230829.txt", 
                   sep = "\t", header = TRUE)
# This one is not  
test <- read.table("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", 
                   sep = "\t", header = TRUE, encoding = "UTF-8")
# Error in scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  : 
#                 line 129 did not have 30 elements
test <- read.table("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", 
                   sep = "\t", header = FALSE, encoding = "UTF-8", 
                   skip = 1)
# Also problem with 'read_tsv', just in a different place  
test <- readr::read_tsv("data/example_external_data/ICES_DOME_STATIONS_20230829.txt")
problems(test)

# Increase 'guess_max' to solve this  
test <- readr::read_tsv("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", guess_max = 7000)
table(test$station_country)

test <- readr::read_tsv("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", n_max = 6000)
problems(test)
test <- readr::read_tsv("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", n_max = 6000, guess_max = 6000)
problems(test)

test1 <- read.table("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", 
                    sep = "\t", header = TRUE, nrows = 128)
dim(test1)
tail(test1, 1)
test2 <- read.table("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", 
                    sep = "\t", header = FALSE, skip = 127, nrows = 1)
test3 <- read.table("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", 
                    sep = "\t", header = FALSE, skip = 128, nrows = 1)
dim(test2)
dim(test3)
test2[1:15]
test3[1:15]
test2[16:30]
test3[16:30]
test4 <- readLines("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", n = 129)
tail(test4, 2)
test[129:130]
test

test1 <- read.table("data/example_external_data/ICES_DOME_STATIONS_20230829.txt", 
                    sep = "\t", header = FALSE, skip = 128, nrows = 1)
str(test)
# debugonce(read_data)
# debugonce(read_contaminants)
# debugonce(harsat:::read_stations)
# debugonce(report_file_digest)
debugonce(harsat:::safe_read_file)
#   enc <- readr::guess_encoding(file)
biota_data <- read_data(
  compartment = "biota",
  purpose = "HELCOM",
  contaminants = "ICES_FO_PW_DATA_20230829.txt",   # NB, replace biota data filename above, as appropriate 
  stations = "ICES_DOME_STATIONS_20230829.txt",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path("data", "example_external_data"),
  data_format = "external",
  info_dir = file.path("information", "HELCOM_2023"),
)

