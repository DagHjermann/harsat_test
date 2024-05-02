

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
# AMAP data ----------------------------------------------------------------------------------------------------
#

#
# . Check data  ----
#

# readLines(("data/example_OSPAR/biota.txt"), 3)
fn <- "data/example_external_data/EXTERNAL_FO_PW_DATA.csv"
readLines(fn)
dat_all <- read.csv(fn, sep = ",", header = TRUE, stringsAsFactors = FALSE)
xtabs(~station_code, dat_all)
xtabs(~species, dat_all)
xtabs(~determinand, dat_all)


#
# . Read data  -------------
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
# Prepare data for next stage -------------------------------------
#

biota_data <- tidy_data(biota_data)

#
# Construct timeseries --------------------------------------------
#

oddities.dir <- file.path("oddities", "amap_external")
if (!dir.exists(oddities.dir)) {
  dir.create(oddities.dir, recursive = TRUE)
} 

biota_timeseries <- create_timeseries(
  biota_data,
  determinands = ctsm_get_determinands(biota_data$info),
  determinands.control = NULL,
  oddity_path = oddities.dir,
  return_early = FALSE,
  print_code_warnings = FALSE,
  get_basis = get_basis_most_common,
  normalise = FALSE,
  normalise.control = list()
)

cat("Number of time series created:", nrow(biota_timeseries$timeSeries), "\n")

dir(oddities.dir)
dir(file.path(oddities.dir, "biota")
read.csv("oddities/biota/species_group_queries.csv")
read.csv("oddities/biota/unit_queries.csv")

#
# Assessment ------------------------------------------------------
#

# str(biota_timeseries, 1)
# nrow(biota_timeseries$timeSeries)
# View( biota_timeseries$timeSeries)

params <- c("CB118", "CD")
sel_series <- biota_timeseries$timeSeries$determinand %in% params
sum(sel_series)

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
# biota_assessment <- readRDS("data/example_external_data/OSPAR_NO_2022_assessment.rds")

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

fn <- "external_amap_output.csv"
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

res <- read.csv(paste0(summary.dir, "/", fn))
res %>% filter(shape != "") %>% View("res ok")
table(addNA(res$shape))
table(addNA(res$))


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


#
# Appendix 2: German data (from Christoph) ---------------------------------------------------------------------
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



