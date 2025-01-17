
# 1. Packages -------------------------------------------

# install.packages(c("glue", "cli", "rlang", "utf8", "fansi", "Rcpp", "stringi", "digest", "purrr"))
# devtools::install("../harsat")
# remotes::install_github("osparcomm/HARSAT@main")
# remotes::install_local("~/Downloads/harsat_0.1.2.tar")

# help(package = "")

library(harsat)
library(dplyr)
library(readr)

#
# 2. Make subset csv files ----
# NOTE: not needed  
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

# # Creating subset 2
# dat_sel <- dat_all %>% 
#   filter(grepl("30B Oslo City area", statn) & PARAM %in% c("CD", "PB", "PFOS", "PFO"))
# xtabs(~param, dat_sel)
# write.table(dat_sel, "data/example_OSPAR/biota_30B_Oslo.txt", sep = "\t", row.names = FALSE)

# # Creating subset of stations file
# dat <- read_delim("data/example_OSPAR/stations.txt", guess_max = 13000)
# sel <- dat$station_name %in% c("30A Gressholmen", "30B Oslo City area")
# sum(sel)   # 6 rows
# dat_sel <- dat[sel,]
# write_delim(dat_sel, "data/example_OSPAR/stations_30A_30B.txt")

test <- read_delim("data/example_OSPAR/stations_30A_30B.txt")

#
# 3. Read data --------------------------------------------
#

# debugonce(read_data)
# debugonce(read_stations)
biota_data <- read_data(
  compartment = "biota", 
  purpose = "OSPAR",                               
  contaminants = "biota_30B_Oslo.txt", 
  stations = "stations_30A_30B.txt", 
  data_dir = file.path("data", "example_OSPAR"),         ## i.e., C:\Users\test\ospar\data
  info_dir = file.path("information", "OSPAR_2022"),  ## i.e., C:\Users\test\ospar\information
  extraction = "2023/08/23"
)

# View(biota_data$data)

#
# Prepare data for next stage ------------------------
#

biota_data <- tidy_data(biota_data)

#
# Construct timeseries -----------------------------------


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





# Tidy data ----------------------------------------
biota_data <- tidy_data(biota_data)

str(biota_data, 1)
str(biota_data$info, 1)
ctsm_get_determinands(biota_data$info)

#
# Construct timeseries ---------------------------------
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

ctsm_get_determinands(biota_data$info)

str(biota_timeseries, 1)
str(biota_timeseries$timeSeries, 1)

#
# Assessment - main run -------------------------------------
#
# Note: the control argument specifies that the post-hoc power metrics will 
#   be based on a power of 80% and an annual percentage increase in concentration of 10%.

rerun_assessment <- FALSE

if (rerun_assessment){
  
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
  
  # saveRDS(biota_assessment, "data/example_OSPAR/biota_30B_Oslo_assess.rds")
  
} else {
  
  biota_assessment <- readRDS("data/example_OSPAR/biota_30B_Oslo_assess.rds")
  
}


# . check assessment result object -------------------------------

str(biota_assessment, 1)
str(biota_assessment$assessment, 1)
str(biota_assessment$assessment[[1]], 1)

str(biota_assessment$assessment[[1]]$annualIndex, 1)
str(biota_assessment$assessment[[1]]$coefficients, 1)
# View(biota_assessment$assessment[[1]]$coefficients)

library(ggplot2)
ggplot(biota_assessment$assessment[[1]]$annualIndex, 
       aes(year, index, shape = censoring)) +
  geom_point()
  


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
# Plots ---------------------------------------------------------------------------
#

plot.dir <- file.path("plots", "ospar", "biota")
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


plot_assessment2(
  biota_assessment,
  output_dir = plot.dir,
  file_type = c("data", "index"),
  file_format = "png"
)
