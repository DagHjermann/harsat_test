

# install.packages(c("glue", "cli", "rlang", "utf8", "fansi", "Rcpp", "stringi", "digest", "purrr"))
# devtools::install("../harsat")
# remotes::install_github("osparcomm/HARSAT@main")
# remotes::install_local("~/Downloads/harsat_0.1.2.tar")

library(harsat)

# dir_data <- paste0(here(), "data/amap")
dir_data <- "data/example_external_data"

biota_data <- read_data(
  compartment = "biota",
  purpose = "AMAP",
  contaminants = "EXTERNAL_FO_PW_DATA.csv",   # NB, replace biota data filename above, as appropriate 
  stations = "EXTERNAL_AMAP_STATIONS.csv",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path(dir_data),
  data_format = "external",
  info_dir = file.path("information", "AMAP"),
)

# Tidy data
biota_data <- tidy_data(biota_data)

# Construct timeseries
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



# Assessment
# Main runs
# Note: the control argument specifies that the post-hoc power metrics will 
#   be based on a power of 80% and an annual percentage increase in concentration of 10%.

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

# Check convergence
check_assessment(biota_assessment, save_result = FALSE)

# Summary files
# This writes the summary data to a file in output/example_external_data. The argument 
#   extra_output = "power" ensures that the power metrics for lognormally distributed 
#   data will be exported.

summary.dir <- file.path("output", "amap")

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
