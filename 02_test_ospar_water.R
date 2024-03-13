

# install.packages(c("glue", "cli", "rlang", "utf8", "fansi", "Rcpp", "stringi", "digest", "purrr"))
# devtools::install("../harsat")
# remotes::install_github("osparcomm/HARSAT@main")
# remotes::install_local("~/Downloads/harsat_0.1.2.tar")

library(harsat)


water_data <- read_data(
  compartment = "water", 
  purpose = "OSPAR",                               
  contaminants = "water.txt", 
  stations = "stations.txt", 
  data_dir = file.path("data", "example_ospar"),         ## i.e., C:\Users\test\ospar\data
  info_dir = file.path("information", "OSPAR_2022"),  ## i.e., C:\Users\test\ospar\information
  extraction = "2023/08/23"
)

# View(water_data$data)


# Tidy data
water_data <- tidy_data(water_data)

# Construct timeseries
# For each timeseries, use the basis which is reported most often in the data

water_timeseries <- create_timeseries(
  water_data,
  determinands = ctsm_get_determinands(water_data$info),
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

water_assessment <- run_assessment(
  water_timeseries,
  subset = NULL,
  AC = NULL,
  get_AC_fn = NULL,
  recent_trend = 20L,
  parallel = FALSE, 
  extra_data = NULL,
  control = list(power = list(target_power = 80, target_trend = 10)) 
)

str(water_assessment, 1)

# Check convergence
check_assessment(water_assessment, save_result = FALSE)

# Summary files
# This writes the summary data to a file in output/example_external_data. The argument 
#   extra_output = "power" ensures that the power metrics for lognormally distributed 
#   data will be exported.

summary.dir <- file.path("output", "amap")
if (!dir.exists(summary.dir)) {
  dir.create(summary.dir, recursive = TRUE)
} 

write_summary_table(
  water_assessment,
  output_file = "water-FO-PW-test-output.csv",   # NB, file will be overwritten so change name as appropriate to retain results
  output_dir = summary.dir,
  export = TRUE,
  determinandGroups = NULL,
  symbology = NULL,
  collapse_AC = NULL, 
  extra_output = "power"
)

res <- read.csv(paste0(summary.dir, "/water-FO-PW-test-output.csv"))

#
# Plots
#

plot.dir <- file.path("plots", "ospar", "water")
if (!dir.exists(plot.dir)) {
  dir.create(plot.dir, recursive = TRUE)
} 


plot_assessment(
  water_assessment,
  subset = 1:10,
  output_dir = plot.dir,
  file_type = c("data", "index"),
  file_format = "png"
)

