
#
# running a whol assessment procedure, from reading raw data to assessment itself:

test_assessment <- function(filename_rawdata, 
                            filename_stations,
                            selected_station_names = "10B Varangerfjorden",
                            selected_params = "CB118"){
  
  # 3. Read data 
  biota_data <- read_data(
    compartment = "biota", 
    purpose = "custom", 
    contaminants = filename_rawdata, 
    data_format = "external", 
    stations = filename_stations, 
    data_dir = file.path("data", "example_OSPAR"),         ## i.e., C:\Users\test\ospar\data
    info_dir = file.path("information", "OSPAR_2022"),  ## i.e., C:\Users\test\ospar\information
    extraction = "2023/08/23"
  )
  
  # 4. Tidy data
  biota_data <- tidy_data(biota_data)
  
  # 5. Construct timeseries
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
  
  # 6. Assessment
  # Runs the time series analyses itself
  
  # a. find station codes for selected stations 
  selected_station_codes <- biota_timeseries$stations %>%
    filter(station_name %in% selected_station_names) %>%
    pull(station_code)
  
  # b. select time series for given substances and stations  
  timeseries_selected <- biota_timeseries$timeSeries$determinand %in% selected_params & 
    biota_timeseries$timeSeries$station_code %in% selected_station_codes
  
  cat("===============================\nNumber of selected time series:",
      sum(timeseries_selected))
  cat("\n===============================\n")
  
  biota_assessment <- run_assessment(
    biota_timeseries,
    subset = timeseries_selected,
    AC = NULL,
    get_AC_fn = NULL,
    recent_trend = 20L,
    parallel = FALSE, 
    extra_data = NULL
  )
  
  # let's find the first time series that actually was assessed:
  i <- which(timeseries_selected)[1]
  
  # which data series is this?
  biota_timeseries$timeSeries[i,]
  
  # let's check its assessment object - it is also a list, 
  #   with 13 items in this case
  
  assessments <- biota_assessment$assessment[timeseries_selected]
  
  cat("===============================\nresult:\n===============================\n")
  print(
    str(assessments, 1)
  )
  cat("===============================\n")
  
  invisible(assessments)
  
}
