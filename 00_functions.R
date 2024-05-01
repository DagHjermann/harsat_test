
plot_assessment2 <- function(
    assessment_obj, 
    subset = NULL, 
    output_dir = ".",
    file_type = c("data", "index"),
    file_format = c("png", "pdf")) {
  
  # silence non-standard evaluation warnings
  seriesID <- NULL
  
  # graphics_functions.R
  
  # check file_type, file_format and output_dir are valid
  
  file_format = match.arg(file_format)
  
  if (!all(file_type %in% c("data", "index"))) {
    stop(
      "\nArgument 'file_type' is invalid: ", 
      "must be 'data' or 'index' or both of them", 
      call. = FALSE
    )
  }
  
  if (!dir.exists(output_dir)) {
    stop(
      "\nThe output directory '", output_dir, "' does not exist.\n", 
      "Create it or check the information supplied to argument 'output_dir'",
      " is correct.",
      call. = FALSE
    )
  }
  
  
  info <- assessment_obj$info
  timeSeries <- assessment_obj$timeSeries 
  
  
  # set up time series information:
  # - merge with station information
  # - add in additional useful variables 
  # - subset if necessary
  
  timeSeries <- tibble::rownames_to_column(timeSeries, "series")
  
  timeSeries <- dplyr::left_join(
    timeSeries, 
    assessment_obj$stations, 
    by = "station_code"
  )
  
  
  timeSeries$group <- ctsm_get_info(
    info$determinand, 
    timeSeries$determinand, 
    "group", 
    info$compartment,
    sep = "_"
  )
  
  timeSeries$distribution <- ctsm_get_info(
    info$determinand, 
    timeSeries$determinand, 
    "distribution"
  )
  
  if (info$compartment == "water") {
    timeSeries$matrix <- "WT"
  }
  
  timeSeries <- harsat:::apply_subset(timeSeries, subset, parent.frame())
  
  series_id <- row.names(timeSeries)
  
  browser()
  
}