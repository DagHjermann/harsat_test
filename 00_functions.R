
get_assessment_data <- function(
    assessment_obj, 
    subset = NULL) {
  
  # silence non-standard evaluation warnings
  seriesID <- NULL
  
  # graphics_functions.R
  
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
  
  
  # plot each timeSeries
  
  result <- lapply(series_id, function(id) {
    
    data <- dplyr::filter(assessment_obj$data, seriesID == id)
    
    assessment <- assessment_obj$assessment[[id]]
    
    
    # get relevant series info
    
    series <- timeSeries[id, ]
    
    
    # get file name from id, and add country and station name 
    # for easier identification
    
    output_id <- sub(
      series$station_code,
      paste(series$station_code, series$country, series$station_name), 
      id,
      fixed=TRUE
    )
    
    
    # get rid of any slashes that might have crept in 
    
    output_id <- gsub(" / ", " ", output_id, fixed = TRUE)
    output_id <- gsub("/", " ", output_id, fixed = TRUE)
    
    output_id <- gsub(" \ ", " ", output_id, fixed = TRUE)
    output_id <- gsub("\\", " ", output_id, fixed = TRUE)
    
    
    # plot assessment with index
    
    list(data=data, assessment=assessment, series=series, info=info, output_id=output_id)
    
  })
  
  names(result) <- series_id
  result 
  
}  

