
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


get_trend_symbols <- function(assessment_object, alpha = 0.05){
  
  # NOTE: finds recent trend (using 'prtrend', 'rtrend', 
  #   not 'pltrend', 'ltrend')
  
  # Get all summaries (empty or not)
  summ_all <- purrr::map(assessment_object$assessment, "summary")
  # names(summ_all)[1:3]
  # summ_all[[2]]
  
  # Get boolean variable for whether there is a result  
  summ_has_result <- purrr::map_lgl(summ_all, \(.) length(.) > 0)
  # summ_has_result[1:3]

  # Get non-empty summaries 
  summ <- summ_all[summ_has_result] %>% bind_rows()
  summ$series <- names(summ_all[summ_has_result])
  # head(summ)
  
  # Code based on start of harsat:::ctsm_symbology_OSPAR
  # - Only change: 'summary' in line 1 and 2 replaced with 'summ'
  with(summ, {
    shape <- character(nrow(summ))
    trendFit <- !is.na(pltrend)     # this is for long trend, but this just checks whether we got a trend
    shape[trendFit] <- "large_filled_circle"
    isTrend <- !is.na(prtrend) & prtrend < alpha
    upTrend <- isTrend & rtrend > 0
    downTrend <- isTrend & rtrend < 0
    shape[downTrend] <- "downward_triangle"
    shape[upTrend] <- "upward_triangle"
    statusFit <- !trendFit & nyfit >= 3
    shape[statusFit] <- "small_filled_circle"
    shape[!trendFit & !statusFit] <- "small_open_circle"
    shape
  })
  
}


# Function to find ICES station code (a number, but formatted as string) from NIVA station code  

find_station_code <- function(nivacode, timeseries_object){
  sel <- grepl(nivacode, timeseries_object$stations$station_name)
  result <- timeseries_object$stations$station_code[sel]
  if (length(result) > 1){
    stop("More than one station found!")
  }
  result
}

if (FALSE){
  # test 
  osparcodes <- find_station_code("36A1", biota_timeseries)
}

                
                
