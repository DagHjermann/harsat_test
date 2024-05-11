
plot_assessment <- function(
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
  
  timeSeries <- apply_subset(timeSeries, subset, parent.frame())
  
  series_id <- row.names(timeSeries)
  
  
  # plot each timeSeries
  
  lapply(series_id, function(id) {
    
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
    
    if ("index" %in% file_type) {
      
      output_file <- paste0(output_id, " index.", file_format)
      output_file <- file.path(output_dir, output_file)
      
      switch(
        file_format, 
        png = png(output_file, width = 680, height = 480), 
        pdf = pdf(output_file, width = 7, height = 7 * 12 / 17)
      )
      
      plot.data(data, assessment, series, info, type = "assessment", xykey.cex = 1.4) 
      dev.off()
      
    }    
    
    
    # plot assessment with data
    
    if ("data" %in% file_type) {
      
      output_file <- paste0(output_id, " data.", file_format)
      output_file <- file.path(output_dir, output_file)
      
      switch(
        file_format, 
        png = png(output_file, width = 680, height = 480), 
        pdf = pdf(output_file, width = 7, height = 7 * 12 / 17)
      )
      
      plot.data(data, assessment, series, info, type = "data", xykey.cex = 1.4)
      dev.off()
      
    }  
    
  })
  
  invisible() 
}  

