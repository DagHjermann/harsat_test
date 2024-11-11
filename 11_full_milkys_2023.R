

# Packages -------------------------------------------

# install.packages(c("glue", "cli", "rlang", "utf8", "fansi", "Rcpp", "stringi", "digest", "purrr"))
# devtools::install("../harsat")
# remotes::install_github("osparcomm/HARSAT@main")
# if "HTTP error 401. Bad credentials"
#   
# remotes::install_local("~/Downloads/harsat_0.1.2.tar")

# help(package = "harsat")

library(harsat)
library(dplyr)
library(readr)
library(ggplot2)
source("00_functions.R")

#
# check parameters ---------------------------------------------------------------------------
#

# old data
fn <- "data/example_external_data/OSPAR_NO_2022.csv"
df <- readr::read_csv(fn)
params1 <- df$determinand |> unique() |> sort()

# old info
fn <- "information/OSPAR_2022/determinand.csv"
df <- readr::read_csv(fn)
params1b <- df$determinand |> unique() |> sort()

# new data
fn <- "data/full_OSPAR_2023/raw_data.csv"
df <- readr::read_csv(fn)
params2 <- df$determinand |> unique() |> sort()

# check 36A/36A1
xtabs(~station_name, df %>% filter(grepl("36A", station_name)))
xtabs(~station_name + year, df %>% filter(grepl("36A", station_name)))

# new info
newinfo <- readRDS("data/full_OSPAR_2023/info.rds")
df <- newinfo$determinand
params2b <- rownames(df) |> unique() |> sort()

# setdiff(params1, params2)
# setdiff(params2, params1)
# setdiff(params2, params1b)

setdiff(params2b, params1b)




#
# read_data ------------------------------------------------------------------------
#

# debugonce(read_data)
# OLD data
biota_data <- read_data(
  compartment = "biota",
  purpose = "OSPAR",
  contaminants = "OSPAR_NO_2022.csv",   # NB, replace biota data filename above, as appropriate 
  stations = "ICES_DOME_STATIONS_20230829_NO.csv",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path("data", "example_external_data"),
  data_format = "external",
  info_dir = file.path("information", "OSPAR_2022")
)

# NEW data
biota_data <- read_data(
  compartment = "biota",
  purpose = "OSPAR",
  contaminants = "raw_data.csv",   # NB, replace biota data filename above, as appropriate 
  stations = "ICES_DOME_STATIONS_20230829_NO.csv",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path("data", "full_ospar_2023"),
  data_format = "external",
  info_dir = file.path("information", "OSPAR_2022")
)

str(biota_data, 1)
str(biota_data$info, 1)
str(biota_data$info$thresholds, 1)

#
# tidy_data  ------------------------------------------------------
#

biota_data <- tidy_data(biota_data)


# debugonce(create_timeseries)
# debugonce(harsat:::merge_auxiliary)

#
# replace 'info' with modified info ---------------------------------------------------------------------------
#

# read from file (created in milkys4)
newinfo <- readRDS("data/full_OSPAR_2023/info.rds")

# add recent_years
newinfo$recent_years <- biota_data$info$recent_years
# add max_year
newinfo$max_year <- 2023

# 'determinand': set 'biota_auxiliary' to NA for VDSI and EROD  
sel <- rownames(newinfo$determinand) %in% c("VDSI.PLUS1", "EROD.BOTHSEXES")
newinfo$determinand$biota_auxiliary[sel] <- NA
newinfo$determinand$distribution[sel] <- "lognormal"

# 'thresholds': add thresholds for HG.LENADJ
thresholds_hgadj <- newinfo$thresholds %>% 
  filter(determinand == "HG") %>%
  mutate(determinand = "HG.LENADJ")
newinfo$thresholds <- newinfo$thresholds %>%
  bind_rows(thresholds_hgadj)

# check
str(biota_data$info, 1)
str(newinfo, 1)

# newinfo should have more determinands abd thresholds
nrow(biota_data$info$determinand)
nrow(newinfo$determinand)
nrow(biota_data$info$thresholds)
nrow(newinfo$thresholds)

# Check that otherwise, they have the same components
setdiff(names(biota_data$info), names(newinfo))
setdiff(names(newinfo), names(biota_data$info))

# Replace info object
biota_data$info <- newinfo

# Check end of determinand dataset
biota_data$info$determinand %>% tail(10)

#
# create_timeseries --------------------------------------------
#

oddities.dir <- file.path("oddities", "milkys")
if (!dir.exists(oddities.dir)) {
  dir.create(oddities.dir, recursive = TRUE)
} 

# debugonce(create_timeseries)


# biota_data$data should not include country and station_name  
# - will lead to an error in left_join on station_code
# IMPROVEMENT: deleting these variable this should happen automatically, with a warning 

if ("country" %in% names(biota_data$data))
  biota_data$data$country <- NULL
if ("station_name" %in% names(biota_data$data))
  biota_data$data$station_name <- NULL

biota_timeseries <- create_timeseries(
  biota_data,
  determinands = ctsm_get_determinands(biota_data$info),
  determinands.control = NULL,
  oddity_path = oddities.dir,   # this doesn't seem to be respected, files are written to oddities/biota
  return_early = FALSE,
  print_code_warnings = FALSE,
  get_basis = get_basis_most_common,
  normalise = FALSE,
  normalise.control = list()
)

# IMPROVEMENT: Could be added at the end of create_time series
cat("Number of time series created:", nrow(biota_timeseries$timeSeries), "\n")

# Check that HG.LENADJ is included in 'biota_timeseries'  
biota_data$data %>% filter(grepl("HG", determinand)) %>% xtabs(~determinand, .)
biota_timeseries$data %>% filter(grepl("HG", determinand)) %>% xtabs(~determinand, .)

#
# run_assessment (test) ------------------------------------------------------
#

# str(biota_timeseries, 1)
# nrow(biota_timeseries$timeSeries)
# View( biota_timeseries$timeSeries)


#
# We pick just a few, using the 'subset' option in 'biota_assessment'  
# IMPROVEMENT (documentation):
# The 'subset' option takes a boolean vector (TRUE/FALSE) the same length as 'biota_timeseries$timeSeries'  
#

params <- c("CB118", "CD")
params <- c("CD")
params <- c("HG.LENADJ")
params <- c("D4", "D4", "D4")
sel_series1 <- biota_timeseries$timeSeries$determinand %in% params
sel_series2 <- biota_timeseries$timeSeries$species %in% "Gadus morhua"
sel_series2 <- biota_timeseries$timeSeries$species %in% "Mytilus edulis"
# Pick only the first 2
# sel_series <- sel_series & cumsum(sel_series) <= 2
sum(sel_series)

params <- c("EROD.BOTHSEXES", "SPAH15", "SPAH16", "VDSI.PLUS1",
            "HG.LENADJ", "D4", "D4", "D4")
params <- c("EROD.BOTHSEXES", "SPAH15", "SPAH16", "VDSI.PLUS1")
params <- c("SPAH15", "SPAH16")
params <- c("EROD.BOTHSEXES", "VDSI.PLUS1")
params <- c("VDSI.PLUS1")
params <- c("EROD.BOTHSEXES", "SPAH15", "SPAH16",
            "HG.LENADJ", "D4", "D4", "D4")
params <- c("HG.LENADJ")

# these are OK
params <- c("SPAH15", "SPAH16",
            "HG.LENADJ", "D4", "D4", "D4")
# these create an error, see below
# params <- c("VDSI.PLUS1")
# params <- c("EROD.BOTHSEXES")
# Error in checkForRemoteErrors(val) : 
#   8 nodes produced errors; first error: replacement has 0 rows, data has 2

sel_series <- biota_timeseries$timeSeries$determinand %in% params
sel_series_names <- rownames(biota_timeseries$timeSeries)[sel_series]

setdiff(params, names(biota_timeseries$info$determinand))


length(sel_series)
sum(sel_series)

rm(biota_assessment_extra)

# debugonce(run_assessment)
# debugonce(harsat:::assessment_engine)
# debugonce(harsat:::assess_lmm)
biota_assessment_extra <- run_assessment(
  biota_timeseries, 
  subset = sel_series,
  AC = NULL,
  get_AC_fn = NULL,
  recent_trend = 10,
  parallel = FALSE, 
  extra_data = NULL,
  control = list(power = list(target_power = 80, target_trend = 10)) 
)
str(biota_assessment_extra, 1)

# save
# saveRDS(biota_assessment_extra, "data/full_OSPAR_2023/OSPAR_NO_2023_assessment_01_extra.rds")
# combine with original 
biota_assessment_orig <- readRDS("data/full_OSPAR_2023/OSPAR_NO_2023_assessment_01.rds")
biota_assessment_new <- biota_assessment_orig
biota_assessment_new$data <- bind_rows(
  biota_assessment_orig$data,
  biota_assessment_new$data %>% filter(seriesID %in% sel_series_names)
)
View(biota_assessment_new$data)

#
# run_assessment (all) ------------------------------------------------------
#

rerun_assessment <- FALSE
# rerun_assessment <- TRUE

save_path <- "data/full_OSPAR_2023/OSPAR_NO_2023_assessment_02.rds"

if (rerun_assessment){
  # Takes some hours!
  # 04h 33m 45s for the entire data sets
  
  # with full data, stops after 17% (29 minutes):
  # Error in checkForRemoteErrors(val) : 
  #   2 nodes produced errors; first error: replacement has 0 rows, data has 21
  
  biota_assessment <- run_assessment(
    biota_timeseries,
    # subset = sel_series,
    AC = NULL,
    get_AC_fn = NULL,
    recent_trend = 10,
    parallel = TRUE, 
    extra_data = NULL,
    control = list(power = list(target_power = 80, target_trend = 10)) 
  )
  
  saveRDS(biota_assessment, save_path)
  
} else {
  biota_assessment <- readRDS(save_path)
}


#
# run_assessment (selection) ------------------------------------------------------
#

#
# Select only by parameters  
#
params <- c("SPAH15", "SPAH16", "VDSI.PLUS1", "EROD.BOTHSEXES", 
            "HG.LENADJ", "D4", "D4", "D4")
sel_series1 <- biota_timeseries$timeSeries$determinand %in% params
sel_series <- sel_series1

#
# Select by parameters + species
#
# sel_series2 <- biota_timeseries$timeSeries$species %in% "Gadus morhua"
# sel_series2 <- biota_timeseries$timeSeries$species %in% "Mytilus edulis"
# sel_series <- sel_series1 & sel_series2

#
# Select only by stations    
#
nivacodes <- c("36A1", "227G2")
# Function to find OSPAR station code (a number) from NIVA station code  
# Note
find_station_code <- function(nivacode, timeseries_object){
  sel <- grepl(nivacode, timeseries_object$stations$station_name)
  result <- timeseries_object$stations$station_code[sel]
  if (length(result) > 1){
    stop("More than one station found!")
  }
  result
}
# test 
osparcodes <- c(find_station_code("36A1", biota_timeseries),  
                find_station_code("227G2", biota_timeseries))  
sel_series1 <- biota_timeseries$timeSeries$station_code %in% osparcodes  
sel_series <- sel_series1
# 13 minutes processing time

#
# Number of series selected
#
message(sum(sel_series), " series selected")

rerun_assessment <- FALSE
rerun_assessment <- TRUE
save_path <- "data/full_OSPAR_2023/OSPAR_NO_2023_assessment_extra03.rds"  # UPDATE

if (rerun_assessment){
  
  if (file.exists(save_path)){
    stop("NOTE: The file ", sQuote(save_path), " already exists")
  } else {
    message("No file wil be overewritten")
  }
  
  # Note: if you run using 'parallel = TRUE', you cannot see which determinand caused
  # problems in case of errors, making it difficult to sort out errors.
  # You will typically get the error message  'Error in checkForRemoteErrors(val)' 
  # - see issue: https://github.com/osparcomm/HARSAT/issues/206
  biota_assessment <- run_assessment(
    biota_timeseries,
    subset = sel_series,
    AC = NULL,
    get_AC_fn = NULL,
    recent_trend = 10,
    parallel = FALSE, 
    extra_data = NULL,
    control = list(power = list(target_power = 80, target_trend = 10)) 
  )
  
  # Add object that says which time series that actually has been attempted 
  #   to run 
  # (Needed, as I can't see a way in the final object to tell the difference between
  #   attemped to run but fail, or thos that were not selected for running)
  # This object is used in the functipon 'extract_trend_results' 
  #   in 'milkys4' project script 903
  # 
  biota_assessment$subset_series <- rownames(biota_timeseries$timeSeries)[sel_series]
  
  saveRDS(biota_assessment, save_path)
  
} else {
  biota_assessment <- readRDS(save_path)
}


#
# check_assessment (checks convergence) ---------------------------------------------
#

check_assessment(biota_assessment, save_result = FALSE)
# Only one!


#
# look into assessment object ----------------------------------------------------------------------
#

# the original time series:
head(biota_assessment$timeSeries, 2)

# NOTE: returns assessment object with length equal to the entire data set  
length(biota_assessment$assessment)
lacking <- purrr::map_lgl(biota_assessment$assessment, is.null)
cat(100*mean(lacking), "percent of the", length(lacking), "time series were not assessed")
cat(sum(!lacking), "time series were assessed")

# check the assessment object of the first time series
i <- 1
str(biota_assessment$assessment[[i]], 1)

# check the assessment object of the first assessed time series
i <- which(!lacking)[1]
i
str(biota_assessment$assessment[[i]], 1)

# get summaries  
summ_all <- purrr::map(biota_assessment$assessment, "summary")
names(summ_all)[1:3]

# - summaries with results
summ_has_result <- purrr::map_lgl(summ_all, \(.) length(.) > 0)
mean(summ_has_result)
summ <- summ_all[summ_has_result] %>% bind_rows()
summ$series <- names(summ_all[summ_has_result])
summ %>% 
  filter(series == "4684 CD Gadus morhua LI NA")

# get contrasts
contr_all <- purrr::map(biota_assessment$assessment, "contrasts")
is.data.frame(contr_all[[2]])
contr_has_result <- purrr::map_lgl(contr_all, is.data.frame)
contr <- contr_all[contr_has_result]
contr[["4684 CD Gadus morhua LI NA"]]

# fitted trend line
str(biota_assessment, 1)
str(biota_assessment$assessment[["4684 CD Gadus morhua LI NA"]], 1)
fitted <- biota_assessment$assessment[["4684 CD Gadus morhua LI NA"]]$pred
ggplot(fitted, aes(year, fit)) + 
  geom_path() +
  geom_path(aes(y = ci.lower), linetype = "dashed") +
  geom_path(aes(y = ci.upper), linetype = "dashed")
  
harsat:::ctsm.lmm.contrast(fitt, 2004, 2023)

#
# Get trends using self-defined function 'get_trend_symbol'  
#

sel <- names(biota_assessment$assessment) == "4684 CD Gadus morhua LI NA"
sum(sel)
get_trend_symbols(biota_assessment)[sel]
debugonce(get_trend_symbols)
trend_symbols <- get_trend_symbols(biota_assessment)
str(trend_symbols, 1)

#
# get long and short-term trends ---------------------------------------------------------  
#

# get contrasts
contr_all <- purrr::map(biota_assessment$assessment, "contrasts")
is.data.frame(contr_all[[2]])
contr_has_result <- purrr::map_lgl(contr_all, is.data.frame)
contr <- contr_all[contr_has_result]

# Add "trend_type" to all data frames
contr <- purrr::map(contr, \(.x) {mutate(.x, trend_type = rownames(.x))} )

# Add "series" to all data frames
contr <- purrr::map2(contr, names(contr), \(.x, .y) {mutate(.x, series = .y)} )
contr[["4684 CD Gadus morhua LI NA"]]

# Get series data frame (includes station code, determinand, ...)
df_series <- biota_assessment$timeSeries
df_series$series <- rownames(df_series)
head(df_series)

df_contr <- contr %>%
  bind_rows() %>%
  left_join(
    biota_assessment$timeSeries, by = "series")




#
# run_assessment, one series ------------------------------------------------------
#


# Single parameters  
# params <- c("CB118", "CD")
# sel_series <- biota_timeseries$timeSeries$determinand %in% params

# Single series  
sel_series <- rownames(biota_timeseries$timeSeries) %in% "4684 CD Gadus morhua LI NA"

length(sel_series)
sum(sel_series)

assessm <- run_assessment(
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
# write_summary_table ----------------------------------------------------------------------
#
# This writes the summary data to a file in output/example_external_data. The argument 
#   extra_output = "power" ensures that the power metrics for lognormally distributed 
#   data will be exported.

summary.dir <- file.path("output", "external")
if (!dir.exists(summary.dir)) {
  dir.create(summary.dir, recursive = TRUE)
} 

# fn <- "external_amap_output.csv"
fn <- "external_ospar_no_output.csv"
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

res_entire_dataset <- read.csv(paste0(summary.dir, "/", fn))
nrow(res_entire_dataset)
res <- res_entire_dataset %>% filter(shape != "") 
nrow(res)

table(addNA(res$shape))


#
# plot_assessment ---------------------------------------------------------------------------
#

plot.dir <- file.path("plots", "milkys")
if (!dir.exists(plot.dir)) {
  dir.create(plot.dir, recursive = TRUE)
} 

# IMPROVEMEMT:
# need to specify 'subset = sel_series' where 'sel_series' 
#   are assessments you have already done
# otherwise the error will be hard to understand  

# MAke 'sel_series2', a subset of 'sel_series'
which(sel_series)
sel_series2 <- rep(FALSE, length(sel_series))
sel_series2[which(sel_series)[1:3]] <- TRUE
which(sel_series2)

# debugonce(plot_assessment)
# debugonce(harsat:::plot.data)
#   harsat:::plot.data contains the plotting itself  
# plot.scales
# plot.AC
plot_assessment(
  biota_assessment,
  subset = sel_series2,     # need to specify subset here as well
  output_dir = plot.dir,
  file_type = c("data", "index"),
  file_format = "png"
)
length(dir(plot.dir))  # 77 - one too much!!
nrow(res)*2            # 76 (expected one 'data' and one 'index' plot per series)

#
# What is the extra plot?
#

nm1a <- with(res, 
             paste(station_code, country, station_name, determinand,
                   species, matrix, "NA index"))
nm1b <- with(res, 
             paste(station_code, country, station_name, determinand,
                   species, matrix, "NA data"))
nm1 <- c(nm1a, nm1b)
nm2 <- sub(".png", "", dir(plot.dir), fixed = TRUE)
setdiff(nm2, nm1)

# the extra plot is:
#   10921 Norway 24B Bergen harbour CB101 Gadus morhua LI NA index.png
# so, for some reason, one CB101 series is also included :-D 


#
# get_assessment_data ----------------------------------------------------------------------------
#

plotdat <- get_assessment_data(
  biota_assessment,
  subset = sel_series2)

str(plotdat[[i]], 1)
str(plotdat[[i]]$assessment, 1)
str(plotdat[[i]]$assessment$contrasts, 1)
str(plotdat[[i]]$info, 1)

library(ggplot2)

i <- 2
plotdat[[i]]$assessment$fullData
ggplot(plotdat[[i]]$assessment$fullData, aes(year)) +
  geom_ribbon(
    data = plotdat[[i]]$assessment$pred, 
    aes(ymin = exp(ci.lower), ymax = exp(ci.upper)),  # note; hard-coded exp
    fill = "lightblue") + 
  geom_path(
    data = plotdat[[i]]$assessment$pred, 
    aes(y = exp(fit))) + 
  geom_point(
    aes(y = concentration, color = censoring),
    color = "darkred") +
  scale_y_log10() +
  labs(title = plotdat[[i]]$output_id)

str(plotdat[[i]]$info, 1)


#
# Report ----------------------------------------------------------------------------
#


report.dir <- file.path("reports", "milkys")
if (!dir.exists(report.dir)) {
  dir.create(report.dir, recursive = TRUE)
} 


# Only PCB 7, and only one station   
params <- c("CB28", "CB52", "CB105", "CB118", "CB138", "CB153", "CB180")
params <- c("CB118", "CB138", "CB153", "CB180")
params <- c("CB138", "CB153")
stations <- c("I024")
sts <- subset(biota_assessment$stations, 
                   grepl(stations, station_name))$station_code
sel_series2 <- with(biota_assessment$timeSeries,
                    determinand %in% params & station_code %in% sts)
length(sel_series2)
sum(sel_series2)


# Only CD, and only the 5 first stations  
params <- c("CD")
sts <- biota_timeseries$timeSeries %>%
  filter(determinand %in% params) %>%
  pull(station_code) %>%
  head(5)
sel_series2 <- with(biota_timeseries$timeSeries,
                    determinand %in% params & station_code %in% sts)
length(sel_series2)
sum(sel_series2)

# debugonce(report_assessment)

# the actual stuff going on here is found in 'report_assessment.Rmd'  
# in C:\R\Library\harsat\markdown

report_assessment(
  biota_assessment,
  subset = sel_series2,
  output_dir = report.dir
)





#
# FOR TESTING --------------------------------------
#

# debugonce(plot_assessment2)
plot_assessment2(
  biota_assessment,
  output_dir = plot.dir,
  file_type = c("data", "index"),
  file_format = "png"
)


