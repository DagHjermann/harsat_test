

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

# check new parameters  
df_new_pars <- read_csv("data/full_OSPAR_2023/Param 2022 lack data 2023.csv")
table(addNA(df_new_pars$`Lacking, priority`))
df_new_pars <- df_new_pars %>%
  filter(`Lacking, priority` %in% 2:3)
setdiff(df_new_pars$PARAM, params2)
# "Mirex" "Nonaklor, trans-" "Oxyklordan" "PROTV" 
# "Toksafen Parlar 26" "Toksafen Parlar 50"
# "Toksafen Parlar 62"

# new info
newinfo <- readRDS("data/full_OSPAR_2023/info.rds")
df <- newinfo$determinand
params2b <- rownames(df) |> unique() |> sort()

# setdiff(params1, params2)
# setdiff(params2, params1)
# setdiff(params2, params1b)

setdiff(params2b, params1b)

#
# fix data ----------------------------------------------------------------------
#

if (FALSE){
  
  # copy oldfile
  # dir("data/full_OSPAR_2023")
  # file.copy("data/full_OSPAR_2023/raw_data.csv",
  #           "data/full_OSPAR_2023/raw_data_OLD1.csv")
  
  # existing in info$determinands, but must be renamed: 
  # KRYSEN = CHR
  # HEPTAKLOR EPOKSID = HCEPX
  # TRANS-HEPTAKLOREPOKSID = HCEPT
  # will be added to info$determinands, but change them here first: 
  # PERFLUORDEKANSULFONAT (PFDS) = PFDS
  # PERFLUORTRIDEKANSYRE (PFTRA) = PFTRA
  
  # Read original data
  fn <- "data/full_OSPAR_2023/raw_data.csv"
  df <- readr::read_csv(fn)

  # Change KRYSEN to CHR
  sel <- df$determinand %in% "KRYSEN"; sum(sel)
  df$determinand[sel] <- "CHR"
  sel <- df$determinand %in% "HEPTAKLOR EPOKSID"; sum(sel)
  df$determinand[sel] <- "HCEPX"
  sel <- df$determinand %in% "TRANS-HEPTAKLOREPOKSID"; sum(sel)
  df$determinand[sel] <- "HCEPT"
  sel <- df$determinand %in% "PERFLUORDEKANSULFONAT (PFDS)"; sum(sel)
  df$determinand[sel] <- "PFDS"
  sel <- df$determinand %in% "PERFLUORTRIDEKANSYRE (PFTRA)"; sum(sel)
  df$determinand[sel] <- "PFTRA"
  
  #
  # station 71G (imposex-> intersex)
  #
  sel <- df$station_name %in% "71G Fugløyskjær" & df$determinand %in% c("VDSI.PLUS1", "INTERSEX")
  xtabs(~year + species, df[sel,])
  # 1. 
  sel <- df$station_name %in% "71G Fugløyskjær" & df$determinand %in% c("VDSI.PLUS1")
  xtabs(~year + species, df[sel,])
  df$determinand[sel] <- "VDSI.INTERSEX.PLUS1"
  df$species[sel] <- "N. lapillus L. littorea"
  # 2
  sel <- df$station_name %in% "71G Fugløyskjær" & df$determinand %in% c("INTERSEX")
  df$determinand[sel] <- "VDSI.INTERSEX.PLUS1"
  df$unit[sel] <- "ug/kg"
  df$value[sel] <- df$value[sel] + 1
  df$species[sel] <- "N. lapillus L. littorea"
  # check
  sel <- df$station_name %in% "71G Fugløyskjær" & df$determinand %in% c("VDSI.INTERSEX.PLUS1")
  xtabs(~year + species + determinand + unit, df[sel,])
  
  #
  # OH-pyren (PYR1OH) at 15B
  #
  # Lacking 2001:2004, 2008:2014
  df %>%
    filter(grepl("15B", station_name) & determinand == "PYR1OH") %>%
    xtabs(~year + determinand + station_name, .)  
  # Get raw data
  dat_105 <- readRDS("../Milkys2_pc/Files_from_Jupyterhub_2022/Raw_data/105_data_with_uncertainty_2023-09-12.rds")
  # They lack lack 2001:2004, 2008:2014 for PYR1OH but not for PYR1O (adjusted data)
  # Also, absorbance exists for most of these years (2001:2004 and 2011-2015, but not 2008-2010)
  dat_105 %>%
    filter(STATION_CODE == "15B" & PARAM %in% c("PYR1O", "PYR1OH", "AY380", "AY", "ABS380")) %>%
    xtabs(~MYEAR + PARAM, .)
  # Make data to add
  dat_105_to_add_1 <- dat_105 %>%
    filter(STATION_CODE == "15B" & PARAM %in% "PYR1O" & MYEAR %in% c(2001:2004, 2008:2014)) %>%
    mutate(
      PYR1O = VALUE_WW,
      PARAM = "PYR1OH",
      VALUE_WW = NA)
  dat_105_absorbance <- dat_105 %>%
    filter(STATION_CODE == "15B" & PARAM %in% c("AY", "AY380", "ABS380") & MYEAR %in% c(2001:2004, 2008:2014)) %>%
    select(STATION_CODE, MYEAR, SAMPLE_NO2, VALUE_WW) %>%
    rename(AY380 = VALUE_WW)
  nrow(dat_105_to_add_1)
  nrow(dat_105_absorbance)
  dat_105_to_add_2 <- dat_105_to_add_1 %>%
    left_join(dat_105_absorbance, join_by(STATION_CODE, SAMPLE_NO2, MYEAR), relationship = "many-to-one") %>%
    # Make unnormalized data (also see scr. 172):
    mutate(VALUE_WW = PYR1O*AY380)
  nrow(dat_105_to_add_2)
  # Format data in OSPAR format
  # We start with the existing data - we will use the first row of this to create the 
  #   "fixed" parts of the data we weill add
  df_existing <- df %>%
    filter(grepl("15B", station_name) & determinand == "PYR1OH")
  # names(df_existing) %>% dput()
  fixed_columns <- c("country", "station_code", "station_name", "sample_latitude", 
                     "sample_longitude", "species", "sex", "n_individual", 
                     "subseries", "sample", "determinand", "matrix", "basis", "unit", 
                     "limit_detection", "limit_quantification", 
                     "uncertainty", "unit_uncertainty", "method_pretreatment", "method_analysis", 
                     "method_extraction")
  dat_to_add_fixed <- df_existing[1, fixed_columns]
  # Variable part of the data, reformatted to OSPAR value (and assuming sample date 15.10)
  dat_to_add_variable <- dat_105_to_add_2 %>%
    rename(
      year = MYEAR, 
      value = VALUE_WW) %>%
    mutate(
      censoring = case_when(
        is.na(FLAG1) ~ as.character(NA),
        !is.na(FLAG1) ~ "Q"),
      date = lubridate::ymd(paste0(dat_105_to_add_2$MYEAR, "-10-15"))
    ) %>%
    select(year, date, value, censoring)
  # Finally, create the data we will add:
  dat_to_add <- bind_cols(
    dat_to_add_fixed,
    dat_to_add_variable)
  df_new <- bind_rows(
    df,
    dat_to_add
  ) 
  # Check - the number of columns should be the same
  dim(df)
  dim(df_new)
  # Write to csv file
  readr::write_csv(df_new, fn, na = "")

  
  #
  # OH-pyren (PYR1OH) at 15B - fix "sample"
  #
  # when we did the stuff above to add extra PYR1OH, we also introduced a lot of duplicate 'sample'
  #   specifically, 185 observations with sample = 5284
  # these are deleted in create_timeseries ()
  #
  range(df$sample)
  sel <- with(df, grepl("15B", station_name) & determinand == "PYR1OH")
  df_existing <- df[sel,]
  sum(sel)
  plot(which(sel))
  plot(which(sel), df_existing$year)
  plot(which(sel), as.numeric(df_existing$sample))
  table(df_existing$sample)
  # Make new sample numbers for all with sample = 5284  
  sel2 <- with(df, grepl("15B", station_name) & determinand == "PYR1OH" & sample == "5284")
  n <- sum(sel2)
  new_sample_start <- max(as.numeric(df$sample)) + 1
  new_samples <- seq(new_sample_start, length.out = n)
  new_samples_char <- as.character(new_samples)
  # Give new sample numbers
  df$sample[sel2] <- new_samples_char
  
  #
  # Write to csv file
  #
  readr::write_csv(df, fn, na = "")
  
  # NOTE: important to put na = ""!
  # - the default is na = "NA", which is not allowed, it should be "" 
  # - this will go unnoticed until 'create_timeseries' when over-LOQ values will
  # mysteriously be deleted (it gives a message, but there are many messages so it
  # easily goes unnoticed)
  # - see code of 'harsat:::check_censoring ' (which is called from 'create_timeseries'):
  #   data <- ctsm_check(data, 
  #                      !censoring %in% c("", "D", "Q", "<"), 
  #                      action = "delete", 
  #                      message = "Unrecognised censoring values", 
  #                      file_name = "censoring_codes_unrecognised", 
  #                      info = info)
  
}


#
# read_data ------------------------------------------------------------------------
#

# debugonce(read_data)
# OLD data
biota_data_1 <- read_data(
  compartment = "biota",
  purpose = "OSPAR",
  contaminants = "OSPAR_NO_2022.csv",   # NB, replace biota data filename above, as appropriate 
  stations = "ICES_DOME_STATIONS_20230829_NO.csv",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path("data", "example_external_data"),
  data_format = "external",
  info_dir = file.path("information", "OSPAR_2022")
)

# NEW data
biota_data_1 <- read_data(
  compartment = "biota",
  purpose = "OSPAR",
  contaminants = "raw_data.csv",   # NB, replace biota data filename above, as appropriate 
  stations = "ICES_DOME_STATIONS_20230829_NO.csv",    # NB, replace station data filename above, as appropriate 
  data_dir = file.path("data", "full_ospar_2023"),
  data_format = "external",
  info_dir = file.path("information", "OSPAR_2022")
)

str(biota_data_1, 1)
str(biota_data_1$info, 1)
str(biota_data_1$info$thresholds, 1)

# Check OH-pyren (PYR1OH) at 15B
# Should now also include 2001:2004, 2008:2014 (see "fix data" part above)
biota_data_1$data %>%
  filter(grepl("15B", station_name) & determinand == "PYR1OH") %>% # View()
  xtabs(~year + determinand + station_name, .)  

biota_data_1$data %>%
  filter(grepl("15B", station_name) & determinand == "PYR1OH") %>% # View()
  ggplot(aes(year, value, color = addNA(censoring))) +
  geom_jitter(width = 0.1)


#
# tidy_data  ------------------------------------------------------
#

biota_data <- tidy_data(biota_data_1)


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
# add to info file ---------------------------------------------
#

# BBJF  - already in info file, it turns out!
# BKF   - already in info file, it turns out! 

# PFDCA      
# PFHPA
# PFHXA
# PERFLUORDEKANSULFONAT (PFDS) = PFDS
# PERFLUORTRIDEKANSYRE (PFTRA) = PFTRA

# HEPTAKLOR
# DOT
# MOT

info <- biota_data$info
table(info$determinand$pargroup)

# Make new rows 
new_row <- info$determinand[c("BAP","BAP"),]
rownames(new_row) <- c("BBJF", "BKF")            # HARD-CODED - see dat_orig_6
new_row$common_name <- rownames(new_row)
# new_row$biota_sd_constant <- NA
# new_row$biota_sd_variable <- NA
new_row
new_row1 <- new_row                              # HARD-CODED

# Make new rows
new_row <- info$determinand[rep("PFUNDA",5),]
rownames(new_row) <- c(
  "PFDCA" , "PFHPA", "PFHXA", "PFDS", "PFTRA")            # HARD-CODED - see dat_orig_6
new_row$common_name <- rownames(new_row)
# new_row$biota_sd_constant <- NA
# new_row$biota_sd_variable <- NA
new_row
new_row2 <- new_row                             # HARD-CODED

# Make new rows
new_row <- info$determinand[rep("HCEPT",1),]
rownames(new_row) <- c("HEPTAKLOR")            # HARD-CODED - see dat_orig_6
new_row$common_name <- rownames(new_row)
# new_row$biota_sd_constant <- NA
# new_row$biota_sd_variable <- NA
new_row
new_row3 <- new_row                             # HARD-CODED

# Make new rows
new_row <- info$determinand[rep("TBSN+",2),]
rownames(new_row) <- c("DOT", "MOT")            # HARD-CODED - see dat_orig_6
new_row$common_name <- rownames(new_row)
# new_row$biota_sd_constant <- NA
# new_row$biota_sd_variable <- NA
new_row
new_row4 <- new_row                             # HARD-CODED

# Make new rows
new_row <- info$determinand[rep("VDSI.PLUS1",1),]
rownames(new_row) <- c("VDSI.INTERSEX.PLUS1")            # HARD-CODED - see dat_orig_6
new_row$common_name <- rownames(new_row)
# new_row$biota_sd_constant <- NA
# new_row$biota_sd_variable <- NA
new_row
new_row5 <- new_row                             # HARD-CODED

# PAH metabolites + ALAD - exist in the info file,
#   but change it to a "pesticide", j
sel_modify <- rownames(info$determinand) %in% c("BAP3OH", "PA1OH", "PYR1OH", "ALAD") # HARD-CODED
info$determinand[sel_modify,]
# change group to pesticide
info$determinand$pargroup <- "O-HER"
info$determinand$biota_group  <- "Pesticides"
info$determinand$biota_unit <- "ug/kg"
# remove fish length (LNMEA):
info$determinand$biota_auxiliary   <- "LIPIDWT%~DRYWT%"
info$determinand[sel_modify,]

# PAH metabolites + ALAD: must also change unit in the data
str(biota_data, 1)
sel <- biota_data$data$determinand %in% c("BAP3OH", "PA1OH", "PYR1OH", "ALAD")  
sum(sel)
biota_data$data$unit[sel] <- "ug/kg"

# sel2 <- biota_data$data$unit %in% "ng/min/mg protein"
# table(biota_data$data$determinand[sel2])

info_new <- info

new_rows_all <- bind_rows(
  new_row2, new_row3, new_row4, new_row5)  # turns out that BBJKF and BKF

# Check that there is no overlap of names
names1 <- rownames(info$determinand)
names2 <- rownames(new_rows_all)

# Should be empty - length(check) = 0
check <- intersect(names1, names2)
length(check)

biota_data$info$determinand <- bind_rows(
  info$determinand, 
  new_rows_all)

#
# Add species "N. lapillus L. littorea"
#

info$species %>% head(3)
sel <- info$species$reference_species %in% "Littorina littorea"
info$species[sel,]
new_row <- info$species[sel,]
rownames(new_row) <- "N. lapillus L. littorea" 
new_row$reference_species <- rownames(new_row)
new_row$common_name <- "Dog whelk C. periwinkle"
new_row

biota_data$info$species <- bind_rows(
  info$species, 
  new_row)
#
# Fix 'censoring' ---------------------------------------------
#
# value 'NA' is not allowed, it should be '' 
# see code of 'harsat:::check_censoring ' (which is called from 'create_timeseries'):
#   data <- ctsm_check(data, 
#                      !censoring %in% c("", "D", "Q", "<"), 
#                      action = "delete", 
#                      message = "Unrecognised censoring values", 
#                      file_name = "censoring_codes_unrecognised", 
#                      info = info)
# the question is why only "CHR" was affecrted by this??


# sel <- biota_data$data$censoring %in% "NA"
# sum(sel)
# biota_data$data$censoring[sel] <- ""


#
# create_timeseries --------------------------------------------
#

oddities.dir <- file.path("oddities", "milkys")
if (!dir.exists(oddities.dir)) {
  dir.create(oddities.dir, recursive = TRUE)
} 

# biota_data$data should not include country and station_name  
# - will lead to an error in left_join on station_code
# IMPROVEMENT: deleting these variable this should happen automatically, with a warning 

if ("country" %in% names(biota_data$data))
  biota_data$data$country <- NULL
if ("station_name" %in% names(biota_data$data))
  biota_data$data$station_name <- NULL

str(biota_data, 1)
str(biota_data$data, 1)
# biota_data_BACK <- biota_data
# View(biota_data$stations )
# View(biota_data$data)
# biota_data$data <- biota_data$data %>%
#   filter(determinand == "PYR1OH" & station_code == "4887")

# debugonce(create_timeseries)
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
# str(biota_timeseries, 1)

# IMPROVEMENT: Could be added at the end of create_time series
cat("Number of time series created:", nrow(biota_timeseries$timeSeries), "\n")

# Check that HG.LENADJ is included in 'biota_timeseries'  
biota_data$data %>% filter(grepl("HG", determinand)) %>% xtabs(~determinand, .)
biota_timeseries$data %>% filter(grepl("HG", determinand)) %>% xtabs(~determinand, .)

# Check that PYR1OH is included in 'biota_timeseries'  
biota_data$data %>% filter(grepl("PYR", determinand)) %>% xtabs(~determinand, .)
biota_timeseries$data %>% filter(grepl("PYR", determinand)) %>% xtabs(~determinand, .)

# parameters starting with "B"
biota_data$data %>% filter(substr(determinand,1,1) == "B") %>% xtabs(~determinand, .)
biota_data$data %>% filter(substr(determinand,1,1) == "V") %>% xtabs(~determinand, .)

biota_timeseries$data %>% filter(grepl("PYR1OH", determinand)) %>% 
  ggplot(aes(year, value, color = addNA(censoring))) +
  geom_jitter(width = 0.1)

biota_timeseries$data %>% 
  filter(grepl("PYR1OH", determinand)) %>% 
  arrange(censoring) %>%
  ggplot(aes(year, value, color = addNA(censoring))) +
  geom_jitter(width = 0.1)


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
params <- c("SCB7")
sel_series1 <- biota_timeseries$timeSeries$determinand %in% params
sel_series2 <- biota_timeseries$timeSeries$species %in% "Gadus morhua"
sel_series2 <- biota_timeseries$timeSeries$species %in% "Mytilus edulis"
sel_series3 <- biota_timeseries$timeSeries$station_code %in% c("5104", "4689", "5031", "4878", "5019", "12204", "4848", "4850", "4855")
sel_series3 <- biota_timeseries$timeSeries$station_code %in% c("4850")
# Pick only the first 2
# sel_series <- sel_series & cumsum(sel_series) <= 2
sel_series <- sel_series1 & sel_series2
sel_series <- sel_series1 & sel_series2 & sel_series3
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

# check selection
length(sel_series)
sum(sel_series)

# check data
check_timeseries <- biota_timeseries$timeSeries[sel_series,]
check_data <- biota_timeseries$data %>%
  filter(seriesID %in% rownames(check_timeseries))
table(addNA(check_data$censoring))
gg <- ggplot(check_data, aes(year, value, color = censoring)) +
  geom_point() +
  facet_wrap(vars(determinand, station_code))
gg
# Check assessment
assess_path <- "data/full_OSPAR_2023/OSPAR_NO_2023_assessment_01_extra.rds"
assess <- readRDS(assess_path)
# str(assess, 1)
sel_check <- rownames(check_timeseries) %in% rownames(assess$timeSeries)
sum(sel_check)

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
# Select only by parameters (1) 
#
params <- c("SPAH15", "SPAH16", "VDSI.PLUS1", "EROD.BOTHSEXES", 
            "HG.LENADJ", "D4", "D4", "D4")
sel_series1 <- biota_timeseries$timeSeries$determinand %in% params
sel_series <- sel_series1


#
# Select only by parameters (2) 
#
# OSPAR_NO_2023_assessment_extra04.rds
#
params <- c("D5", "D6")
sel_series1 <- biota_timeseries$timeSeries$determinand %in% params
sel_series <- sel_series1

#
# Select only by parameters (3) 
#
# OSPAR_NO_2023_assessment_extra05.rds
#
params <- rownames(new_rows_all)
params <- c(params, "CHR")       # add CHR (chrysen), as these now combine KRYSEN and CHR
sel_series1 <- biota_timeseries$timeSeries$determinand %in% params
sel_series <- sel_series1

#
# Select only by parameters (4) 
#
# OSPAR_NO_2023_assessment_extra06.rds
#
params <- "VDSI.INTERSEX.PLUS1"
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
osparcodes <- c(find_station_code("36A1", biota_timeseries),  
                find_station_code("227G2", biota_timeseries))  
sel_series1 <- biota_timeseries$timeSeries$station_code %in% osparcodes  
sel_series <- sel_series1
# 13 minutes processing time


#
# Select by stations and parameters    
#
# OSPAR_NO_2023_assessment_extra07.rds
#
nivacodes <- c("15B")
nivacodes <- c("15B", "23B", "30B", "53B")
# load 'find_station_code' above
osparcodes <- purrr::map_chr(nivacodes, \(x) find_station_code(x, biota_timeseries))
sel_series1 <- biota_timeseries$timeSeries$station_code %in% osparcodes  
sel_series2 <- biota_timeseries$timeSeries$determinand %in% c("PYR1OH")
sel_series <- sel_series1 & sel_series2


#
# Number of series selected
#
message(sum(sel_series), " series selected")

rerun_assessment <- FALSE
# rerun_assessment <- TRUE
save_path <- "data/full_OSPAR_2023/OSPAR_NO_2023_assessment_extra08.rds"  # UPDATE

if (rerun_assessment){
  
  if (file.exists(save_path)){
    stop("NOTE: The file ", sQuote(save_path), " already exists")
  } else {
    message("No file will be overewritten")
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

# add station_name to time series  
biota_assessment$timeSeries <- biota_assessment$timeSeries %>%
  left_join(biota_assessment$stations %>% select(station_code, station_name))

sel <- biota_assessment$timeSeries$determinand == "CHR"
sum(sel)
biota_assessment$timeSeries[sel,]
biota_assessment$timeSeries$station_name[sel]
sel <- biota_assessment$timeSeries$determinand == "CHR" &
  grepl("Single", biota_assessment$timeSeries$station_name)
sum(sel)

assm <- biota_assessment$assessment[sel][[1]]
str(assm, 1)
assm$fullData

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

# fitted trend line for series by name
series_name <- "4684 CD Gadus morhua LI NA"
series_name <- "4887 PYR1OH Gadus morhua BI"
str(biota_assessment$assessment[[series_name]], 1)
fitted <- biota_assessment$assessment[[series_name]]$pred
ggplot(fitted, aes(year, fit)) + 
  geom_path() +
  geom_path(aes(y = ci.lower), linetype = "dashed") +
  geom_path(aes(y = ci.upper), linetype = "dashed")
  
# fitted trend line with data  
fullData <- biota_assessment$assessment[[series_name]]$fullData
annualIndex <- biota_assessment$assessment[[series_name]]$annualIndex

ggplot(annualIndex, aes(year)) + 
  scale_shape_manual(values = c("<" = 6), na.value = 16) +
  geom_ribbon(data = fitted, aes(ymin = exp(ci.lower), ymax = exp(ci.upper)), fill = "lightblue") +
  geom_path(data = fitted, aes(y = exp(fit))) +
  geom_point(aes(y = exp(index), shape = censoring)) +
  labs(
    y = expression(Concentration*","*~mu*g/kg~(w.w.))
  ) +
  theme_bw()
  

#
# Get trends using self-defined function 'get_trend_symbol'  
#
sel <- names(biota_assessment$assessment) == series_name
sum(sel)
get_trend_symbols(biota_assessment)[sel]
# debugonce(get_trend_symbols)
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


