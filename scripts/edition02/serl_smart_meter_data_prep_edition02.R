# This code prepares the half-hourly and daily smart meter data for
# the SERL Observatory datasets.

# Code developed by Ellen Webborn, UCL using R version 4.0.1 (2020-06-06)
# Most recent edits made by Ellen Webborn, 2021-03-31

# For the latest versions of all code and documentation please visit the SERL
#  GitHub page https://github.com/smartEnergyResearchLab


# Setup -------------------------------------------------------------------

library(data.table)
library(lubridate)
library(stringr)

options(datatable.fread.input.cmd.message=FALSE)

# Import all file names, locations, source function files
source("D:/Users/ucldata/observatory_release_pre_processing/observatoryData/scripts/edition02/setup_edition02.R")

# Source functions
source("D:/Users/ucldata/observatory_release_pre_processing/observatoryData/scripts/functions/smart_meter_prep_functions.R")

# Define Input Variables --------------------------------------------------

save_data <- TRUE # TRUE will overwrite any existing output csv & RData files


# Error codes -------------------------------------------------------------

# Error flags:
#   3   No read but invalid read time for another read type (ie ignore, not an issue)
#   2   No meter - or at least none we find via the DCC inventory
#   1   Valid read - a read that doesn't meet any other error flagging criteria
#   0   Missing read for an existing meter
#  -1  'Max read' - see definition in setup
#  -2  'High read' - see definition in setup
#  -3   Negative read
#  -4   Incorrect units - see 'correctUnits' code chunk
# ---

# minimum number of valid readings required to determine if daily electricity 
#  import data was incorrectly recorded in kWh
min_n_to_determine_unit_error <- 30 

# Define what half-hourly sum and daily read to be matching or similar means
elec_match_limit <- 1     # (Wh)
elec_similar_limit <- 10  # (Wh)
gas_match_limit <- 0.001  # (m^3)
gas_similar_limit <- 0.01 # (m^3)

# We try to make generous limits before classifying a read as dubiously 'high' 
#  200 amp fuse box, with 240 Volt circuit gives 48kW max electricity
#  16m^3/hr is generous for gas flow, therefore 8m^3/half hour
#  4kW is a reasonable maximum for solar PV capacity 

error_codes_hh <- data.table(
  read = c(
    "Elec_act_imp_hh_Wh",
    "Elec_react_imp_hh_varh",
    "Elec_act_exp_hh_Wh",
    "Elec_react_exp_hh_varh",
    "Gas_hh_m3"
  ),
  err_colname = c(
    "Elec_act_imp_flag",
    "Elec_react_imp_flag",
    "Elec_act_exp_flag",
    "Elec_react_exp_flag",
    "Gas_flag"
  ),
  max_err = c(16777215,
              16777215,
              16777215,
              16777215,
              16777.215),
  high_err = c(24 * 1000,
               10000,
               4000 / 2,
               10000,
               8),
  deviceType = c(rep("ESME", 4),
                 "GPF"),
  readType = c("AI",
               "RI",
               "AE",
               "RE",
               "AI")
)

error_codes_daily <- data.table(
  read = c("Elec_act_imp_d_Wh",
           "Gas_d_m3"),
  err_colname = c("Elec_act_imp_flag",
                  "Gas_flag"),
  max_err = c(16777215,
              16777.215),
  high_err = c(48 * 24 * 1000,
               8 * 48),
  deviceType = c("ESME",
                 "GPF"),
  readType = c("DL",
               "DL")
)


# Import and preprocess data ----------------------------------------------

collection_end_date <- ymd(most_recent_smart_meter_date)

inventory <- fread(inventory_file)
setnames(inventory, old = "puprn", new = "PUPRN")

participant_details <- fread(participant_details_file)

readDates <-
  determine.theoretical.read.dates(theoretical_dates_file,
                                   inventory,
                                   participant_details,
                                   collection_end_date)

ptm <- proc.time()
hh <- import.and.rbind(hh_filename, location_orig)
proc.time() - ptm # 91 seconds elapsed

ptm <- proc.time()
daily <- import.and.rbind(daily_filename, location_orig)
proc.time() - ptm # 2 seconds elapsed

# deal with integer-64 in hh data 
#  (replace 64-bit equivalent of 16777215 with 32-bit version)
hh[elec_active_import_profile_hh_wh > 16777215, 
   elec_active_import_profile_hh_wh := 16777215]
hh[, elec_active_import_profile_hh_wh := as.integer(elec_active_import_profile_hh_wh)]


# Process half-hourly data ------------------------------------------------
ptm <- proc.time()
name.hh.cols(hh)
get.meter.existence(hh, readDates)
proc.time() - ptm # 7

ptm <- proc.time()
hh <- format.hh.date.times(hh)
proc.time() - ptm # 24

ptm <- proc.time()
hh[, Valid_read_time := HH %in% seq(1:48)]
proc.time() - ptm # 1

ptm <- proc.time()
code.errors(hh, error_codes_hh)
proc.time() - ptm # 73

ptm <- proc.time()
convert.gas.hh(hh)
select.hh.cols(hh)
proc.time() - ptm # 20


# Process daily data ------------------------------------------------------
ptm <- proc.time()
name.daily.cols(daily)
format.daily.date.times(daily)
daily <- calc.hh.sums(daily, hh)
validate.daily.read.time(daily)
get.meter.existence(daily, readDates, resolution = "daily")
code.errors(daily, error_codes_daily)
convert.gas.daily(daily)
correct.elec.in.kwh(daily, min_n_to_determine_unit_error)
calc.flag.daily.sum.match(daily, elec_match_limit, elec_similar_limit,
                          gas_match_limit, gas_similar_limit)
select.daily.cols(daily)
proc.time() - ptm # 35 seconds elapsed


# Save half-hourly and daily datasets -------------------------------------

ptm <- proc.time()
if(save_data == TRUE) {
  
  daily_saving_name <- get.serl.filename("daily_data", release_version)
  fwrite(daily, file = paste(location_processed, daily_saving_name, ".csv", sep =  ""))
  save(daily, file = paste(location_RData, daily_saving_name, ".RData", sep = ""))
  
  hh_saving_name <- get.serl.filename("hh_data", release_version)
  fwrite(hh, file = paste(location_processed, hh_saving_name, ".csv", sep =  ""))
  save(hh, file = paste(location_RData, hh_saving_name, ".RData", sep = ""))
  
}
proc.time() - ptm # 8.25 minutes


# Read-type summary -------------------------------------------------------

ptm <- proc.time()
rt_summary <-
  create.read.type.summary(hh, daily, readDates, error_codes_hh, error_codes_daily)
proc.time() - ptm # 30 seconds elapsed

ptm <- proc.time()
rt_summary <-
  get.valid.read.dates(hh,
                       daily,
                       readDates,
                       error_codes_hh,
                       error_codes_daily,
                       rt_summary)
proc.time() - ptm # 26 seconds elapsed

ptm <- proc.time()
correct.theoretical.start(rt_summary)
calc.error.percentages(rt_summary)
proc.time() - ptm # 0 seconds elapsed

ptm <- proc.time()
rt_summary <- get.read.type.stats()
proc.time() - ptm # 93 s elapsed

reorder.rt_summary.cols(rt_summary)


# Save read type summary --------------------------------------------------

ptm <- proc.time()
if(save_data == TRUE) {
  
  rt_saving_name <- get.serl.filename("rt_data", release_version)
  
  fwrite(rt_summary,
         file = paste(location_processed, rt_saving_name, ".csv", sep =  ""))
  
  save(rt_summary,
       file = paste(location_RData, rt_saving_name, ".RData", sep = ""))
  
}
proc.time() - ptm # 0.3 seconds elapsed



# Participant-level summary -----------------------------------------------

ptm <- proc.time()
participant_summary <- create.participant.summary(rt_summary, 
                                                  survey_file, 
                                                  participant_details, 
                                                  epc_file)
proc.time() - ptm # 1 second elapsed


# Save participant-level summary  -----------------------------------------

ptm <- proc.time()
if(save_data == TRUE) {
  
  pp_summary_saving_name <-
    get.serl.filename("ps_data", release_version)
  
  fwrite(
    participant_summary,
    file = paste(location_processed, pp_summary_saving_name, ".csv", sep =  "")
  )
  
  save(
    participant_summary,
    file = paste(location_RData, pp_summary_saving_name, ".RData", sep =  "")
  )
  
}
proc.time() - ptm # 0 seconds elapsed



# Prep for documentation --------------------------------------------------

ptm <- proc.time()

# PUPRN stats/variables
all_puprn <- unique(rt_summary$PUPRN)
n_no_elec <-
  length(all_puprn[!(all_puprn %in% rt_summary[deviceType == "ESME", PUPRN])])
n_no_gas <-
  length(all_puprn[!(all_puprn %in% rt_summary[deviceType == "GPF", PUPRN])])
n_pp <- length(all_puprn)


# Half-hourly stats/variables
first_read_date_hh <- rt_summary[readType != "DL",
                                 min(firstValidReadDate, na.rm = TRUE)]

last_read_date_hh <- rt_summary[readType != "DL",
                                max(lastValidReadDate, na.rm = TRUE)]

n_hh_p <- length(unique(hh$PUPRN))
nrow_hh <- nrow(hh)
ncol_hh <- ncol(hh)

hh_colnames <- colnames(hh)
hh_col_class <- lapply(hh[1:10], class)


# Daily stats/variables
first_read_date_d <- rt_summary[readType == "DL", 
                                min(firstValidReadDate, na.rm = TRUE)]

last_read_date_d <- rt_summary[readType == "DL", 
                               max(lastValidReadDate, na.rm = TRUE)]

n_d_p <- length(unique(daily$PUPRN))
nrow_d <- nrow(daily)
ncol_d <- ncol(daily)

d_colnames <- colnames(daily)
d_col_class <- lapply(daily[1:10], class)


# Read-type summary stats/variables
nrow_reads <- nrow(rt_summary)
ncol_reads <- ncol(rt_summary)

rt_colnames <- colnames(rt_summary)
rt_col_class <- lapply(rt_summary[1:10], class)

read.type.tab <- rt_summary[valid > 0 | wrongUnits > 0, 
                            .N, 
                            keyby = c("deviceType", "readType")]


# Participant summary stats/variables
nrow_pp <- nrow(participant_summary)
ncol_pp <- ncol(participant_summary)

p_colnames <- colnames(participant_summary)
p_col_class <- lapply(participant_summary[1:10], class)

proc.time() - ptm # 2 seconds

# Save 

ptm <- proc.time()
save(
  first_read_date_hh,
  last_read_date_hh,
  n_hh_p,
  nrow_hh,
  ncol_hh,
  first_read_date_d,
  last_read_date_d,
  n_d_p,
  nrow_d,
  ncol_d,
  nrow_reads,
  ncol_reads,
  nrow_pp,
  ncol_pp,
  hh_colnames,
  hh_col_class,
  d_colnames,
  d_col_class,
  rt_colnames,
  rt_col_class,
  p_colnames,
  p_col_class,
  read.type.tab,
  all_puprn,
  n_no_elec,
  n_no_gas,
  n_pp,
  file = sm_doc_input_file
)
proc.time() - ptm # 0 seconds elapsed
