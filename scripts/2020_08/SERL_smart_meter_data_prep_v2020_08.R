# This code prepares the half-hourly and daily smart meter data from 
#  August 2018 - July 2020 as part of the August 2020 SERL Observatory 
#  data release. 

# Code developed by Ellen Webborn, UCL using R version 3.6.0 (2019-09-01)
# Most recent edits made 2020-09-07

# For the latest versions of all code and documentation please visit the SERL
#  GitHub page https://github.com/smartEnergyResearchLab


# Setup -------------------------------------------------------------------

library(data.table)
library(lubridate)
library(stringr)

options(datatable.fread.input.cmd.message=FALSE)

# Import all filenames, locations, source function files
source("N:/R/observatoryData/scripts/setup_v2020_08.R")


# Define Input Variables --------------------------------------------------

save_data <- FALSE # TRUE will overwrite any existing output csv & RData files


# Error codes -------------------------------------------------------------

# Error flags:
#   2   No meter - or at least none we find via the DCC inventory
#   1   Valid read - a read that doesn't meet any other error flagging criteria
#   0   Missing read for an existing meter
#  -1  'Max read' - see definition in setup
#  -2  'High read' - see definition in setup
#  -3  Negative read
#  -4  Incorrect units - see 'correctUnits' code chunk
#  -5  Incorrect read time (trumps all others except meter doesn't exist)
# ---

# minimum number of valid readings required to determine if daily electricity 
#  import data was incorrectly recorded in kWh
min_n_to_determine_unit_error <- 30 

# Define what half-hourly sum and daily read to be matching or similar means
elec_match_limit <- 1     # (Wh)
elec_similar_limit <- 10  # (Wh)
gas_match_limit <- 0.001  # (m^3)
gas_similar_limit <- 0.01 # (m^3)

err_code_values <- -5:2
n_err_codes <- length(err_code_values)

error_codes_hh <- data.table(read = c("Elec_act_imp_hh_Wh",
                                      "Elec_react_imp_hh_varh",
                                      "Elec_act_exp_hh_Wh",
                                      "Elec_react_exp_hh_varh",
                                      "Gas_hh_m3"),
                             err_colname = c("Elec_act_imp_flag",
                                             "Elec_react_imp_flag",
                                             "Elec_act_exp_flag",
                                             "Elec_react_exp_flag",
                                             "Gas_flag"),
                              max_err = c(16777215, 
                                          16777215, 
                                          16777215,
                                          16777215, 
                                          16777.215),
                              high_err = c(1000000,
                                           10000,
                                           5000,
                                           10000,
                                           1000),
                             deviceType = c(rep("ESME", 4), 
                                            "GPF"),
                             readType = c("AI",
                                          "RI",
                                          "AE",
                                          "RE",
                                          "AI")
                          )


error_codes_daily <- data.table(read = c("Elec_act_imp_d_Wh",
                                      "Gas_d_m3"),
                             err_colname = c("Elec_act_imp_flag",
                                             "Gas_flag"),
                             max_err = c(16777215, 
                                         16777.215),
                             high_err = c(1000000,
                                          1000),
                             deviceType = c("ESME", 
                                            "GPF"),
                             readType = c("DL",
                                          "DL")
                             )


# FunctionDefinitions ---------------------------------------------------------------

attach.EPC.basic.info <- function(participant_summary, epc_file) {
  EPC <- fread(epc_file)
  setnames(EPC, "puprn", "PUPRN")
  setkey(EPC, PUPRN)
  participant_summary <- EPC[, .(PUPRN, current_energy_rating)][participant_summary]
  participant_summary[, EPC_exists := TRUE]
  participant_summary[is.na(current_energy_rating), EPC_exists := FALSE]
  setnames(participant_summary, "current_energy_rating", "EPC_rating")
  return(participant_summary)
}

attach.participant.info <- function(participant_summary, participant_details_file) {
  participant_details <- fread(participant_details_file)
  setkey(participant_details, PUPRN)
  participant_summary <- participant_details[, .(PUPRN, Region, IMD_quintile, LSOA, grid_cell)][participant_summary]
  return(participant_summary)
}

attach.survey.data <- function(participant_summary, survey_file) {
  load(survey_file)
  setkey(survey_clean, Respondent_ID)
  survey_clean[, N_required := 30 + 
                 (A302 > 0 | A303 > 0 | A304 > 0 | A305 > 0 | A306 > 0 |
                    A307 > 0 | A308 > 0 | A309 > 0 | A310 > 0 | A3_Other > 0) +
                 (A402 > 0 |A403 > 0) + 
                 (A6 == 1) + 
                 (A7 == 1) + 
                 (B2 == 2) +
                 (C5 == 1) +
                 (C1 > 1) * 3
               ]
  
  survey_clean[, N_answered := (A1 > 0) + (A2 > -2) + (A3_sum > 0) + (A4_sum > 0) + (A5 > -2) + 
                 (A6 > 0) + (A7 > 0) + (A8 > -2) + (A9_sum > 0) + (A10 > 0) + (A11 > 0) +
                 (A12_Taps_sum > 0) + (A12_Shower_sum > 0) + (A13_01 > 0) + (A13_02 > 0) + 
                 (A14 > -2) + (A1501 > -2) + (A1502 > -2) + (A16_sum > 0) + 
                 (B1 > 0) + (B2 > 0) + (B3 > 0) + (B4 > 0) + (B5> 0) + (B6 > 0) + (B7 > -2) + 
                 (B8 > -2) + (B9 > -2) + (B10_sum > 0) + 
                 (C1 > 0) + (C2_sum > 0) + (C3_sum > 0) + (C4 > -2) + (C5 > -2) + (C6 > -2) + 
                 (D1 > 0) + (D2 %in% c(-1, 1, 2, 3)) + (D3 > 0 | D3 == -3) + (D4 != -2)
               ]
  
  survey_clean[, Perc_answered := round(N_answered / N_required * 100, 2)]
  
  setnames(survey_clean, "Respondent_ID", "PUPRN")
  
  pp_summary <- survey_clean[, list(PUPRN, N_answered, Perc_answered)][participant_summary]
  return(pp_summary)
}

calc.daily.sums.from.hh <- function(hh) {
  # Elec active import
  ## Sum the valid reads for each PUPRN for each day
  valid_elec_imp_reads <- hh[Elec_act_imp_flag == 1, 
                             sum(Elec_act_imp_hh_Wh), 
                             keyby = .(PUPRN, Read_date_effective)]
  setnames(valid_elec_imp_reads, old = "V1", new = "Elec_act_imp_hh_sum_Wh")
  
  ## Count how many valid reads exist for each day for each PUPRN
  n_valid_elec_imp_reads <- hh[Elec_act_imp_flag == 1, 
                               .N, 
                               keyby = .(PUPRN, Read_date_effective)]
  
  hh_elec_sums <- n_valid_elec_imp_reads[valid_elec_imp_reads]
  setnames(hh_elec_sums, old = "N", new = "N_elec_hh")
  
  # Gas import
  ## Sum the valid reads for each PUPRN for each day
  valid_gas_imp_reads <- hh[Gas_flag == 1, 
                            sum(Gas_hh_m3), 
                            keyby = .(PUPRN, Read_date_effective)]
  setnames(valid_gas_imp_reads, old = "V1", new = "Gas_hh_sum_m3")
  
  ## Count how many valid reads exist for each day for each PUPRN
  n_valid_gas_imp_reads <- hh[Gas_flag == 1, 
                              .N, 
                              keyby = .(PUPRN, Read_date_effective)]
  
  hh_gas_sums <- n_valid_gas_imp_reads[valid_gas_imp_reads]
  setnames(hh_gas_sums, old = "N", new = "N_gas_hh")
  
  return(list(hh_elec_sums, hh_gas_sums))
}

calc.error.percentages <- function(rt_summary) {
  rt_summary[, percValid := round(f_1 / maxPossReads * 100, 2)]
  rt_summary[, percMissing := round(f_0 / maxPossReads * 100, 2)]
  rt_summary[, percError := round(100 - percValid - percMissing, 2)]
  rt_summary[, percValidOrUnitError := round((f_1 + f_minus4) / maxPossReads * 100, 2)] 
}

calc.hh.sums.flag.mismatch <- function(daily, hh) {
  valid_sums <- calc.daily.sums.from.hh(hh)
  setkeyv(daily, c("PUPRN", "Read_date_effective"))
  daily <- valid_sums[[1]][daily]
  daily <- valid_sums[[2]][daily]
  daily[is.na(N_elec_hh), N_elec_hh := 0]
  daily[is.na(N_gas_hh), N_gas_hh := 0]
  daily[N_elec_hh != 48, Elec_act_imp_hh_sum_Wh := NA]
  daily[N_gas_hh != 48, Gas_hh_sum_m3 := NA]
  daily[, Elec_act_imp_sum_diff := round(Unit_correct_elec_act_imp_d_Wh - Elec_act_imp_hh_sum_Wh, 3)]
  daily[, Gas_sum_diff := round(Gas_d_m3 - Gas_hh_sum_m3, 3)]
  return(daily)
}

code.errors <- function(sm.data, error_codes) {
  for(i in 1:nrow(error_codes)) {
    # initially code all reads as valid
    sm.data[, eval(error_codes[i, err_colname]) := 1] 
    # missing data
    sm.data[is.na(get(error_codes[i, read])), 
            eval(error_codes[i, err_colname]) := 0] 
    # 'max read error'
    sm.data[get(error_codes[i, read]) >= error_codes[i, max_err], 
            eval(error_codes[i, err_colname]) := -1] 
    # High read error
    sm.data[get(error_codes[i, read]) < error_codes[i, max_err] & 
              get(error_codes[i, read]) >= error_codes[i, high_err], 
            eval(error_codes[i, err_colname]) := -2]
    # negative read error
    sm.data[get(error_codes[i, read]) < 0, 
            eval(error_codes[i, err_colname]) := -3]
    # incorrect read time
    sm.data[Valid_read_time == FALSE, eval(error_codes[i, err_colname]) := -5]
    
    # ESME or GPF doesn't exist/isn't registered 
    if(stringr::str_detect(error_codes[i, read], "imp") == TRUE) {
      sm.data[Elec_import_exists == FALSE, eval(error_codes[i, err_colname]) := 2]
    } else if(stringr::str_detect(error_codes[i, read], "Gas") == TRUE) {
      sm.data[Gas_exists == FALSE, eval(error_codes[i, err_colname]) := 2]
    } else {
      sm.data[Elec_export_exists == FALSE, eval(error_codes[i, err_colname]) := 2]
    }
    
  }
  return(sm.data)
}

convert.elec.daily <- function(daily) {
  # requires doing unit correction first
  daily[, Elec_act_imp_d_kWh := Unit_correct_elec_act_imp_d_Wh / 1000]
  return(daily)
}

convert.gas.daily <- function(daily) {
  daily[, Gas_d_kWh := round(convert.m3.kwh(Gas_d_m3), 3)]
}

convert.gas.hh <- function(hh.data) {
  hh.data[, Gas_hh_kWh := round(convert.m3.kwh(Gas_hh_m3), 3)]
  hh.data[, Gas_hh_Wh := Gas_hh_kWh * 1000]
}

convert.m3.kwh <- function(m3, CV = 39.5) {
  # converts volume of gas in m3 to energy in kWh (estimate)
  kWh <- m3 * 1.02264 * CV / 3.6
}

correct.elec.in.kwh <- function(daily, min_n_to_determine_unit_error) {
  max_elec_reads <- daily[Elec_act_imp_flag == 1, max(Elec_act_imp_d_Wh), keyby = PUPRN]
  id_kWh <- max_elec_reads[V1 < 100, PUPRN]
  n_elec_reads_suspected_kWh <- daily[Elec_act_imp_flag == 1 & PUPRN %in% id_kWh, .N, keyby = PUPRN]
  id_kWh_sufficient_N <- n_elec_reads_suspected_kWh[N >= min_n_to_determine_unit_error]
  # change error code from valid (1) to unit error (-4)
  daily[PUPRN %in% id_kWh_sufficient_N$PUPRN & Elec_act_imp_flag == 1, Elec_act_imp_flag := -4]
  # create new column for unit-corrected (or original) data
  daily[, Unit_correct_elec_act_imp_d_Wh := Elec_act_imp_d_Wh]
  daily[Elec_act_imp_flag == -4, Unit_correct_elec_act_imp_d_Wh := Elec_act_imp_d_Wh * 1000]
  return(daily)
}

correct.rt.perc.out.of.date.range <- function(rt_summary) {
  # Correct theoretical start if first valid read date is earlier. 
  #  Suspect it's due to meter replacement and we don't have details of
  #  previous meter. 
  
  rt_summary[theoreticalStart > firstValidReadDate, 
             theoreticalStart := firstValidReadDate]
  
  return(rt_summary)
}

correct.theoretical.start <- function(rt_summary) {
  # sometimes a meter gets replaced and we didn't know about the previous one
  #  therefore we get readings sooner than we expected
  #  current approach is to adjust the theoretical start date
  rt_summary[firstValidReadDate < theoreticalStart, 
             `:=`(theoreticalStart = firstValidReadDate,
                  daysRange = as.integer(theoreticalEnd - theoreticalStart) + 1,
                  maxPossReads = daysRange * 48)]
  rt_summary[readType == "DL", maxPossReads := daysRange]
  return(rt_summary)
}

create.participant.summary <- function(rt_summary, survey_file, participant_details_file, epc_file) {
  tmp_poss_reads <- copy(rt_summary[, .(PUPRN,
                                        deviceType,
                                        readType,
                                        firstValidReadDate,
                                        lastValidReadDate,
                                        f_1,
                                        percValid)])
  setnames(tmp_poss_reads, 
           old = c("firstValidReadDate", "lastValidReadDate", "f_1", "percValid"), 
           new = c("Start", "End", "NumValid", "PercValid")
  )
  
  tmp_poss_reads[readType == "AI" & deviceType == "ESME", readType := "HH_Act_Im"]
  tmp_poss_reads[readType == "RI", readType := "HH_React_Im"]
  tmp_poss_reads[readType == "AE", readType := "HH_Act_Ex"]
  tmp_poss_reads[readType == "RE", readType := "HH_React_Ex"]
  tmp_poss_reads[readType == "DL" & deviceType == "ESME", readType := "D_Act_Im"]
  tmp_poss_reads[readType == "DL" & deviceType == "GPF", readType := "D_Im"]
  tmp_poss_reads[readType == "AI" & deviceType == "GPF", readType := "HH_Im"]
  
  ## convert from long to wide 
  participant_summary <- dcast(tmp_poss_reads, PUPRN ~ deviceType + readType,  
                               value.var = c( "Start", "End", "NumValid", "PercValid"))
  
  ## hh invalid read times
  count_invalid_read_times <- hh[Valid_read_time == FALSE, .N, keyby = PUPRN]
  colnames(count_invalid_read_times) <- c("PUPRN", "invalidReadTimes_HH")
  
  participant_summary <- count_invalid_read_times[participant_summary]
  participant_summary[is.na(invalidReadTimes_HH), invalidReadTimes_HH := 0]
  
  ## daily invalid read times
  count_invalid_read_times <- daily[Valid_read_time == FALSE, .N, keyby = PUPRN]
  colnames(count_invalid_read_times) <- c("PUPRN", "invalidReadTimes_D")
  
  participant_summary <- count_invalid_read_times[participant_summary]
  participant_summary[is.na(invalidReadTimes_D), invalidReadTimes_D := 0]
  
  participant_summary <- attach.survey.data(participant_summary, survey_file)
  participant_summary <- attach.participant.info(participant_summary, participant_details_file)
  participant_summary <- attach.EPC.basic.info(participant_summary, epc_file)
  
  setnames(participant_summary, 
           old = c("Region",
                   "grid_cell",
                   "IMD_quintile",
                   "EPC_exists",
                   "EPC_rating",
                   "N_answered", 
                   "Perc_answered"),
           new = c("region",
                   "gridCell",
                   "imdQuintile",
                   "epcExists",
                   "epcRating",
                   "numSurveyAns", 
                   "percSurveyAns")
  )
  
  setcolorder(participant_summary, c("PUPRN", 
                                     "region",
                                     "LSOA",
                                     "gridCell",
                                     "imdQuintile",
                                     "epcExists",
                                     "epcRating",
                                     "numSurveyAns", 
                                     "percSurveyAns",
                                     "invalidReadTimes_D",
                                     "invalidReadTimes_HH",
                                     "Start_ESME_D_Act_Im",
                                     "End_ESME_D_Act_Im",
                                     "NumValid_ESME_D_Act_Im",
                                     "PercValid_ESME_D_Act_Im",
                                     "Start_ESME_HH_Act_Im",
                                     "End_ESME_HH_Act_Im",
                                     "NumValid_ESME_HH_Act_Im",
                                     "PercValid_ESME_HH_Act_Im",
                                     "Start_ESME_HH_Act_Ex",
                                     "End_ESME_HH_Act_Ex",
                                     "NumValid_ESME_HH_Act_Ex",
                                     "PercValid_ESME_HH_Act_Ex",
                                     "Start_ESME_HH_React_Im",
                                     "End_ESME_HH_React_Im",
                                     "NumValid_ESME_HH_React_Im",
                                     "PercValid_ESME_HH_React_Im",
                                     "Start_ESME_HH_React_Ex",
                                     "End_ESME_HH_React_Ex",
                                     "NumValid_ESME_HH_React_Ex",
                                     "PercValid_ESME_HH_React_Ex",
                                     "Start_GPF_D_Im",
                                     "End_GPF_D_Im",
                                     "NumValid_GPF_D_Im",
                                     "PercValid_GPF_D_Im",
                                     "Start_GPF_HH_Im",
                                     "End_GPF_HH_Im",
                                     "NumValid_GPF_HH_Im",
                                     "PercValid_GPF_HH_Im")
  )
  
}

create.read.type.summary <- function(hh, daily, readDates, error_codes_hh, error_codes_daily) {
  n_hh_error_types <- nrow(error_codes_hh)
  n_daily_error_types <- nrow(error_codes_daily)
  n_error_types <- n_hh_error_types + n_daily_error_types
  
  tmp.list <- vector(mode = "list", length = n_error_types)
  
  for(i in 1:n_hh_error_types) {
    tmp.list[[i]] <- sum.errors(dt = hh,
                                error_colname = error_codes_hh[i, err_colname],
                                dType = error_codes_hh[i, deviceType],
                                rType = error_codes_hh[i, readType])
  }
  for(i in 1:n_daily_error_types) {
    tmp.list[[n_hh_error_types + i]] <- sum.errors(dt = daily,
                                                   error_colname = error_codes_daily[i, err_colname],
                                                   dType = error_codes_daily[i, deviceType],
                                                   rType = error_codes_daily[i, readType])    
  }
  error_summary <- rbindlist(tmp.list, fill=TRUE)
  setkeyv(error_summary, c("PUPRN", "deviceType", "readType"))
  rt_summary <- error_summary[readDates]
  rt_summary <- replace.na.in.data.table(rt_summary)
  err_count_colnames <- c("f_minus5", "f_minus4", "f_minus3", "f_minus2", "f_minus1", "f_0", "f_1")
  setnames(rt_summary, 
           old = c("-5", "-4", "-3", "-2", "-1", "0", "1"), 
           new = err_count_colnames, 
           skip_absent = TRUE)
  rt_cols <- colnames(rt_summary)
  for(i in 1:length(err_count_colnames)) {
    if(!(err_count_colnames[i] %in% rt_cols)) {
      rt_summary[, eval(err_count_colnames[i]) := 0]
    }
  }
  rt_summary[, f_0 := maxPossReads - f_1 - f_minus1 - f_minus2 - f_minus3 - f_minus4 - f_minus5]
  setnames(rt_summary, "2", "tmp")
  rt_summary[, tmp := NULL]
  return(rt_summary)
}

determine.theoretical.read.dates <- function(theoretical_dates_file, 
                                             inventory,
                                             participant_details,
                                             collection_end_date) {
  theoretical_dates <- fread(theoretical_dates_file)
  setnames(theoretical_dates, "puprn", "PUPRN", skip_absent = TRUE)
  theoretical_dates <- handle.duplicate.listings(theoretical_dates)
  decommission_dates <- get.decommission.dates(inventory)
  consent_end_dates <- participant_details[, .(PUPRN, WoC_CoT_effective_date)]
  consent_end_dates[, WoC_CoT_effective_date := as.Date(WoC_CoT_effective_date,
                                                        format = "%d/%m/%Y")]
  setkeyv(theoretical_dates, c("PUPRN", "deviceType"))
  setkeyv(decommission_dates, c("PUPRN", "deviceType"))
  setkey(consent_end_dates, PUPRN)
  theoretical_dates <- decommission_dates[theoretical_dates]
  theoretical_dates <- consent_end_dates[theoretical_dates]
  theoretical_dates[, theoreticalStart := ymd(start)]
  theoretical_dates[, theoreticalEnd := pmin(WoC_CoT_effective_date - 1,
                                             dateDecommissioned,
                                             collection_end_date,
                                             na.rm = TRUE)]

  theoretical_dates <- theoretical_dates[, .(PUPRN, deviceType, readType,
                                             theoreticalStart, theoreticalEnd)]
  
  #remove rows for devices that we only started collection from after the collection_end_date
  theoretical_dates <- theoretical_dates[theoreticalStart <= collection_end_date, ]
  # deal with export read types not treated separately
  export_dates <- copy(theoretical_dates[readType == "EX", ])
  export_dates <- rbind(export_dates, export_dates)
  half_n_export_rows <- nrow(export_dates)/2
  export_dates[1:half_n_export_rows, readType := "AE"]
  export_dates[(half_n_export_rows + 1):(2*half_n_export_rows), readType := "RE"]
  theoretical_dates_tmp <- theoretical_dates[readType != "EX"]
  theoretical_dates <- rbind(export_dates, theoretical_dates_tmp)
  setkeyv(theoretical_dates, c("PUPRN", "deviceType", "readType"))
  
  theoretical_dates[, daysRange := as.integer(theoreticalEnd - theoreticalStart) + 1]
  theoretical_dates[, maxPossReads := daysRange * 48]
  theoretical_dates[readType == "DL", maxPossReads := daysRange]
  
  return(theoretical_dates)
}

find.first.last.valid.dates <- function(dt, error_flag, dType, rType) {
  valid_start_dates <- dt[get(error_flag) == 1, min(Read_date_effective, na.rm = TRUE), 
                          keyby = PUPRN]
  setnames(valid_start_dates, "V1", "firstValidReadDate")
  
  valid_end_dates <- hh[get(error_flag) == 1, max(Read_date_effective, na.rm = TRUE), 
                        keyby = PUPRN]
  setnames(valid_end_dates, "V1", "lastValidReadDate")
  
  valid_start_dates[, `:=`(deviceType = dType,
                           readType = rType)]
  
  valid_end_dates[, `:=`(deviceType = dType,
                         readType = rType)]
  
  output <- list(valid_start_dates, valid_end_dates)
  return(output)
}

flag.daily.sum.match <- function(daily, elec_match_limit, elec_similar_limit, 
                                 gas_match_limit, gas_similar_limit) {
  # Flag if the daily electricity reading matches or is similar to (defined in setup) 
  #  the sum of the half-hourly reads for that day. 
  
  ## setup all with code 999 in order to catch any types of matching missed
  daily[, Elec_sum_match := 999]
  
  ## No electricity meter
  daily[Elec_import_exists == FALSE, 
        Elec_sum_match := 2] # No meter
  
  ## not possible to compare as don't have 48 valid hh reads and/or a valid daily read
  daily[Elec_sum_match == 999 & (Elec_act_imp_flag != 1 | N_elec_hh != 48), 
        Elec_sum_match := 0]
  
  ## Meter read recorded in kWh so comparison ignored
  daily[Elec_act_imp_flag == -4, 
        Elec_sum_match := 3] 
  
  ## Sum and daily read match (within elec_match_limit)
  daily[Elec_sum_match == 999 & abs(Elec_act_imp_sum_diff) <= elec_match_limit, 
        Elec_sum_match := 1] 
  
  ## Similar but don't match
  daily[Elec_sum_match == 999 & abs(Elec_act_imp_sum_diff) <= elec_similar_limit,
        Elec_sum_match := -1] 
  
  ## Don't match and not similar despite all relevant readings being valid
  daily[Elec_sum_match == 999 & abs(Elec_act_imp_sum_diff) > elec_similar_limit, 
        Elec_sum_match := -2]
  
  
  # Flag if the daily gas reading matches or is similar to (defined in setup) 
  #  the sum of the half-hourly reads for that day. 
  
  ## Setup all with code 999 in order to catch any types of matching missed
  daily[, Gas_sum_match := 999]
  
  ## No gas meter
  daily[Gas_exists == FALSE, 
        Gas_sum_match := 2] 
  
  ## not possible to compare as don't have 48 valid hh reads and/or a valid daily read
  daily[Gas_sum_match == 999 & (Gas_flag != 1 | N_gas_hh != 48), 
        Gas_sum_match := 0] 
  
  ## Sum and daily read match (within gas_match_limit defined in setup)
  daily[Gas_sum_match == 999 & abs(Gas_sum_diff) <= gas_match_limit, 
        Gas_sum_match := 1] 
  
  ## Similar but don't match
  daily[Gas_sum_match == 999 & abs(Gas_sum_diff) <= gas_similar_limit, 
        Gas_sum_match := -1]
  
  ## Don't match and not similar despite all relevant readings being valid
  daily[Gas_sum_match == 999 & abs(Gas_sum_diff) > gas_similar_limit, 
        Gas_sum_match := -2]
  
  return(daily)
}

format.date.times <- function(sm_data, resolution = "hh") {
  # get date format for date columns, dealing with different date formats
  sm_data[, Read_date_effective := as.Date(Read_date_effective, 
                                           format = "%Y/%m/%d")]
  
  sm_data[, Read_date_time := lubridate::ymd_hms(Read_date_time)]
  sm_data[, Read_date_time_effective := NULL]
  
  if(resolution == "hh") {
    hh_unique_times <- unique(sm_data, by = "Read_date_time")
    hh_unique_times[, Read_date_time := ymd_hms(Read_date_time)]
    
    hh_unique_times <- data.table(Read_date_time = hh_unique_times$Read_date_time)
    
    setkey(sm_data, Read_date_time)
    setkey(hh_unique_times, Read_date_time)
    
    sm_data <- hh_unique_times[sm_data]
    setkey(sm_data, PUPRN)
    setcolorder(sm_data, c("PUPRN", "Read_date_effective", "Read_date_time"))
  }
  
  return(sm_data)
}

get.decommission.dates <- function(inventory) {
  # for each PUPRN-deviceType, check if there are multiple entries (device replacement)
  # create table of decommission dates for each PUPRN-deviceType
  inventory[dateDecommissioned == "", dateDecommissioned := NA_character_]
  multi_entries <- inventory[, .N, keyby = list(PUPRN, deviceType)][N > 1]
  n_multi <- nrow(multi_entries)
  multi_resolved <- data.table(PUPRN = multi_entries$PUPRN,
                               deviceType = multi_entries$deviceType,
                               dateDecommissioned = rep(NA_character_, n_multi))
  for(i in 1:n_multi) {
    multi_resolved[i, dateDecommissioned := inventory[PUPRN == multi_resolved[i, PUPRN] &
                                                        deviceType == multi_resolved[i, deviceType],
                                                      max(dateDecommissioned)]]
  }
  
  multi_resolved <- rbind(multi_resolved, 
                          inventory[!PUPRN %in% multi_entries$PUPRN, 
                                    .(PUPRN, deviceType, dateDecommissioned)])
  multi_resolved[, dateDecommissioned := as.Date(dateDecommissioned,
                                                 format = "%Y/%m/%d")]
  return(multi_resolved)
}

get.meter.existence <- function(sm_data, sm_starts, resolution = "hh") {
  # Note: assuming that if half-hourly exists then daily exists
  sm_data[, Elec_import_exists := PUPRN %in% sm_starts[deviceType == "ESME" &
                                                         readType == "AI", PUPRN]] # Active import
  if (resolution == "hh") {
    sm_data[, Elec_export_exists := PUPRN %in% sm_starts[deviceType == "ESME" &
                                                           readType == "AE", PUPRN]] # Active export
  }
  sm_data[, Gas_exists := PUPRN %in% sm_starts[deviceType == "GPF", PUPRN]]
  return(sm_data)
}

get.read.type.stats <- function(hh, daily, rt_summary, error_codes_hh, error_codes_daily) {
  n_hh_read_types <- nrow(error_codes_hh)
  n_daily_read_types <- nrow(error_codes_daily)
  tmp.list <- vector(mode = "list", n_hh_read_types + n_daily_read_types)
  
  # half-hourly 
  for(i in 1:n_hh_read_types) {
    tmp.list[[i]] <- hh[get(error_codes_hh[i, err_colname]) == 1,
                        as.double(min(get(error_codes_hh[i, read]), na.rm = TRUE)),
                        keyby = PUPRN]
    colnames(tmp.list[[i]]) <- c("PUPRN", "minValidRead")
    
    tmp.list[[i]][, `:=`(maxValidRead = hh[get(error_codes_hh[i, err_colname]) == 1,
                                           as.double(max(get(error_codes_hh[i, read]), na.rm = TRUE)),
                                           keyby = PUPRN]$V1,
                         meanValidRead = hh[get(error_codes_hh[i, err_colname]) == 1,
                                            round(as.double(mean(get(error_codes_hh[i, read]), na.rm = TRUE), 2)),
                                            keyby = PUPRN]$V1,
                         medianValidRead = hh[get(error_codes_hh[i, err_colname]) == 1,
                                              round(as.double(median(get(error_codes_hh[i, read]), na.rm = TRUE)), 2),
                                              keyby = PUPRN]$V1,
                         sdValidRead = hh[get(error_codes_hh[i, err_colname]) == 1,
                                          round(as.double(sd(get(error_codes_hh[i, read]), na.rm = TRUE), 2)),
                                          keyby = PUPRN]$V1,
                         deviceType = rep(error_codes_hh[i, deviceType], nrow(tmp.list[[i]])),
                         readType = rep(error_codes_hh[i, readType], nrow(tmp.list[[i]]))
    )
    ]
  }
  # daily
  for(i in 1:n_daily_read_types) {
    j <- i + n_hh_read_types
    tmp.list[[j]] <- hh[get(error_codes_hh[i, err_colname]) == 1,
                        as.double(min(get(error_codes_hh[i, read]), na.rm = TRUE)),
                        keyby = PUPRN]
    colnames(tmp.list[[i + n_hh_read_types]]) <- c("PUPRN", "minValidRead")
    tmp.list[[j]][, `:=`(maxValidRead = hh[get(error_codes_hh[i, err_colname]) == 1,
                                           as.double(max(get(error_codes_hh[i, read]), na.rm = TRUE)),
                                           keyby = PUPRN]$V1, 
                         meanValidRead = hh[get(error_codes_hh[i, err_colname]) == 1,
                                            round(as.double(mean(get(error_codes_hh[i, read]), na.rm = TRUE), 2)),
                                            keyby = PUPRN]$V1,
                         medianValidRead = hh[get(error_codes_hh[i, err_colname]) == 1,
                                              round(as.double(median(get(error_codes_hh[i, read]), na.rm = TRUE)), 2),
                                              keyby = PUPRN]$V1,
                         sdValidRead = hh[get(error_codes_hh[i, err_colname]) == 1,
                                          round(as.double(sd(get(error_codes_hh[i, read]), na.rm = TRUE), 2)),
                                          keyby = PUPRN]$V1,
                         deviceType = rep(error_codes_daily[i, deviceType], nrow(tmp.list[[j]])),
                         readType = rep(error_codes_daily[i, readType], nrow(tmp.list[[j]]))
    )
    ]
  }
  read_stats <- rbindlist(tmp.list, fill=TRUE)
  setkeyv(read_stats, c("PUPRN", "deviceType", "readType"))
  rt_summary <- read_stats[rt_summary]
  return(rt_summary)
}

get.valid.read.dates <- function(hh, daily, readDates, error_codes_hh, error_codes_daily,
                                 rt_summary) {
  n_hh_error_types <- nrow(error_codes_hh)
  n_daily_error_types <- nrow(error_codes_daily)
  n_error_types <- n_hh_error_types + n_daily_error_types
  
  start.list <- vector(mode = "list", length = n_error_types)
  end.list <- vector(mode = "list", length = n_error_types)
  
  #HH
  for(i in 1:n_hh_error_types) {
    tmp <- find.first.last.valid.dates(hh, error_codes_hh[i, err_colname],
                                       error_codes_hh[i, deviceType], 
                                       error_codes_hh[i, readType])
    start.list[[i]] <- tmp[[1]]
    end.list[[i]] <- tmp[[2]]
    
  }
  #Daily
  for(i in 1:n_daily_error_types) {
    tmp <- find.first.last.valid.dates(daily, error_codes_daily[i, err_colname],
                                       error_codes_daily[i, deviceType], 
                                       error_codes_daily[i, readType])
    start.list[[n_hh_error_types + i]] <- tmp[[1]]   
    end.list[[n_hh_error_types + i]] <- tmp[[2]]
  }
  
  starts_combined <- rbindlist(start.list)
  ends_combined <- rbindlist(end.list)
  key_cols <- c("PUPRN", "deviceType", "readType")
  setkeyv(starts_combined, key_cols)
  setkeyv(ends_combined, key_cols)
  setkeyv(rt_summary, key_cols)
  
  rt_summary <- starts_combined[rt_summary]
  rt_summary <- ends_combined[rt_summary]
  return(rt_summary)
}

handle.duplicate.listings <- function(t_d = theoretical_dates) {
  # only keep earliest start date. Setkey sorts from earliest to latest
  setkey(t_d, start)
  # Unique keeps the first entry which we sorted to be the earliest date
  t_d_unique <- unique(t_d, by = c("PUPRN", "deviceType", "readType"))
  return(t_d_unique)
}

import.and.rbind <- function(file.names, file.location) {
  # takes a vector of file names, one file location and reads in the tables, row binds them, 
  # and outputs one data table. 
  tmp <- lapply(file.names, function(x) data.table::fread(paste(file.location, x, sep = "")))
  dt_out <- data.table::rbindlist(tmp, fill = TRUE)
  return(dt_out)
}

name.daily.cols <- function(daily_data) {
  setnames(daily_data, 
           old = c("Elec_current_read",
                   "Gas_current_read"),
           new = c("Elec_act_imp_d_Wh",
                   "Gas_d_m3")
  )
  return(daily_data)
}

name.hh.cols <- function(hh_data) {
  setnames(hh_data, 
           old = c("HH_segment", 
                   "elec_active_import_profile_hh_wh",
                   "elec_reactive_import_profile_hh_wh",
                   "elec_active_export_profile_hh_wh",
                   "elec_reactive_export_profile_hh_wh",
                   "gas_active_import_profile_hh_L"),
           new = c("HH",
                   "Elec_act_imp_hh_Wh",
                   "Elec_react_imp_hh_varh",
                   "Elec_act_exp_hh_Wh",
                   "Elec_react_exp_hh_varh",
                   "Gas_hh_m3")
  )
  return(hh_data)
}

process.multiple.read.schedules <- function(datesTable) {
  # deal with schedule duplicates when a meter was replaced
  multi_entries <-
    datesTable[, .N, keyby = list(PUPRN, deviceType, readType)][N > 1]
  setkeyv(datesTable, c("PUPRN", "deviceType", "readType"))
  setkeyv(multi_entries, c("PUPRN", "deviceType", "readType"))
  multi_resolved <-
    datesTable[multi_entries[, c("PUPRN", "deviceType", "readType")]]
  
  ## set the start and end dates for meters with multiple schedules as the earliest
  # and latest schedule dates across all their schedules
  for (r in 1:nrow(multi_entries)) {
    theoreticalStart <-
      multi_resolved[PUPRN == multi_entries[r, PUPRN] &
                       deviceType == multi_entries[r, deviceType] &
                       readType == multi_entries[r, readType],
                     min(theoreticalStart)]
    
    theoreticalEnd <-
      multi_resolved[PUPRN == multi_entries[r, PUPRN] &
                       deviceType == multi_entries[r, deviceType] &
                       readType == multi_entries[r, readType],
                     max(theoreticalEnd)]
    
    multi_resolved[PUPRN == multi_entries[r, PUPRN] &
                     deviceType == multi_entries[r, deviceType] &
                     readType == multi_entries[r, readType],
                   `:=`(theoreticalStart = theoreticalStart,
                        theoreticalEnd = theoreticalEnd)]
  }
  
  multi_resolved <-
    unique(multi_resolved, by = c("PUPRN", "deviceType", "readType"))
  
  single_records <-
    datesTable[datesTable[, .N, keyby = list(PUPRN, deviceType, readType)][N == 1]]
  
  single_records[, N := NULL]
  
  datesTableOutput <- rbind(single_records, multi_resolved)
  return(datesTableOutput)
  
} 

reorder.sm.cols <- function(sm_data, resolution = "hh") {
  if(resolution == "hh") {
    setcolorder(sm_data, c("PUPRN", 
                           "Read_date_effective",
                           "Read_date_time",
                           "HH",
                           "Valid_read_time",
                           "Elec_import_exists",
                           "Elec_act_imp_hh_Wh",
                           "Elec_act_imp_flag",
                           "Elec_react_imp_hh_varh",
                           "Elec_react_imp_flag",
                           "Elec_export_exists",
                           "Elec_act_exp_hh_Wh",
                           "Elec_act_exp_flag",
                           "Elec_react_exp_hh_varh",
                           "Elec_react_exp_flag",
                           "Gas_exists",
                           "Gas_hh_m3",
                           "Gas_hh_Wh",
                           "Gas_hh_kWh",
                           "Gas_flag")
    )
  } else {
    setcolorder(sm_data, c("PUPRN", 
                           "Read_date_effective",
                           "Read_date_time",
                           "Valid_read_time",
                           "Valid_24h_read_flag",
                           "Elec_import_exists",
                           "Elec_act_imp_d_Wh",
                           "Unit_correct_elec_act_imp_d_Wh",
                           "Elec_act_imp_d_kWh",
                           "Elec_act_imp_flag",
                           "N_elec_hh",
                           "Elec_act_imp_hh_sum_Wh",
                           "Elec_act_imp_sum_diff",
                           "Elec_sum_match",
                           "Gas_exists",
                           "Gas_d_m3",
                           "Gas_d_kWh",
                           "Gas_flag",
                           "N_gas_hh",
                           "Gas_hh_sum_m3",
                           "Gas_sum_diff",
                           "Gas_sum_match"
                           )
    )
    
  }
}

reorder.rt_summary.cols <- function(rt_summary) {
  data.table::setcolorder(rt_summary, 
                          neworder = c("PUPRN", 
                                       "deviceType", 
                                       "readType", 
                                       "theoreticalStart", 
                                       "theoreticalEnd",
                                       "firstValidReadDate",
                                       "lastValidReadDate",
                                       "daysRange",
                                       "maxPossReads", 
                                       "percValid", 
                                       "percValidOrUnitError", 
                                       "percMissing", 
                                       "percError", 
                                       "f_1", 
                                       "f_0", 
                                       "f_minus1", 
                                       "f_minus2", 
                                       "f_minus3", 
                                       "f_minus4", 
                                       "f_minus5",
                                       "minValidRead",
                                       "maxValidRead",
                                       "meanValidRead",
                                       "medianValidRead",
                                       "sdValidRead")
  )
}

replace.na.in.data.table <- function(dt, replacement = 0) {
  for(i in names(dt)) {
    dt[is.na(get(i)), (i) := replacement]
  }
  return(dt)
}

sum.errors <- function(dt = hh, 
                       error_colname = "Elec_act_imp_flag", 
                       dType = "ESME", 
                       rType = "AI") {
  dt2 <- dt[, .N, keyby = .(PUPRN, get(error_colname))]
  dt3 <- dcast(dt2, PUPRN ~ get, value.var = "N")
  dt3 <- replace.na.in.data.table(dt3)
  dt3[, `:=`(deviceType = dType,
             readType = rType)]
  return(dt3)
}

validate.daily.read.time <- function(daily) {
  daily[, Valid_read_time := TRUE]
  daily[hour(Read_date_time) != 0 | !minute(Read_date_time) %in% c(0, 30),
        Valid_read_time := FALSE]
  return(daily)
}



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
hh_orig <- import.and.rbind(hh_filename, location_orig)
proc.time() - ptm # 91 seconds elapsed


ptm <- proc.time()
daily_orig <- import.and.rbind(daily_filename, location_orig)
proc.time() - ptm # 4 seconds elapsed

# deal with integer-64 in hh data (replace 64-bit equivalent of 16777215 with 32-bit version)
hh <- copy(hh_orig)
hh[elec_active_import_profile_hh_wh > 16777215, 
   elec_active_import_profile_hh_wh := 16777215]
hh[, elec_active_import_profile_hh_wh := as.integer(elec_active_import_profile_hh_wh)]

daily <- copy(daily_orig)

# Remove originals to save space (skip to allow comparison)
rm(daily_orig)
rm(hh_orig)
gc()


# Main processing ---------------------------------------------------------


hh <- name.hh.cols(hh)
hh <- get.meter.existence(hh, readDates) 

daily <- name.daily.cols(daily)
daily <- get.meter.existence(daily, readDates, resolution = "daily")

ptm <- proc.time()
hh <- format.date.times(hh, resolution = "hh")
daily <- format.date.times(daily, resolution = "daily")
proc.time() - ptm # 53 seconds elapsed

hh[, Valid_read_time := HH %in% seq(1:48)]
daily <- validate.daily.read.time(daily)

ptm <- proc.time()
hh <- code.errors(hh, error_codes_hh)
daily <- code.errors(daily, error_codes_daily)
proc.time() - ptm # 23 seconds elapsed

hh <- convert.gas.hh(hh)
daily <- convert.gas.daily(daily)

daily <- correct.elec.in.kwh(daily, min_n_to_determine_unit_error)
daily <- convert.elec.daily(daily)

ptm <- proc.time()
daily <- calc.hh.sums.flag.mismatch(daily, hh) 
proc.time() - ptm # 20 seconds elapsed

ptm <- proc.time()
daily <- flag.daily.sum.match(daily, elec_match_limit, elec_similar_limit,
                              gas_match_limit, gas_similar_limit)
proc.time() - ptm # .4 seconds elapsed
  
hh <- reorder.sm.cols(hh)
daily <- reorder.sm.cols(daily, "daily")


# save
if(save_data == TRUE) {
  
  daily_saving_name <- get.serl.filename("daily_data", release_version)
  hh_saving_name <- get.serl.filename("hh_data", release_version)
  
  ptm <- proc.time()
  fwrite(hh, 
         file = paste(location_processed, hh_saving_name, ".csv", sep =  ""))
  proc.time() - ptm # 43.75 seconds elapsed
  
  ptm <- proc.time()
  fwrite(daily, 
         file = paste(location_processed, daily_saving_name, ".csv", sep =  ""))
  proc.time() - ptm # 1.28 seconds elapsed
  
  ptm <- proc.time()
  save(hh, 
       file = paste(location_RData, hh_saving_name, ".RData", sep = ""))
  proc.time() - ptm # 474.3 seconds elapsed
  
  ptm <- proc.time()
  save(daily, 
       file = paste(location_RData, daily_saving_name, ".RData", sep = ""))
  proc.time() - ptm # 9.82 seconds elapsed
}


# Read-type summary -------------------------------------------------------


ptm <- proc.time()
rt_summary <- create.read.type.summary(hh, daily, readDates, error_codes_hh, error_codes_daily) 
proc.time() - ptm # 12 seconds elapsed

ptm <- proc.time()
rt_summary <- get.valid.read.dates(hh, daily, readDates, error_codes_hh, error_codes_daily, rt_summary) 
proc.time() - ptm # 11 seconds elapsed

rt_summary <- correct.theoretical.start(rt_summary)

ptm <- proc.time()
rt_summary <- calc.error.percentages(rt_summary)
proc.time() - ptm # 0.02 seconds elapsed

rt_summary <- correct.rt.perc.out.of.date.range(rt_summary)

ptm <- proc.time()
rt_summary <- get.read.type.stats(hh, daily, rt_summary, error_codes_hh, error_codes_daily)
proc.time() - ptm # 72 seconds elapsed

rt_summary <- reorder.rt_summary.cols(rt_summary)

# save
if(save_data == TRUE) {
  
  rt_saving_name <- get.serl.filename("rt_data", release_version)
  
  ptm <- proc.time()
  fwrite(rt_summary, 
         file = paste(location_processed, rt_saving_name, ".csv", sep =  ""))
  proc.time() - ptm # 0.2 seconds elapsed
  
  ptm <- proc.time()
  save(rt_summary, 
       file = paste(location_RData, rt_saving_name, ".RData", sep = ""))
  proc.time() - ptm # 0.2 seconds elapsed
}


# Participant-level summary -----------------------------------------------

ptm <- proc.time()
participant_summary <- create.participant.summary(rt_summary, 
                                                  survey_file, 
                                                  participant_details_file, 
                                                  epc_file)
proc.time() - ptm # 1.2 seconds elapsed

# save
if(save_data == TRUE) {
  
  pp_summary_saving_name <- get.serl.filename("ps_data", release_version)
  
  ptm <- proc.time()
  fwrite(participant_summary, 
         file = paste(location_processed, pp_summary_saving_name, ".csv", sep =  ""))
  proc.time() - ptm # 0.06 seconds elapsed
  
  ptm <- proc.time()
  save(participant_summary, 
       file = paste(location_RData, pp_summary_saving_name, ".RData", sep =  ""))
  proc.time() - ptm # 0.08 seconds elapsed
}


