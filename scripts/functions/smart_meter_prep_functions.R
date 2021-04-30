# This code the functions needed in the preparation of smart meter data for
# the SERL Observatory datasets.

# Code developed by Ellen Webborn, UCL using R version 4.0.1 (2020-06-06)
# Most recent edits made by Ellen Webborn, 2021-04-23

# For the latest versions of all code and documentation please visit the SERL
#  GitHub page https://github.com/smartEnergyResearchLab


attach.EPC.basic.info <- function(participant_summary, epc_file) {
  EPC <- fread(epc_file)
  setnames(EPC, "puprn", "PUPRN")
  setkey(EPC, PUPRN)
  participant_summary <- EPC[, .(PUPRN, currentEnergyRating)][participant_summary]
  participant_summary[, EPC_exists := TRUE]
  participant_summary[is.na(currentEnergyRating), EPC_exists := FALSE]
  setnames(participant_summary, "currentEnergyRating", "EPC_rating")
  return(participant_summary)
}

attach.participant.info <- function(participant_summary, participant_details) {
  setkey(participant_details, PUPRN)
  participant_summary <- participant_details[, .(PUPRN, Region, IMD_quintile, LSOA, 
                                                 grid_cell, pilot_cell)][participant_summary]
  setnames(participant_summary, "pilot_cell", "pilot_test_cell")
  return(participant_summary)
}

attach.survey.data <- function(participant_summary, survey_file) {
  load(survey_file)
  setkey(survey_reordered, PUPRN)
  survey_reordered[, N_required := 30 + 
                 (A302 > 0 | A303 > 0 | A304 > 0 | A305 > 0 | A306 > 0 |
                    A307 > 0 | A308 > 0 | A309 > 0 | A310 > 0 | A3_Other > 0) +
                 (A402 > 0 |A403 > 0) + 
                 (A6 == 1) + 
                 (A7 == 1) + 
                 (B2 == 2) +
                 (C5 == 1) +
                 (C1 > 1) * 3
  ]
  
  survey_reordered[, N_answered := (A1 > 0) + (A2 > -2) + (A3_sum > 0) + (A4_sum > 0) + (A5 > -2) + 
                 (A6 > 0) + (A7 > 0) + (A8 > -2) + (A9_sum > 0) + (A10 > 0) + (A11 > 0) +
                 (A12_Taps_sum > 0) + (A12_Shower_sum > 0) + (A13_01 > 0) + (A13_02 > 0) + 
                 (A14 > -2) + (A1501 > -2) + (A1502 > -2) + (A16_sum > 0) + 
                 (B1 > 0) + (B2 > 0) + (B3 > 0) + (B4 > 0) + (B5> 0) + (B6 > 0) + (B7 > -2) + 
                 (B8 > -2) + (B9 > -2) + (B10_sum > 0) + 
                 (C1 > 0) + (C2_sum > 0) + (C3_sum > 0) + (C4 > -2) + (C5 > -2) + (C6 > -2) + 
                 (D1 > 0) + (D2 %in% c(-1, 1, 2, 3)) + (D3 > 0 | D3 == -3) + (D4 != -2)
  ]
  
  survey_reordered[, Perc_answered := round(N_answered / N_required * 100, 2)]
  
  pp_summary <- survey_reordered[, list(PUPRN, N_answered, Perc_answered)][participant_summary]
  return(pp_summary)
}

calc.daily.sums.from.hh <- function(hh) {
  # Elec active import
  ## Sum the valid reads for each PUPRN for each day
  valid_elec_imp_reads <- hh[Elec_act_imp_flag == 1, 
                             sum(Elec_act_imp_hh_Wh), 
                             keyby = .(PUPRN, Read_date_effective_local)]
  setnames(valid_elec_imp_reads, old = "V1", new = "Elec_act_imp_hh_sum_Wh")
  
  ## Count how many valid reads exist for each day for each PUPRN
  n_valid_elec_imp_reads <- hh[Elec_act_imp_flag == 1, 
                               .N, 
                               keyby = .(PUPRN, Read_date_effective_local)]
  
  hh_elec_sums <- n_valid_elec_imp_reads[valid_elec_imp_reads]
  setnames(hh_elec_sums, old = "N", new = "N_elec_hh")
  
  # Gas import
  ## Sum the valid reads for each PUPRN for each day
  valid_gas_imp_reads <- hh[Gas_flag == 1, 
                            sum(Gas_hh_m3), 
                            keyby = .(PUPRN, Read_date_effective_local)]
  setnames(valid_gas_imp_reads, old = "V1", new = "Gas_hh_sum_m3")
  
  ## Count how many valid reads exist for each day for each PUPRN
  n_valid_gas_imp_reads <- hh[Gas_flag == 1, 
                              .N, 
                              keyby = .(PUPRN, Read_date_effective_local)]
  
  hh_gas_sums <- n_valid_gas_imp_reads[valid_gas_imp_reads]
  setnames(hh_gas_sums, old = "N", new = "N_gas_hh")
  
  return(list(hh_elec_sums, hh_gas_sums))
}

calc.error.percentages <- function(rt_summary) {
  rt_summary[, percValid := round(valid / maxPossReads * 100, 2)]
  rt_summary[, percMissing := round(missing / maxPossReads * 100, 2)]
  rt_summary[, percError := round(100 - percValid - percMissing, 2)]
  rt_summary[, percValidOrUnitError := round((valid + wrongUnits) / maxPossReads * 100, 2)] 
}

calc.flag.daily.sum.match <- function(daily, elec_match_limit, elec_similar_limit, 
                                      gas_match_limit, gas_similar_limit) {
  # Calculate difference between daily reads and half-hourly sums
  # Flag if the daily electricity reading matches or is similar to (defined in setup) 
  #  the sum of the half-hourly reads for that day. 
  
  daily[, Elec_act_imp_sum_diff := round(Unit_correct_elec_act_imp_d_Wh - 
                                           Elec_act_imp_hh_sum_Wh, 3)]
  
  daily[, Gas_sum_diff := round(Gas_d_m3 - Gas_hh_sum_m3, 3)]
  
  ## setup all with code 999 in order to catch any types of matching missed
  daily[, Elec_sum_match := 999]
  
  ## No electricity meter
  daily[Elec_import_exists == FALSE, 
        Elec_sum_match := 2] # No meter
  
  ## not possible to compare as don't have 48 valid hh reads and/or a valid daily read
  daily[Elec_sum_match == 999 & is.na(Elec_act_imp_sum_diff), 
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
  daily[Gas_sum_match == 999 & is.na(Gas_sum_diff), 
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

calc.hh.sums <- function(daily, hh) {
  valid_elec_imp_reads <- hh[Elec_act_imp_flag == 1,
                             sum(Elec_act_imp_hh_Wh),
                             keyby = .(PUPRN, Read_date_effective_local)]
  setnames(valid_elec_imp_reads, old = "V1", new = "Elec_act_imp_hh_sum_Wh")
  
  ## Count how many valid reads exist for each day for each PUPRN
  n_valid_elec_imp_reads <- hh[Elec_act_imp_flag == 1,
                               .N,
                               keyby = .(PUPRN, Read_date_effective_local)]
  
  hh_elec_sums <- n_valid_elec_imp_reads[valid_elec_imp_reads]
  setnames(hh_elec_sums, old = "N", new = "N_elec_hh")
  
  # Gas import
  ## Sum the valid reads for each PUPRN for each day
  valid_gas_imp_reads <- hh[Gas_flag == 1,
                            sum(Gas_hh_m3),
                            keyby = .(PUPRN, Read_date_effective_local)]
  setnames(valid_gas_imp_reads, old = "V1", new = "Gas_hh_sum_m3")
  
  ## Count how many valid reads exist for each day for each PUPRN
  n_valid_gas_imp_reads <- hh[Gas_flag == 1,
                              .N,
                              keyby = .(PUPRN, Read_date_effective_local)]
  
  hh_gas_sums <- n_valid_gas_imp_reads[valid_gas_imp_reads]
  setnames(hh_gas_sums, old = "N", new = "N_gas_hh")
  
  # Create same table for gas and merge them
    hh_sums <- merge.data.table(
    hh_elec_sums,
    hh_gas_sums,
    by = c("PUPRN",
           "Read_date_effective_local"),
    all = TRUE
  )
  hh_sums[is.na(N_elec_hh), N_elec_hh := 0]
  hh_sums[is.na(N_gas_hh), N_gas_hh := 0]
  
  bst_dates <-
    fread(paste(location_orig, "bst_dates_to_2024.csv", sep = ""))
  setkey(bst_dates, Read_date_effective_local)
  
  setkey(hh_sums, "Read_date_effective_local")
  hh_sums <- bst_dates[, .(Read_date_effective_local, n_hh)][hh_sums]
  hh_sums[is.na(n_hh), n_hh := 48]
  
  # Replace hh_sums with NA if there weren't the right amount
  hh_sums[N_elec_hh != n_hh, Elec_act_imp_hh_sum_Wh := NA]
  hh_sums[N_gas_hh != n_hh, Gas_hh_sum_m3 := NA]
  
  # Remove rows where no daily total is possible
  hh_sums <- hh_sums[!is.na(Elec_act_imp_hh_sum_Wh) |
                       !is.na(Gas_hh_sum_m3)]
  
  daily <- merge.data.table(
    daily,
    hh_sums[, .(PUPRN,
                Read_date_effective_local,
                Elec_act_imp_hh_sum_Wh,
                Gas_hh_sum_m3)],
    by = c("PUPRN", "Read_date_effective_local"),
    all = TRUE
  )
  
  # Give the rows without daily sums a Read_date_time_local (midnight day before)
  daily[is.na(Read_date_time_local),
        Read_date_time_local := lubridate::force_tz(Read_date_effective_local - 1,
                                                    "Europe/London")]
  
  return(daily)
  
}


calc.stats <- function(i, data_t, error_codes) {
  return(data_t[get(error_codes[i, err_colname]) == 1 & Valid_read_time == TRUE, 
                .(minValidRead = as.double(min(get(error_codes[i, read]), na.rm = TRUE)),
                  maxValidRead = as.double(max(get(error_codes[i, read]), na.rm = TRUE)),
                  meanValidRead = round(as.double(mean(get(error_codes[i, read]), na.rm = TRUE), 2)),
                  medianValidRead = round(as.double(median(get(error_codes[i, read]), na.rm = TRUE)), 2),
                  sdValidRead = round(as.double(sd(get(error_codes[i, read]), na.rm = TRUE), 2)),
                  deviceType = error_codes[i, deviceType],
                  readType = error_codes[i, readType]
                ),
                keyby = PUPRN])
}

code.errors <- function(sm.data, error_codes) {
  
  for(i in 1:nrow(error_codes)) {
    # ESME or GPF doesn't exist/isn't registered 
    if(stringr::str_detect(error_codes[i, read], "imp") == TRUE) {
      sm.data[Elec_import_exists == FALSE, eval(error_codes[i, err_colname]) := 2]
    } else if(stringr::str_detect(error_codes[i, read], "Gas") == TRUE) {
      sm.data[Gas_exists == FALSE, eval(error_codes[i, err_colname]) := 2]
    } else {
      sm.data[Elec_export_exists == FALSE, eval(error_codes[i, err_colname]) := 2]
    }
    
    # if meter exists but no read, code as missing
    sm.data[is.na(get(error_codes[i, read])) & 
              is.na(get(error_codes[i, err_colname])), 
            eval(error_codes[i, err_colname]) := 0]
    
    # if coded as missing but invalid read time, code as not needed (3)
    sm.data[Valid_read_time == FALSE & 
              get(error_codes[i, err_colname]) == 0,
            eval(error_codes[i, err_colname]) := 3]
    
    # Now we have meter existence, and actual values, code these values
    
    # Max error
    sm.data[get(error_codes[i, read]) >= error_codes[i, max_err],
            eval(error_codes[i, err_colname]) := -1]
    
    #High error
    sm.data[get(error_codes[i, read]) >= error_codes[i, high_err] & 
              get(error_codes[i, read]) < error_codes[i, max_err],
            eval(error_codes[i, err_colname]) := -2]
    
    # Negative
    sm.data[get(error_codes[i, read]) < 0,
            eval(error_codes[i, err_colname]) := -3]
    
    # Valid (none of the above)
    sm.data[is.na(get(error_codes[i, err_colname])),
            eval(error_codes[i, err_colname]) := 1]
    
  }
  
  return(sm.data)
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
  # Correct where all daily reads < 100 and at least 
  #  min_n_to_determine_unit_error reads exist
  max_elec_reads <- daily[Elec_act_imp_flag == 1, max(Elec_act_imp_d_Wh), keyby = PUPRN]
  id_kWh <- max_elec_reads[V1 < 100, PUPRN]
  n_elec_reads_suspected_kWh <- daily[Elec_act_imp_flag == 1 & 
                                        PUPRN %in% id_kWh, .N, keyby = PUPRN]
  id_kWh_sufficient_N <- n_elec_reads_suspected_kWh[N >= min_n_to_determine_unit_error]
  
  # change error code from valid (1) to unit error (-4)
  daily[PUPRN %in% id_kWh_sufficient_N$PUPRN & 
          Elec_act_imp_flag == 1, 
        Elec_act_imp_flag := -4]
  # create new column for unit-corrected (or original) data
  daily[, Unit_correct_elec_act_imp_d_Wh := Elec_act_imp_d_Wh]
  daily[Elec_act_imp_flag == -4, Unit_correct_elec_act_imp_d_Wh := Elec_act_imp_d_Wh * 1000]
  
  # Correct if daily is approx hh_sum / 1000 (only issue for elec)
  ## create new column to help with comparison. 'Approx' means anything with form 
  #    x000 to x999 (hh sum) and x-1 to x+1 (daily sum)
  daily[, comparator := floor(Elec_act_imp_hh_sum_Wh / 1000)]

  # flag anything we haven't caught previously, isn't just a 0 read, and is within our 'approx'
  #  range. Ignoring zeros as lots of one-off cases found that aren't relevant. 
  daily[, might_correct := Unit_correct_elec_act_imp_d_Wh >= comparator - 1 & 
          Unit_correct_elec_act_imp_d_Wh <= comparator + 1 & 
          Unit_correct_elec_act_imp_d_Wh != 0 &
          Elec_act_imp_flag != -4]
  
  ## count how many issues per PUPRN because we'll insist on at least 5 in order to correct
  #  (based on investigations, will miss some and miss-flag others, but seems to minimise issues)
  counts <- daily[might_correct == TRUE, .N, by = PUPRN]

  daily[, to_correct := might_correct == TRUE & 
          PUPRN %in% counts[N >= 5, PUPRN]]
  
  # make correction. Note we could have replaced with hh_sums but this stays consistent with 
  #  first set of unit correction, and likely other daily reads will want to be replaced with
  #  hh_sums by researchers
  daily[to_correct == TRUE, `:=`(Elec_act_imp_flag = -4,
                                 Unit_correct_elec_act_imp_d_Wh = Elec_act_imp_d_Wh * 1000)]
  
  # delete columns no longer required
  daily[, `:=`(comparator = NULL,
               might_correct = NULL,
               to_correct = NULL)]
  
  return(daily)
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

create.participant.summary <- function(rt_summary, survey_file, participant_details, epc_file) {
  tmp_poss_reads <- copy(rt_summary[, .(PUPRN,
                                        deviceType,
                                        readType,
                                        firstValidReadDate,
                                        lastValidReadDate,
                                        valid,
                                        percValid)])
  setnames(tmp_poss_reads, 
           old = c("firstValidReadDate", "lastValidReadDate", "valid", "percValid"), 
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
  participant_summary <- attach.participant.info(participant_summary, participant_details)
  participant_summary <- attach.EPC.basic.info(participant_summary, epc_file)
  
  setnames(participant_summary, 
           old = c("N_answered", 
                   "Perc_answered"),
           new = c("N_survey_ans", 
                   "Perc_survey_ans")
  )
  
  setcolorder(participant_summary, c("PUPRN", 
                                     "Region",
                                     "LSOA",
                                     "grid_cell",
                                     "IMD_quintile",
                                     "EPC_exists",
                                     "EPC_rating",
                                     "pilot_test_cell",
                                     "N_survey_ans", 
                                     "Perc_survey_ans",
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
  err_count_colnames <- c("validWrongTime", "wrongUnits", 
                          "negative", "highRead", "maxRead", 
                          "missing", "valid", "notNeeded")
  setnames(rt_summary, 
           old = c("-5", "-4", "-3", "-2", "-1", "0", "1", "3"), 
           new = err_count_colnames, 
           skip_absent = TRUE)
  rt_cols <- colnames(rt_summary)
  for(i in 1:length(err_count_colnames)) {
    if(!(err_count_colnames[i] %in% rt_cols)) {
      rt_summary[, eval(err_count_colnames[i]) := 0]
    }
  }
  rt_summary[, missing := maxPossReads - valid - maxRead - highRead - negative -
                          wrongUnits - validWrongTime]
  rt_summary[, `:=`(`2` = NULL,
                    notNeeded = NULL)]
  return(rt_summary)
}

determine.theoretical.read.dates <- function(theoretical_dates_file, 
                                             inventory,
                                             participant_details,
                                             collection_end_date) {
  theoretical_dates <- fread(theoretical_dates_file)
  colnames(theoretical_dates) <- c(c("PUPRN", "deviceType", "readType", "start"))
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
  valid_start_dates <- dt[get(error_flag) == 1 & Valid_read_time == TRUE, 
                          min(Read_date_effective_local, na.rm = TRUE), 
                          keyby = PUPRN]
  setnames(valid_start_dates, "V1", "firstValidReadDate")
  
  valid_end_dates <- hh[get(error_flag) == 1 & Valid_read_time == TRUE, 
                        max(Read_date_effective_local, na.rm = TRUE), 
                        keyby = PUPRN]
  setnames(valid_end_dates, "V1", "lastValidReadDate")
  
  valid_start_dates[, `:=`(deviceType = dType,
                           readType = rType)]
  
  valid_end_dates[, `:=`(deviceType = dType,
                         readType = rType)]
  
  return(list(valid_start_dates, valid_end_dates))
}


format.daily.date.times <- function(daily) {
  # Configure read_date_time, make clear it's in local time not UTC
  setnames(daily, "Read_date_time", "Read_date_time_local")
  daily[, Read_date_time_local := lubridate::force_tz(Read_date_time_local,
                                                      "Europe/London")]
  
  # Set the effective read date (day before if at midnight (ideal), or < midday)
  daily[, Read_date_effective_local := date(Read_date_time_local)]
  daily[hour(Read_date_time_local) < 12, 
        Read_date_effective_local := Read_date_effective_local - 1]
  
  # remove unnecessary columns
  daily[, `:=`(Read_date_time_effective = NULL,
               Read_date_effective = NULL,
               Valid_24h_read_flag = NULL)]
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

format.hh.date.times <- function(hh) {
  hh[, `:=`(Read_date_time_effective = NULL,
            Read_date_effective = NULL)]
  setnames(hh, "Read_date_time", "Read_date_time_UTC")
  
  tmp <- data.table(Read_date_time_UTC = unique(hh$Read_date_time_UTC))
  tmp[, Read_date_time_local := format(Read_date_time_UTC, 
                                       tz = "Europe/London",
                                       usetz = TRUE)]
  tmp[, Read_date_effective_local := date(Read_date_time_local)]
  tmp[hour(Read_date_time_local) == 0 & 
        minute(Read_date_time_local) < 15, 
      Read_date_effective_local := Read_date_effective_local - 1]
  setkey(tmp, "Read_date_time_UTC")
  setkey(hh, "Read_date_time_UTC")
  
  hh <- tmp[hh]
  
  return(hh)
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
  for (i in 1:n_multi) {
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

get.meter.existence <- function(sm_data, sm_starts, inv = inventory, resolution = "hh") {
  # Note: assuming that if half-hourly exists then daily exists
  sm_data[, Elec_import_exists := PUPRN %in% sm_starts[deviceType == "ESME" &
                                                         readType %in% c("AI", "RI"), 
                                                       PUPRN]] 
  if (resolution == "hh") {
    sm_data[, Elec_export_exists := PUPRN %in% sm_starts[deviceType == "ESME" &
                                                           readType == "AE", PUPRN]] # Active export
  }
  sm_data[, Gas_exists := PUPRN %in% sm_starts[deviceType == "GPF", PUPRN] & 
            PUPRN %in% inv[deviceType == "GSME" & deviceStatus == "Commissioned", PUPRN]] 
  # Note: amended Jan 2021 since GPF-only isn't viable but exists
  return(sm_data)
}

get.read.type.stats <- function() {
  tmp1 <- lapply(1:nrow(error_codes_hh), 
                 calc.stats,
                 data_t = hh,
                 error_codes = error_codes_hh)
  
  tmp2 <- lapply(1:nrow(error_codes_daily), 
                 calc.stats,
                 data_t = daily,     
                 error_codes = error_codes_daily)
  
  read_stats <- rbindlist(c(tmp1, tmp2), fill=TRUE)
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
  return(unique(t_d, by = c("PUPRN", "deviceType", "readType")))
}

import.and.rbind <- function(file.names, file.location) {
  # takes a vector of file names, one file location and reads in the tables,
  #  row binds them, and outputs one data table.
  tmp <-
    lapply(file.names, function(x)
      data.table::fread(paste(file.location, x, sep = "")))
  return(data.table::rbindlist(tmp, fill = TRUE))
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
  
  return(rbind(single_records, multi_resolved))
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
                                       "valid", 
                                       "validWrongTime",
                                       "wrongUnits", 
                                       "missing", 
                                       "maxRead", 
                                       "highRead", 
                                       "negative", 
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

select.daily.cols <- function(daily) {
  # remove unwanted columns, order columns, sort rows
  daily[, `:=`(Elec_import_exists = NULL,
               Gas_exists = NULL,
               Elec_act_imp_sum_diff = NULL,
               Gas_sum_diff = NULL)]
  
  setcolorder(
    daily,
    c("PUPRN",
      "Read_date_effective_local",
      "Read_date_time_local",
      "Valid_read_time",
      "Elec_act_imp_flag",
      "Elec_sum_match",
      "Gas_flag",
      "Gas_sum_match",
      "Elec_act_imp_d_Wh",
      "Unit_correct_elec_act_imp_d_Wh",
      "Elec_act_imp_hh_sum_Wh",
      "Gas_d_m3",
      "Gas_hh_sum_m3",
      "Gas_d_kWh")
  )
  
  setkeyv(daily, c("PUPRN", "Read_date_effective_local"))
  
  return(daily)
}

select.hh.cols <- function(hh) {
  # remove unwanted columns, order columns, sort rows
  hh[, `:=`(HH = NULL,
            Elec_import_exists = NULL,
            Elec_export_exists = NULL,
            Gas_exists = NULL,
            Gas_hh_kWh = NULL)]
  
  setcolorder(
    hh,
    c("PUPRN", 
      "Read_date_effective_local",
      "Read_date_time_local",
      "Read_date_time_UTC",
      "Valid_read_time",
      "Elec_act_imp_flag",
      "Elec_react_imp_flag",
      "Elec_act_exp_flag",
      "Elec_react_exp_flag",
      "Gas_flag",
      "Elec_act_imp_hh_Wh",
      "Elec_react_imp_hh_varh",
      "Elec_act_exp_hh_Wh",
      "Elec_react_exp_hh_varh",
      "Gas_hh_m3",
      "Gas_hh_Wh"))
  
  setkeyv(hh, c("PUPRN", "Read_date_time_UTC"))
  
  return(hh)
}

sum.errors <- function(dt = hh, 
                       error_colname = "Elec_act_imp_flag", 
                       dType = "ESME", 
                       rType = "AI") {
  dt2 <- dt[, .N, keyby = .(PUPRN, get(error_colname))]
  dt3 <- dcast(dt2, PUPRN ~ get, value.var = "N")
  dt3[, `1` := NULL]
  
  # Deal with our split of valid between valid read time and invalid read time
  dt4 <- dt[get(error_colname) == 1, .N, keyby = .(PUPRN, Valid_read_time)]
  dt4 <- dcast(dt4, PUPRN ~ Valid_read_time, value.var = "N")
  setnames(dt4, c("FALSE", "TRUE"), c("-5", "1"), skip_absent=TRUE)
  
  dt3 <- merge(dt3, dt4, all = TRUE)
  
  dt3[, `:=`(deviceType = dType,
             readType = rType)]
  return(dt3)
}

validate.daily.read.time <- function(daily) {
  daily[, Valid_read_time := TRUE]
  daily[hour(Read_date_time_local) != 0 | 
          minute(Read_date_time_local) != 0 |
          second(Read_date_time_local) != 0,
        Valid_read_time := FALSE]
  return(daily)
}

