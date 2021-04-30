# This code prepares the survey data for the SERL Observatory datasets. 

# Code developed by Ellen Webborn, UCL using R version 4.0.1 (2021-01-04)
# Most recent edits made Ellen Webborn, 2021-04-23

# For the latest versions of all code and documentation please visit the SERL
#  GitHub page https://github.com/smartEnergyResearchLab


# Setup -------------------------------------------------------------------

# load relevant libraries, set options
library(data.table)
library(knitr)
library(weathermetrics)

options(datatable.fread.input.cmd.message=FALSE)

# Import all filenames, locations, source function files
source("D:/Users/ucldata/observatory_release_pre_processing/observatoryData/scripts/edition02/setup_edition02.R")


# Define Input Variables --------------------------------------------------

## Define which files are needed/what actions are required
importCsv <- TRUE       # set to false if using RData


# Function Definitions -----------------------------------------------------

recode.dontknow <- function(q = "A2", code_to_change = 5,
                            dt = new_survey_data) {
  dt[get(q) == code_to_change, (q) := -1]
  return(dt)
}

recode_minus_2_zero <- function(q = "A3", n = 10, 
                                names_provided = FALSE,
                                dt = new_survey_data) {
  if(names_provided[1] == FALSE){
    col_names <- get.contributing.cols(q, n)
  } else {
    col_names <- names_provided
  }
  
  for(i in col_names) {
    dt[get(i) == -2, (i) := 0]
  }
  return(dt)
}

get.contributing.cols <- function(q = "A1", n = 2) {
  sapply(1:n, function(x) {
    paste(q, "_", x, sep = "")
  })
}

check.multi.responses <- function(q = "A1", q_cols = col_names,
                                  dt = postal_clean, r = n_post) {
  dt[, eval(paste(q, "_n", sep = "")) := sapply(1:r, function(x) {
    dt[x, sum(.SD, na.rm = TRUE), .SDcols = grep(q, q_cols, value = TRUE)]
  })]
  return(dt)
}

determine.response <- function(q = "A1", n = 2, q_cols = col_names,
                               dt = postal_clean) {
  for(j in 1:n) {
    dt[get(q_cols[j]) == 1 & get(paste(q, "_n", sep = "")) == 1, eval(q) := j]
  }
  return(dt)
}

transform.multicol.responses <- function(question, n_responses,
                                         dt = postal_clean,
                                         r = n_post) {
  col_names <- get.contributing.cols(question, n_responses)
  check.multi.responses(q = question,
                        q_cols = col_names,
                        dt = dt,
                        r = r)
  determine.response(q = question,
                     n = n_responses,
                     q_cols = col_names,
                     dt = dt)
  return(dt)
}


# Sum elements that are positive
sum.positive <- function(x){
  # x is a vector (or similar) of elements to be added (if positive)
  sum(sapply(x, function(y) max(y, 0)))
}


# Question D1 is optional if single-occupant household. 
#  This collects the data entered previously in these cases
fill.D1 <- function(r) {
  # r is a row in new_survey_data e.g. new_survey_data[2,]
  
  if(r[D1 > 0, .N] == 1) {
    ans <- r$D1
  } else if(r[C2_error == TRUE | C2_sum <= 0 | C1 != 1, .N] == 1) {
    ans <- -2
  } else if(r[C2_tot_child == 1, .N] == 1) {
    ans <- 1
  } else if(r[C2_Male_16_24 == 1 | C2_Female_16_24 == 1, .N] == 1) {
    ans <- 2
  } else if(r[C2_Male_25_44 == 1 | C2_Female_25_44 == 1, .N] == 1) {
    ans <- 3
  } else if(r[C2_Male_45_64 == 1 | C2_Female_45_64 == 1, .N] == 1) {
    ans <- 4
  } else if(r[C2_Male_65_74 == 1 | C2_Female_65_74 == 1, .N] == 1) {
    ans <- 5
  } else if(r[C2_Male_75_84 == 1 | C2_Female_75_84 == 1, .N] == 1) {
    ans <- 6
  } else if(r[C2_Male_85_plus == 1 | C2_Female_85_plus == 1, .N] == 1) {
    ans <- 7
  } else {ans <- r$D1}
  ans
}

fill.D2 <- function(r) {
  # r is a row in new_survey_data e.g. new_survey_data[2,] 
  if(r$D2 != -9) {
    ans <- r$D2
  } else if(r[C2_error == TRUE | C2_sum != 1, .N] == 1) {
    ans <- -2
  } else if(r[C2_Male_16_24 == 1 | C2_Male_25_44 == 1 | C2_Male_45_64 == 1 |
              C2_Male_65_74 == 1 | C2_Male_75_84 == 1 | C2_Male_85_plus == 1, .N] == 1) {
    ans <- 1
  } else if(r[C2_Female_16_24 == 1 | C2_Female_25_44 == 1 | C2_Female_45_64 == 1 |
              C2_Female_65_74 == 1 | C2_Female_75_84 == 1 | C2_Female_85_plus == 1, .N] == 1) {
    ans <- 2
  } else {ans = 99}
  ans
}

fill.D3 <- function(r) {
  # r is a row in new_survey_data e.g. new_survey_data[2,] 
  if(r$D3 != -9) {
    ans <- r$D3
  } else if(r[C3_error == TRUE | C3_sum != 1, .N] == 1) {
    ans <- -2
  } else if(r[C301 == 1, .N] == 1) {
    ans <- 1
  } else if(r[C302 == 1, .N] == 1) {
    ans <- 2
  } else if(r[C303 == 1, .N] == 1) {
    ans <- 3
  } else if(r[C304 == 1, .N] == 1) {
    ans <- 4
  } else if(r[C305 == 1, .N] == 1) {
    ans <- 5
  } else if(r[C306 == 1, .N] == 1) {
    ans <- 6
  } else if(r[C307 == 1, .N] == 1) {
    ans <- 7
  } else {ans = 99}
  ans
}

# LoadData ----------------------------------------------------------------

if(importCsv == TRUE) {
  survey_prev_released <- fread(survey_previous_file)
  survey_online_orig <- fread(survey_online_file)
  survey_postal_orig <- fread(survey_postal_file)
} else {
  load(paste(survey_location, survey_output_name, ".RData", sep = ""))
}

online_clean <- copy(survey_online_orig)
postal_clean <- copy(survey_postal_orig)
prev_updated <- copy(survey_prev_released)

n_post <- nrow(postal_clean)



# Deal with online and postal differences ---------------------------------


# A

transform.multicol.responses("A1", 2)
transform.multicol.responses("A2", 5)

## Temperature choices is more complicated: some people answered in deg F and deg C
##  We don't want to lose that data, so create new response code 3 for answered both
postal_clean[A5_1 == 1 & A5_2 == 1, A5_3 := 1]
transform.multicol.responses("A5", 4) # this won't code for multiple options selected
postal_clean[A5_3 == 1 & is.na(A5_4), A5 := 3]

transform.multicol.responses("A6", 2)
transform.multicol.responses("A7", 2)
transform.multicol.responses("A8", 6)
transform.multicol.responses("A10", 6)
transform.multicol.responses("A11", 2)
transform.multicol.responses("A13_1", 6)
transform.multicol.responses("A13_2", 6)
transform.multicol.responses("A14", 5)
transform.multicol.responses("A15_1", 6)
transform.multicol.responses("A15_2", 6)


# B

transform.multicol.responses("B1", 6)
transform.multicol.responses("B2", 2)
transform.multicol.responses("B4", 5)
transform.multicol.responses("B7", 3)
transform.multicol.responses("B8", 3)
transform.multicol.responses("B9", 8)


# C

transform.multicol.responses("C4", 2)
transform.multicol.responses("C5", 3)
transform.multicol.responses("C6", 6)


# D

transform.multicol.responses("D1", 7)
transform.multicol.responses("D2", 4)
transform.multicol.responses("D3", 8)
transform.multicol.responses("D4", 7)


# Combine online ('o') and postal ('p') data ------------------------------

p_cols <- sort(colnames(postal_clean))
o_cols <- sort(colnames(online_clean))

o_cols[!o_cols %in% p_cols]  # character(o) all necessary columns have been created
 
new_survey_data <- rbind(online_clean, postal_clean[, ..o_cols])


# Remove duplicates -------------------------------------------------------

## Check for duplicate PUPRN (people who answered online and by post)
nrow(new_survey_data) - length(unique(new_survey_data$PUPRN)) # 100 duplicates

## mark duplicate
setkey(new_survey_data, PUPRN)
new_survey_data[, dup := .N > 1, by = key(new_survey_data)]

## Count NAs to determine which to keep
new_survey_data[, n_NA := sum(is.na(.SD)), .SDcols = colnames(new_survey_data), 
                by = ResponseId]

# Order by fewest NA responses then by collection method 
##  (puts online first which we prefer as better data quality)
setkeyv(new_survey_data, c("PUPRN", "n_NA", "CollectionMethod"))
new_survey_data <- unique(new_survey_data, by = "PUPRN")


# Recode responses to match pilot response coding -------------------------

# Replace NAs with -2 as this was the original code used to indicate no response
for (i in names(new_survey_data)) {
  new_survey_data[is.na(get(i)), (i) := -2]
}

# Deal with non-responses that should be coded as 0 (where an option isn't selected when multiple can be)
# Replace no answer code -2 with 0 code for not selected

# column names for A12 don't follow the pattern so need separate treatment
a12_names <-
  colnames(new_survey_data)[sapply(colnames(new_survey_data), function(x) {
    stringr::str_detect(x, "A12_")
  })]

recode_minus_2_zero("A3", 10)
recode_minus_2_zero("A4", 6)
recode_minus_2_zero("A9", 7)
recode_minus_2_zero("A16", 10)
recode_minus_2_zero("B10", 14)
recode_minus_2_zero(names_provided = a12_names)


# check remaining response codes match


## Deal with 'don't know' option coded differently (code to -1)
recode.dontknow("A2", 5)
recode.dontknow("A5", 4)
recode.dontknow("A8", 6)
recode.dontknow("A14", 5)
recode.dontknow("A15_1", 6)
recode.dontknow("A15_2", 6)
recode.dontknow("B7", 3)
recode.dontknow("B8", 3)
recode.dontknow("B9", 8)
recode.dontknow("C5", 3)
recode.dontknow("C6", 6)
recode.dontknow("D4", 6)


## Other recodes ('prefer not to say')
new_survey_data[D2 == 4, D2 := -3]
new_survey_data[D3 == 8, D3 := -3]
new_survey_data[D4 == 7, D4 := -3]


## Introduce not applicable coding -9 where skipping did (or should have) taken place

new_survey_data[A1 == 2, A2 := -9]

for(i in 1:6) {
  new_survey_data[A3_1 ==1, 
                  paste("A4_",i, sep = "") := -9]
}

new_survey_data[A4_2 !=1 & A4_3 != 1, 
                A5 := -9]
new_survey_data[A6 == 1, 
                A7 := -9]
new_survey_data[A6 == 1 | A7 == 2, 
                A8 := -9]
new_survey_data[B2 == 1, 
                B3_1 := "-9"]
new_survey_data[C5 != 1 & C6 != -9,
                C6 := -9] 


# Rename columns to match pilot survey column names -----------------------

setnames(new_survey_data,
         old = c("REGION", "QUINTILE", "CollectionMethod", 
                 "SURVEY", "A13_1", "A13_2",
                 "A5_1_TEXT", "A5_2_TEXT",
                 "A3_10_TEXT", "A16_10_TEXT", "B3_1", "B5_1", 
                 "B6_1", "R1", "RecordedDate"),
         new = c("Region", "IMD_quintile", "Collection_method", 
                 "Survey_version", "A13_01", "A13_02",
                 "A5_C_text", "A5_F_text",
                 "A3_Other", "A16_Other", "B3", "B5", "B6", 
                 "Final_comments", "Recorded_date"),
         skip_absent = TRUE)


# for replacing '_' with '0' (or delete if 2 digits)
q <- c("A3", "A4", "A9", "A15", "A16", "B10", "C3")
n <- c(9, 6, 7, 2, 9, 9, 7)
extras <- c("A3_10", "A16_10", "B10_10", "B10_11", "B10_12", "B10_13", "B10_14")

# Replace _ with 0
for(i in 1:length(q)) {
  col_names <- get.contributing.cols(q[i], n[i])
  for(j in col_names) {
    setnames(new_survey_data,
             old = j,
             new = stringr::str_replace(j, "_", "0"))
  }
}

# Remove _
for(i in extras) {
  setnames(new_survey_data,
           old = i,
           new = stringr::str_remove(i, "_"))
}



# A12
a12_tmp1 <- c(rep("Taps", 6), rep("Shower", 6))
a12_tmp2 <- rep(c("DK", "EH", "GB", "NA", "Other", "SWH"), 2)
j <- c(rep(1, 6), rep(2, 6))
k <- rep(c(1:6), 2)

for(i in 1:12) {
  setnames(new_survey_data,
           old = stringr::str_c("A12_", j[i], "_", k[i], "_1"),
           new = stringr::str_c("A12_", a12_tmp1[i], "_", a12_tmp2[i]))
}


# C2 
c2_tmp1 <- c(rep("_Male_", 7), rep("_Female_", 7))
c2_tmp2 <- rep(c("0_15", "16_24", "25_44", "45_64", "65_74", "75_84", "85_plus"), 2)
j <- rep(c(1:7), 2)
k <- c(rep(1, 7), rep(2, 7))

for(i in 1:14) {
  setnames(new_survey_data,
           old = stringr::str_c("C2_", j[i], "_", k[i]),
           new = stringr::str_c("C2", c2_tmp1[i], c2_tmp2[i]))
}

  
  

# A1 ----------------------------------------------------------------------
# --- Before we contacted you about this study, did you know you had a smart meter?

new_survey_data[, .N, keyby = A1]

# A2 ----------------------------------------------------------------------
# --- To what extent (if at all) has having a SM affected your energy use (approx wording)

new_survey_data[, .N, keyby = A2]


# A3 ----------------------------------------------------------------------
# --- Type of central heating

# Test whether the question was answered
new_survey_data[, A3_sum := A301 + A302 + A303 + A304 + 
                  A305 + A306 + A307 + A308 + A309 + A310]
new_survey_data[, .N, keyby = A3_sum] 

# Check A3 for no central heating vs some
A3_cols <- c("A301","A302","A303","A304","A305","A306","A307","A308","A309","A310")
new_survey_data[A301 == 1 & sum(get(A3_cols)) > 1, .N] # 51 
new_survey_data[, `:=`(A3_err = FALSE, A301_edit = FALSE)]
for(i in 2:length(A3_cols)) {
  new_survey_data[A301 == 1 & get(A3_cols[i]) == 1, A3_err := TRUE]
}

# change the 'no central heating' to no for those with central heating, 
##  record an error was found and A301 was edited
new_survey_data[A3_err == TRUE, `:=`(A301 = 0, A301_edit = TRUE)]


# A4 ----------------------------------------------------------------------
# --- Which, if any, of the following controls does your central heating system have?

# get a sum to record no response (when one was possible)
new_survey_data[, A4_sum := A401 + A402 + A403 + A404 + A405 + A406]
new_survey_data[, .N, by = A4_sum] # 85 with no answer

# Check no 'none of these' plus something else
new_survey_data[A405 == 1 & A4_sum > 1, .N] # 1 
new_survey_data[, `:=`(A4_err = FALSE, A405_edit = FALSE)]
# get rid of 'none of these' since one has been selected
new_survey_data[A405 == 1 & A4_sum > 1,`:=`(A405 = 0, A4_err = TRUE, A405_edit = TRUE)] 


# A5 ----------------------------------------------------------------------
# --- Temperature controller set at for late afternoons or evenings
# Note that this was asked differently in version 2: a deg C and deg F box were provided since people 
#  appeared to give deg F sometimes to the pilot survey which only requested deg C. 

# Test for people reporting a temperature setting having previously said they couldn't
new_survey_data[A402 != 1 & A403 != 1 & A5 > -1, .N] # 0, have dealt with this above sufficiently

# Column setup (changing pilot survey data)@
# -- A5: indicates how the question was answered: 
#         1: deg C, 2: deg F, 3 both deg C & deg F, -1 don't know, -2 No answer, -9 not applicable
# -- A5_degC : temp given in deg C, or converted into deg C if in deg F
# -- A5_edit: TRUE if edited (pilot study data only, where deg F box didn't exist)

prev_updated[A5 > -1, ':='(A5_degC = as.double(A5), A5 = 1)]
prev_updated[, A5 := as.integer(A5)]
prev_updated[, .N, keyby = A5]

new_survey_data[A5 == 2, A5_degC := fahrenheit.to.celsius(A5_F_text)]
new_survey_data[A5 == 1 | A5 == 3, A5_degC := as.double(A5_C_text)]

new_survey_data[, A5_edit := FALSE]

new_survey_data[is.na(A5_degC) & A5 == 1] # some can't be converted as not just one number

# Deal with individual cases
new_survey_data[is.na(A5_degC) & A5 == 1 & A5_C_text == "",
                `:=`(A5_C_text = NA,
                     A5 = -2)]
new_survey_data[A5_C_text == "17 or 18", `:=`(A5_degC = 17.5,
                                              A5_edit = TRUE)] # take the mean
new_survey_data[A5_C_text == "18-21", `:=`(A5_degC = 19.5,
                                           A5_edit = TRUE)] # take the mean, not ideal


# Some clearly wrote deg C in deg F box and vice versa. Amend and record as edited in the new column.  

upper.cel <- 35 # Maximum number we accept as a temperature in degrees C
new_survey_data[A5_degC > upper.cel, `:=`(A5_degC = fahrenheit.to.celsius(A5_degC),
                                  A5_edit = TRUE)]
new_survey_data[A5_degC <= fahrenheit.to.celsius(upper.cel), 
                `:=`(A5_degC = celsius.to.fahrenheit(A5_degC),
                     A5_edit = TRUE)] # to undo incorrect conversion to deg C

new_survey_data[A5_edit == TRUE, .N] # 45
new_survey_data[, .N, keyby = A5_degC]


# A6 ----------------------------------------------------------------------
# --- Does your household use any standalone heaters

new_survey_data[, .N, keyby = A6]

# A7 ----------------------------------------------------------------------
# --- Do any of your standalone heaters have their own source of power

new_survey_data[, .N, keyby = A7]


# A8 ----------------------------------------------------------------------
# --- How often are these heaters used in cold weather?

new_survey_data[, .N, keyby = A8]


# A9 ----------------------------------------------------------------------
# --- Reasons for adjusting heating

# Multiple variables (A901 - A907)

# Add variable to assess non-response
new_survey_data[, A9_sum := A901 + A902 + A903 + A904 + A905 + A906 + A907]
new_survey_data[, .N, by = A9_sum] # 67 with no response

# Check none of these and something else hasn't been ticked
new_survey_data[A907 == 1 & A9_sum > 1, .N] # 2
new_survey_data[A907 == 1 & A9_sum > 1, A907 := 0] # Clearly not 'none of these'


# A10 ----------------------------------------------------------------------
# --- How often do you adjust heating controls when house is unoccupied

new_survey_data[, .N, keyby = A10]


# A11 ----------------------------------------------------------------------
# --- During the winter, are there living spaces not normally heated

new_survey_data[, .N, keyby = A11]


# A12 ----------------------------------------------------------------------
# --- How hot water in the taps and shower is heated
# --- 12 variables, half for taps, half for showers

new_survey_data[, A12_Taps_sum := A12_Taps_GB + A12_Taps_EH + A12_Taps_SWH + 
                  A12_Taps_Other + A12_Taps_NA + A12_Taps_DK]
new_survey_data[, A12_Shower_sum := A12_Shower_GB + A12_Shower_EH + A12_Shower_SWH + 
                  A12_Shower_Other + A12_Shower_NA + A12_Shower_DK]
new_survey_data[, .N, keyby = A12_Taps_sum] # 64 with no answer
new_survey_data[, .N, keyby = A12_Shower_sum] # 150 no answer


# A13 ----------------------------------------------------------------------
# --- How often do you switch off lights not being used?
new_survey_data[, .N, keyby = A13_01]

# --- How often do you put more clothes on when you feel cold rather 
#     than putting the heating on or turning it up?
new_survey_data[, .N, keyby = A13_02]

# A14 ----------------------------------------------------------------------
# --- How much effort do you say your household makes to limit energy usage

new_survey_data[, .N, keyby = A14]

# A15 ----------------------------------------------------------------------
# --- How often will your household open the windows on 
# --- (01) a typical warm day
new_survey_data[, .N, keyby = A1501]

# --- (02) a typical cold day
new_survey_data[, .N, keyby = A1502]


# A16 ----------------------------------------------------------------------
# --- Which of the following, if any, is your household considering replacing or adding to
# ---  your heating or energy supply in the next 12 months?

# Deal with 'BLANK' and "" in the 'other - please specify' option
new_survey_data[A1610 == 0 & A16_Other != "", .N] # 0 - no issue for this version
new_survey_data[A16_Other == "BLANK" & A1610 == 0, .N] # 0 - no issue for this version

new_survey_data[, A16_sum := A1601 + A1602 + A1603 + A1604 + A1605 + 
                  A1606 + A1607 + A1608 + A1609 + A1610]
new_survey_data[, .N, keyby = A16_sum] # 69 with no response

# If not applicable - cannot do this, shouldn't have any other option 
#  (leaving the 'other' option available as people are making notes on other things there)

new_survey_data[, A16_edit := FALSE]
for(i in c(1, 3:9)){
  new_survey_data[A1602 == 1 & get(paste("A160", i, sep = "")) == 1, 
                  A16_edit := TRUE]
  new_survey_data[A1602 == 1 & get(paste("A160", i, sep = "")) == 1, 
                  eval(paste("A160", i, sep = "")) := 0]
}
new_survey_data[A16_edit == TRUE, .N] # 9 edited

new_survey_data[, A16_sum := A1601 + A1602 + A1603 + A1604 + A1605 + 
                  A1606 + A1607 + A1608 + A1609 + A1610]


new_survey_data[A1601 == 1 & A16_sum > 1, .N] # 4 not considering anything but also selected another option

for(i in c(3:9)){
  new_survey_data[A1601 == 1 & get(paste("A160", i, sep = "")) == 1, A16_edit := TRUE]
  new_survey_data[A1601 == 1 & get(paste("A160", i, sep = "")) == 1, eval(paste("A160", i, sep = "")) := 0]
} # None edited

new_survey_data[, A16_sum := A1601 + A1602 + A1603 + A1604 + A1605 + 
                  A1606 + A1607 + A1608 + A1609 + A1610]

new_survey_data[A1610 == 1, A16_Other] # More cleaning required of free text response 
#  (left to individual researchers)


# B1 ----------------------------------------------------------------------
# --- What type of accommodation do you live in

new_survey_data[, .N, keyby = B1]


# B2 ----------------------------------------------------------------------
# --- Is your accomodation self contained

new_survey_data[, .N, keyby = B2]


# B3 ----------------------------------------------------------------------
# --- How many other households do you share with

### Check B3: Number of households sharing
new_survey_data[, .N, keyby = B3] # from 0 to 6, along with -2 and -9 codes and blank
new_survey_data[B3 == "", B3 := -2]

# 17 multiple occupancy houses sharing with 0 households - could be that the rooms aren't filled. 
#  Leaving to individual researchers to recode if desired

new_survey_data[, B3 := as.integer(B3)] # was previously character as free text option


# B4 ----------------------------------------------------------------------
# --- Do you own or rent this accomodation

new_survey_data[, .N, keyby = B4]

# B5 & B6------------------------------------------------------------------
### B5
# --- How many rooms are available for use by this household

new_survey_data[, .N, keyby = B5] # 0 to 101 with -2 for no response
new_survey_data[, B5 := as.integer(B5)] # after checking that they can all be converted

### B6
# --- How many of these rooms are bedrooms

new_survey_data[, .N, keyby = B6] # 0 to 8 with -2 for no response
new_survey_data[, B6 := as.integer(B6)] # after checking that they can all be converted

### Compare for impossible combinations

new_survey_data[B5 < B6 & B5 != -2, B5:B6] # 12 with more bedrooms than rooms 

# Check for 0 bedrooms, impute as in the Census (change to 1)
new_survey_data[B6 == 0, .N] # 7
new_survey_data[B6 == 0, B6 := 1]

# Set up error flag
new_survey_data[, `:=`(B5_err = FALSE, B6_err = FALSE)]
new_survey_data[B5 < B6 & B5 != -2, `:=`(B5_err = TRUE, B5 = -4, B6_err = TRUE, B6 = -4)]


# B7 ----------------------------------------------------------------------
# --- During the cold winter weather can you keep comfortably warm

new_survey_data[, .N, keyby = B7]

# B8 ----------------------------------------------------------------------
# --- Do you have problems with condensation, damp or mould

new_survey_data[, .N, keyby = B8]

# B9 ----------------------------------------------------------------------
# --- When was your accommodation built

new_survey_data[, .N, keyby = B9]


# B10 ---------------------------------------------------------------------
# --- Appliances
# --- Multiple variables B1001:B1014

new_survey_data[, B10_sum := B1001 + B1002 + B1003 + B1004 + 
                  B1005 + B1006 + B1007 + B1008 + B1009 +
                  B1010 + B1011 + B1012 + B1013 + B1014]
new_survey_data[, .N, keyby = B10_sum] # 84 with no response


# C1 ----------------------------------------------------------------------
# --- Number of occupants

new_survey_data[, .N, keyby = C1] # 0 to 8, 9 with no occupants

new_survey_data[, C1_new := as.integer(C1)] # cleaning required, creating new variable to store cleaned version
new_survey_data[C1 == 0, C1_new := -2]

# More edits to be made to C1_new when considering C2.

# C2 ----------------------------------------------------------------------
# --- Occupants' age and gender
# Want number of males and females by age to add up to C1 (No. of occupants)

## Get the number of occupants according to C2
nParticipants <- new_survey_data[, .N]
new_survey_data[, C2_sum := sapply(1:nParticipants, 
                                function(x) sum.positive(new_survey_data[x, C2_Male_0_15:C2_Female_85_plus]))]

## Clearly some issues
new_survey_data[, .N, keyby = C2_sum] # from 0 to 151

## Deal with non-response
new_survey_data[C2_sum == 0, C2_sum := -2] # Re-code non-response

## Take the difference to identify incorrect sum
new_survey_data[, C2_sum_diff := C2_sum - C1_new]

# Define the sum as 'correct' if one or both of these questions wasn't answered
new_survey_data[C2_sum == -2 | C1_new == -2, C2_sum_diff := 0]

# Investigate
new_survey_data[C2_sum_diff != 0, .N, keyby = C2_sum_diff]

# Flag whether C2 data should be ignored 
new_survey_data[, C2_error := FALSE]
new_survey_data[C2_sum != C1_new & C2_sum != -2 & C1_new != -2, 
             C2_error := TRUE] # This will be corrected for special cases next

# Manually correct the numbers of people in each age/sex category when clear 
#  that age has been entered not number
# This isn't ideal and doesn't scale but it works well enough for our purposes.

new_survey_data[C2_sum_diff >= 5, c(1, 87:101, 146:148)]

# assumed tally notation used (recorded as 111 not 3)
new_survey_data[PUPRN == "46YBUWV1", `:=`(C2_Male_45_64 = 3,
                                                  C2_error = FALSE,
                                                  C2_sum = 3,
                                                  C2_sum_diff = 0)] 

# Age entered
new_survey_data[PUPRN == "4PJSK661", `:=`(C2_Male_75_84 = 1,
                                                  C2_error = FALSE,
                                                  C2_sum = 1,
                                                  C2_sum_diff = 0)] 

# Age entered
new_survey_data[PUPRN == "68JNQKB1", `:=`(C2_Male_65_74 = 1,
                                                  C2_error = FALSE,
                                                  C2_sum = 1,
                                                  C2_sum_diff = 0)] 

# Ages entered
new_survey_data[PUPRN == "9M2UF8L1", `:=`(C2_Male_0_15 = 1,
                                               C2_Male_25_44 = 1,
                                               C2_Female_0_15 = 1,
                                               C2_Female_25_44 = 1,
                                               C2_error = FALSE,
                                               C2_sum = 4,
                                               C2_sum_diff = 0)] 

# assumed tally notation used (recorded as 11 not 2)
new_survey_data[PUPRN == "DV5L2ZZ1", `:=`(C2_Male_75_84 = 2,
                                                  C2_error = FALSE,
                                                  C2_sum = 2,
                                                  C2_sum_diff = 0)] 

# Ages entered
new_survey_data[PUPRN == "FBCV5VQ1", `:=`(C2_Male_25_44 = 1,
                                                  C2_Female_25_44 = 1,
                                                  C2_error = FALSE,
                                                  C2_sum = 2,
                                                  C2_sum_diff = 0)] 

# Ages entered
new_survey_data[PUPRN == "GUCZ71A1", `:=`(C2_Male_75_84 = 1,
                                                  C2_Female_65_74 = 1,
                                                  C2_error = FALSE,
                                                  C2_sum = 2,
                                                  C2_sum_diff = 0)] 

# assumed 1 pressed twice in error (3 categories with '1', 1 with '11)
new_survey_data[PUPRN == "J41ESKW1", `:=`(C2_Female_25_44 = 1,
                                                  C2_error = FALSE,
                                                  C2_sum = 4,
                                                  C2_sum_diff = 0)]

# Age entered
new_survey_data[PUPRN == "X2F9CS61", `:=`(C2_Male_65_74 = 1,
                                                  C2_error = FALSE,
                                                  C2_sum = 1,
                                                  C2_sum_diff = 0)] 

# Investigate
new_survey_data[C2_sum_diff < 5 & C2_sum_diff > 0, c(1, 87:101, 146:148)]
new_survey_data[C2_sum_diff < 0, c(1, 87:101, 146:148)]

new_survey_data[C2_sum_diff != 0, .N] 
# 29 where it wasn't clear how to rectify the difference



# Calculate number of children
new_survey_data[, C2_tot_child := sapply(1:nParticipants,
                                         function(x)
                                           sum.positive(new_survey_data[x, c("C2_Male_0_15",
                                                                             "C2_Female_0_15")]))]
new_survey_data[C2_error == TRUE | C2_sum == -2, C2_tot_child := -2]

# Calculate number of adults
new_survey_data[, C2_tot_adult := sapply(1:nParticipants,
                                         function(x) {
                                           sum.positive(new_survey_data[x, c(
                                             "C2_Male_16_24",
                                             "C2_Male_25_44",
                                             "C2_Male_45_64",
                                             "C2_Male_65_74",
                                             "C2_Male_75_84",
                                             "C2_Male_85_plus",
                                             "C2_Female_16_24",
                                             "C2_Female_25_44",
                                             "C2_Female_45_64",
                                             "C2_Female_65_74",
                                             "C2_Female_75_84",
                                             "C2_Female_85_plus"
                                           )])
                                         })]

# new_survey_data[C2_error == TRUE | C2_sum == -2, .N] 186
new_survey_data[C2_error == TRUE | C2_sum == -2, C2_tot_adult := -2]

# new_survey_data[C2_tot_adult == 0, .N] 26
# Must be at least one adult (16+) in a house
new_survey_data[C2_tot_adult == 0, C2_error := TRUE] 


# Calculate number of over 65s
new_survey_data[, C2_tot_65_plus := sapply(1:nParticipants,
                                           function(x)
                                             sum.positive(new_survey_data[x, c(
                                               "C2_Male_65_74",
                                               "C2_Male_75_84",
                                               "C2_Male_85_plus",
                                               "C2_Female_65_74",
                                               "C2_Female_75_84",
                                               "C2_Female_85_plus"
                                             )]))]

new_survey_data[C2_error == TRUE | C2_sum == -2, C2_tot_65_plus := -2]

# Don't record non-response as an error
new_survey_data[C2_sum == -2, C2_error := FALSE]

# Investigate further
new_survey_data[C2_sum_diff != 0 & C1_new != -2 & C2_sum != -2, C1:C1_new]
new_survey_data[C2_sum_diff != 0 & C1_new != -2 & C2_sum != -2 & C2_sum > C1_new, 
                C1:C1_new]
new_survey_data[C2_error == TRUE, .N] # 29
new_survey_data[C2_error == TRUE, C1:C1_new]
new_survey_data[C2_sum_diff > 10, ] # none

# All household members 65+?
new_survey_data[, C2_all_65_plus := -2]
new_survey_data[C1_new > 0 & C2_error == FALSE & C2_tot_65_plus == C1_new, 
                C2_all_65_plus := 1]
new_survey_data[C1_new > 0 & C2_error == FALSE & C2_tot_65_plus < C1_new, 
                C2_all_65_plus := 0]
new_survey_data[, .N, keyby = C2_all_65_plus] 
# 997 all 65+, 1947 not all 65+, 137 we don't know


# C3 ----------------------------------------------------------------------
# --- Working status of each occupant (numbers in each category)

## Note that some of these are also applied to the pilot survey data, as they'd
#  been missed previously

## Create new variable, 'C3_sum'
new_survey_data[, C3_sum := sapply(1:nParticipants,
                                   function(x)
                                     sum.positive(new_survey_data[x, C301:C307]))]


## Deal with non-response
new_survey_data[C3_sum == 0, C3_sum := -2] # Re-code non-response
prev_updated[C3_sum == 0, C3_sum := -2]

## Take the difference to identify incorrect sum
new_survey_data[, C3_sum_diff := C3_sum - C2_tot_adult]

# Define the sum as 'correct' if one or both of these questions wasn't answered
new_survey_data[C3_sum == -2 | 
                  C1_new == -2 | 
                  C2_tot_adult == -2 | 
                  C2_error == TRUE, 
                C3_sum_diff := 0]

prev_updated[C3_sum == -2 |
               C1_new == -2 |
               C2_tot_adult == -2 |
               C2_error == TRUE,
             C3_sum_diff := 0]


## Flag whether C3 data should be ignored with C3_Error
new_survey_data[, C3_error := FALSE]
new_survey_data[C3_sum_diff > 0 & C2_tot_adult != -2, C3_error := TRUE] 
# This will be corrected for special cases next

## If C2 is an error and C3_sum = C1 then can take as valid
new_survey_data[C2_error == TRUE & C3_sum == C1_new, C3_error := FALSE]

## Investigate and manually correct where possible
new_survey_data[C3_sum_diff > 0 & C2_error == FALSE & C1_new != -2, 
             c("C1", "C1_new", "C2_tot_adult", "C3_sum", "C3_sum_diff")]  

## Investigate

new_survey_data[, C3_edit := FALSE] # to record any editing

new_survey_data[C3_sum_diff > 10 & C2_error == FALSE & C1_new != -2, 
                c(1, 87, 146, 151, 102:108, 154, 155)] # 8 instances

# Correct the ones that put the number of hours worked instead of number of adults
new_survey_data[C301 > 10 &
                  C2_tot_adult == 1, `:=`(
                    C301 = 1,
                    C3_error = FALSE,
                    C3_edit = TRUE,
                    C3_sum = 1,
                    C3_sum_diff = 0
                  )]

new_survey_data[C302 > 10 &
                  C2_tot_adult == 1, `:=`(
                    C302 = 1,
                    C3_error = FALSE,
                    C3_edit = TRUE,
                    C3_sum = 1,
                    C3_sum_diff = 0
                  )]

new_survey_data[C3_sum_diff <= 10 &
                  C3_sum_diff > 1 & C2_error == FALSE & C1_new != -2,
                c(1, 87, 146, 151, 102:108, 154, 155)] # 39  instances

# discovered the issue that some people fall into 2 categories, 
#  e.g. students or retirees with part-time job

new_survey_data[C3_error == TRUE, .N] #69

# Record if no one was working 
new_survey_data[, None_working := NA]
new_survey_data[C3_error == FALSE & C301 <= 0 & C302 <= 0, None_working := TRUE]
new_survey_data[C3_error == FALSE & (C301 > 0 | C302 > 0), None_working := FALSE]
# new_survey_data[, .N, keyby = None_working] 1551 with no one working

# C4 ----------------------------------------------------------------------
# --- How many people in household hold a degree or higher qualification

# Need to add the text entered in C4_1_TEXT to the variable C4 
#  which indicates how they responded
new_survey_data[C4 == 2, C4 := -1] # don't know/prefer not to say
new_survey_data[C4 == 1 & C4_1_TEXT == -2, C4 := -2]
new_survey_data[C4 == 1, C4 := C4_1_TEXT]

new_survey_data[, C4_error := FALSE]
new_survey_data[C4 > C1_new & C1_new != -2, C4_error := TRUE] 
new_survey_data[C4_error == TRUE, .N] # 3

new_survey_data[, .N, keyby = .(C4, C4_error)] # 1 or 2 qualified


# C5 ----------------------------------------------------------------------
# --- Does household have plug in electric vehicle

new_survey_data[, .N, keyby = C5]

# C6 ----------------------------------------------------------------------
# --- When does household charge electric vehicle at home

new_survey_data[, .N, keyby = C6] 



# D1 ----------------------------------------------------------------------
# --- Age of respondent (not required if single occupant)

new_survey_data[D1 == -2 & C1_new == 1, .N] 
# 560 should be coded as -9 (will then have data filled if available)

new_survey_data[, D1_new := D1]

# Use the fill.D1 function to populate D1 if valid answer in C2
new_survey_data[, D1_new := sapply(1:nParticipants, function(x)
  fill.D1(new_survey_data[x, ]))]

new_survey_data[, .N, keyby = D1_new]

new_survey_data[D1_new == -9, .N] # 0
new_survey_data[D1_new == -2, .N] # 136
new_survey_data[D1_new == -2 & C1_new == 1 & C2_error == FALSE, .N] # 28


# D2 ----------------------------------------------------------------------
# --- Gender of respondent (not required if single occupant)

sort(unique(new_survey_data$D2)) # -9 -3 -2  1  2  3 Fine
new_survey_data[, .N, keyby = D2] # 87 no answer, 174 not applicable as single occupant

# Use the fill.D2 function to populate D2 if valid answer in C2
new_survey_data[, D2_new := sapply(1:nParticipants, function(x)
  fill.D2(new_survey_data[x, ]))]

new_survey_data[, .N, keyby = c("D2", "D2_new")] 
# 9 didn't respond previously, total of 87 with no response


# D3 ----------------------------------------------------------------------
# --- Current employment status of respondent (not required if single occupant)

sort(unique(new_survey_data$D3)) # -9 -3 -2  1  2  3  4  5  6  7
new_survey_data[, .N, keyby = D3] # 174 not applicable, 92 no response

# Use the fill.D3 function to populate D2 if valid answer in C2
new_survey_data[, D3_new := sapply(1:nParticipants, function(x)
  fill.D3(new_survey_data[x, ]))]

new_survey_data[, .N, keyby = c("D3", "D3_new")] # only 16 couldn't have their answer filled


# D4 ----------------------------------------------------------------------
# --- How well do you manage financially

sort(unique(new_survey_data$D4)) # -3 -2  1  2  3  4  5 Fine
new_survey_data[, .N, keyby = D4]


# Final Comments-----------------------------------------------------------
# --- Removed as too disclosive

new_survey_data <- new_survey_data[, Final_comments := NULL]


# Combine all data --------------------------------------------------------

# --- Edit the pilot study data to allow for combining

# Change the name and recode the language variable to make it easier to understand
prev_updated[L1 == 1, Language := "English"]
prev_updated[L1 == 2, Language := "Welsh"]
prev_updated[, L1 := NULL]

# Recode the Collection_method variable
prev_updated[, Collection_method := as.character(Collection_method)]
prev_updated[Collection_method == "1", Collection_method := "Online"]
prev_updated[Collection_method == "2", Collection_method := "Postal"]

setnames(prev_updated, c("A401_edit", "All_65_plus"), 
         c("A405_edit", "C2_all_65_plus")) # discovered an error in the column name

# Create new columns for the pilot survey
prev_updated[, `:=`(Survey_version = "Wave1",
                    Recorded_date = NA_character_,
                    A5_C_text = NA,
                    A5_F_text = NA)
             ]


# --- Create new columns for the new survey data

if(release == "02") {
  new_survey_data[, Language := "English"] # Wales not in Wave 2
}

new_survey_data[, Cell := NA_integer_]

# Compare the columns
common_cols <- intersect(colnames(new_survey_data), 
                         colnames(prev_updated))
setdiff(colnames(new_survey_data), common_cols)
setdiff(colnames(prev_updated), common_cols)

# Combine the datasets after considering the differences between the column names
survey_data_all <- rbind(new_survey_data[, ..common_cols],
                         prev_updated[, ..common_cols])



# Final column editing ----------------------------------------------------

# Remove columns that can be linked from participant summary
survey_data_all[, `:=`(IMD_quintile = NULL, 
                       LSOA = NULL,
                       Region = NULL,
                       Cell = NULL)]

# Reorder columns
survey_reordered <- survey_data_all[, c(1, 118, 116, 117, 152,
                                        2:14, 119:121, 
                                        15:20, 122:124, 21:23, 
                                        125:126, 24:33, 127,
                                        34:35, 38, 37, 41, 40, 
                                        39, 36, 128, 44,
                                        43, 47, 46, 45, 42, 129,
                                        48:63, 130:131, 64:68, 132, 
                                        69, 133, 70:86, 134, 87, 135, 
                                        88, 90, 92, 94, 96, 98, 100,
                                        89, 91, 93, 95, 97, 99, 101,
                                        136:142, 102:108, 143:147, 
                                        109, 148, 110:112, 149, 
                                        113, 150, 114, 151, 115)]

colnames(survey_reordered)
x <- colnames(survey_data_all)
y <- colnames(survey_reordered)
x[!x %in% y]
y[!y %in% x]
min(sort(colnames(survey_reordered)) == sort(colnames(survey_data_all))) 
# 1 therefore all match


# Save --------------------------------------------------------------------
fwrite(survey_reordered, 
       file = paste(location_processed, survey_output_name, ".csv", sep = ""))

save(survey_reordered, 
     file = paste(location_RData, survey_output_name, ".RData", sep = ""))


