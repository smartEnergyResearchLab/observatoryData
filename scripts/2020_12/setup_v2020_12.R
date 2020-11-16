# Stores the file information and variables for each specific release


# Inputs to change for each new release -----------------------------------

release_version <- "2020_12"
release_date <- "2020-12"
survey_version <- "02" # What do we do about having 2 survey versions? 
most_recent_smart_meter_date <- "2020-10-31"
observatory_release_ext <- "Observatory2020_12/"

## Data filenames
climate_file_dates <- c("Aug-Sep 2018",
                        "Oct-Dec 2018",
                        "Jan-Mar 2019",
                        "Apr-Jun 2019",
                        "Jul-Sep 2019",
                        "Oct-Dec 2019",
                        "Jan-Mar 2020")

daily_filename <- "Daily Readings Aug2018-Jul2020.csv"
hh_filename <- "Half-Hourly Readings Aug2018-Jul2020.csv"
epc_filename <- "SERL EPC Data.csv"
inventory_filename <- "Monthly Inventory Data 2020-08-03.csv"
onboarding_filename <- "Onboarding_Report_2020-08-27.csv"
participant_details_filename <- "SERL Participants Data 2020-07-09.csv"
theoret_dates_filename <- "actualStart_20_08_10.csv"

survey_previous_filename <- "SERL_survey_data_v01.csv"
survey_online_filename <- "SERL_Survey_Wave2_Online_2020-11-03.csv"
survey_postal_filename <- "SERL_Survey_Wave2_Postal_2020-11-03.csv"
survey_online_variables_filename <- "SERL_Wave_2_Survey_v1.8_Online_Variables.xlsx"
survey_postal_variables_filename <- "SERL_Wave_2_Survey_v1.8_Postal_Variables.xlsx"


# Inputs to check for each new release ------------------------------------

## Output locations
main_folder <- "S:/ENERGINST_EaB_Project_17_SMRP/Data/Researcher data/"
original_data_ext <- "Original/"
processed_data_ext <- "Processed/"
processed_RData_ext <- "Processed/RData files/"

## Input locations
climate_location <- "S:/ENERGINST_EaB_Project_17_SMRP/Data/Researcher data/Climate data/"
epc_location <- "S:/ENERGINST_EaB_Project_17_SMRP/Data/Researcher data/EPC data/"
inventory_location <- "S:/ENERGINST_EaB_Project_17_SMRP/Data/Inventory/"
onboarding_location <- "S:/ENERGINST_EaB_Project_17_SMRP/Data/Onboarding/"
participant_details_location <- "S:/ENERGINST_EaB_Project_17_SMRP/Data/Researcher data/"
survey_location <- paste(main_folder, observatory_release_ext, original_data_ext, "survey_data/", sep = "")


# No input required -------------------------------------------------------

## Source functions
source("N:/R/observatoryData/scripts/functions/get.serl.filename.R")
source("N:/R/observatoryData/scripts/functions/create.info.table.R")

## Define common variables
fig_caps <- captioner::captioner(prefix = "Figure")
tab_caps <- captioner::captioner(prefix = "Table")

## Create the information table for the start of documents
info_tab <- create.info.table()

## Define locations
location_orig <- paste(main_folder,observatory_release_ext, original_data_ext, sep = "")
location_processed <- paste(main_folder, observatory_release_ext, processed_data_ext, sep = "")
location_RData <- paste(main_folder, observatory_release_ext, processed_RData_ext, sep = "")

## Data file paths
epc_file <- paste(epc_location, epc_filename, sep = "")
inventory_file <- paste(inventory_location, inventory_filename, sep = "")
onboard_file <- paste(onboarding_location, onboarding_filename, sep = "")
participant_details_file <- paste(participant_details_location, participant_details_filename, sep = "")
theoretical_dates_file <- paste(location_orig, theoret_dates_filename, sep = "")

survey_previous_file <- paste(survey_location, survey_previous_filename, sep = "")
survey_online_file <- paste(survey_location, survey_online_filename, sep = "")
survey_postal_file <- paste(survey_location, survey_postal_filename, sep = "")
survey_online_variables_file <- paste(survey_location, survey_online_variables_filename, sep = "")
survey_postal_variables_file <- paste(survey_location, survey_postal_variables_filename, sep = "")

## Output file paths
climate_output_name <- get.serl.filename("climate_data", release_version)
epc_output_name <- get.serl.filename("epc_data", release_version)
survey_output_name <- get.serl.filename("survey_data", survey_version)

## RData files
climate_stats_file <- paste(location_RData, "climate_stats.RData", sep = "")
epc_stats_file <- paste(location_RData, "epc_stats.RData", sep = "")
survey_stats_file <- paste(location_RData, "survey_stats.RData", sep = "")

daily_processed_file <- paste(location_RData, 
                              get.serl.filename("daily_data", release_version), 
                              ".RData", 
                              sep = "")
hh_processed_file <- paste(location_RData,
                           get.serl.filename("hh_data", release_version),
                           ".RData",
                           sep = "")
rt_processed_file <- paste(location_RData,
                           get.serl.filename("rt_data", release_version),
                           ".RData",
                           sep = "")
ps_processed_file <- paste(location_RData,
                           get.serl.filename("ps_data", release_version),
                           ".RData",
                           sep = "")

sm_doc_input_file <- paste(location_RData, "sm_data_documentation_input.RData", sep = "")

dq_input_file <- paste(location_RData, "DQ_input.RData", sep = "")


