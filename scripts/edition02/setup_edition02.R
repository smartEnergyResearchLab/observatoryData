# Stores the file information and variables for each specific release


# Inputs to change for each new release -----------------------------------

release_version <- "edition02"
release <- "02"
survey_version <- "02" 
most_recent_smart_meter_date <- "2020-10-31"

climate_filename <- "Climate_Data_Aug2018-Oct2020.csv"
daily_filename <- "Daily_Readings_Aug2018-Oct2020.csv"
hh_filename <- "Half-Hourly_Readings_Aug2018-Oct2020.csv"
epc_filename <- "SERL EPC Data.csv"
inventory_filename <- "Inventory_Data_2020-12-17.csv"
participant_details_filename <- "SERL_Participants_Data_2020-12-17b.csv"
theoret_dates_filename <- "actual_start_report_2020_12_17.csv"

survey_previous_filename <- "SERL_survey_data_v01.csv"
survey_online_filename <- "SERL_Survey_Wave2_Online_2020-11-03b.csv"
survey_postal_filename <- "SERL_Survey_Wave2_Postal_2020-11-03.csv"
survey_online_variables_filename <- "SERL_Wave_2_Survey_v1.8_Online_Variables.xlsx"
survey_postal_variables_filename <- "SERL_Wave_2_Survey_v1.8_Postal_Variables.xlsx"

postal_survey_consent_form_filename <- "serl_main_recruitment_survey_copy.pdf"
pilot_survey_consent_form_filename <- "serl_pilot_recruitment_survey_copy.pdf"


# No input required -------------------------------------------------------

## Source functions
source("D:/Users/ucldata/observatory_release_pre_processing/observatoryData/scripts/functions/get.serl.filename.R")
source("D:/Users/ucldata/observatory_release_pre_processing/observatoryData/scripts/functions/create.info.table.R")

## Define common variables
fig_caps <- captioner::captioner(prefix = "Figure")
tab_caps <- captioner::captioner(prefix = "Table")

## Create the information table for the start of documents
info_tab <- create.info.table()

## Define locations CHECK
data_directory <- "D:/Users/ucldata/Data/"
location_orig <- paste(data_directory, release_version, "/Original/", sep = "")
location_processed <- paste(data_directory, release_version, "/Processed/", sep = "")
location_RData <- paste(location_processed, "RData files/", sep = "")
location_survey <- paste(location_orig, "survey_data/", sep = "")
location_climate <- paste(location_orig, "climate_data/", sep = "")

## Data file paths
inventory_file <- paste(location_orig, inventory_filename, sep = "")
epc_file <- paste(location_orig, epc_filename, sep = "")
participant_details_file <- paste(location_orig, participant_details_filename, sep = "")
theoretical_dates_file <- paste(location_orig, theoret_dates_filename, sep = "")

survey_previous_file <- paste(location_survey, survey_previous_filename, sep = "")
survey_online_file <- paste(location_survey, survey_online_filename, sep = "")
survey_postal_file <- paste(location_survey, survey_postal_filename, sep = "")
survey_online_variables_file <- paste(location_survey, survey_online_variables_filename, sep = "")
survey_postal_variables_file <- paste(location_survey, survey_postal_variables_filename, sep = "")

questions_table_file <- paste(location_survey, "survey_questions.csv", sep = "")
response_codes_file <- paste(location_survey, "possible_responses.csv", sep = "")

## Output file paths
climate_output_name <- get.serl.filename("climate_data", release_version)
epc_output_name <- get.serl.filename("epc_data", release_version)
survey_output_name <- get.serl.filename("survey_data", release_version)
survey_dictionary_output_name <- get.serl.filename("survey_dictionary", release_version)

## RData files
climate_stats_file <- paste(location_RData, "climate_stats.RData", sep = "")
epc_stats_file <- paste(location_RData, "epc_stats.RData", sep = "")
survey_file <- paste(location_RData, survey_output_name, ".RData", sep = "")
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


