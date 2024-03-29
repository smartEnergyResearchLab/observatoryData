#' Generates the filename of an observatory data file
#'
#' \code{get.serl.filename} reads the file specifics and gives the filename to be used 
#' with the right format, version number etc.
#'
#' Returns a new string
#'
#' @param file_identifier identifies file using common name, e.g. "epc_data"
#' @param version_id e.g. "2020_08" or "01" (for survey data)
#'
#' @import 
#' @family smartMeter
#'
#' @author Ellen Webborn, \email{e.webborn@@ucl.ac.uk}
#' @export


get.serl.filename <- function(file_identifier, version_id) {
  # Valid file identifiers
  
  ## climate_data
  ## climate_doc
  ## epc_doc
  ## epc_data
  ## sm_doc
  ## sm_code
  ## sm_dq_report
  ## hh_data
  ## daily_data
  ## rt_data
  ## ps_data
  ## survey_doc
  ## survey_code
  ## survey_data
  ## survey_questions
  ## survey_responses
  ## readme

  if(file_identifier == "climate_data") {
    name <- "climate_data"
  } else if(file_identifier == "climate_doc") {
    name <- "climate_documentation"
  } else if(file_identifier == "epc_doc") {
    name <- "epc_documentation"
  } else if(file_identifier == "epc_data") {
    name <- "epc_data"
  } else if(file_identifier == "sm_doc") {
    name <- "smart_meter_documentation"
  } else if(file_identifier == "sm_code") {
    name <- "smart_meter_data_prep"
  } else if(file_identifier == "sm_dq_report") {
    name <- "smart_meter_data_quality_report"
  } else if(file_identifier == "hh_data") {
    name <- "smart_meter_hh"
  } else  if(file_identifier == "daily_data") {
    name <- "smart_meter_daily"
  } else if(file_identifier == "rt_data") {
    name <- "smart_meter_rt_summary"
  } else if(file_identifier == "ps_data") {
    name <- "participant_summary"
  } else if(file_identifier == "survey_doc") {
    name <- "survey_documentation"
  } else if(file_identifier == "survey_code") {
    name <- "survey_data_prep"
  } else if(file_identifier == "survey_data") {
    name <- "survey_data"
  } else if(file_identifier == "survey_questions") {
    name <- "survey_questions"    
  } else if(file_identifier == "survey_responses") {
    name <- "survey_responses_summary"     
  } else if(file_identifier == "survey_dictionary") {
    name <- "survey_data_dictionary"    
  } else if(file_identifier == "readme") {
    name <- "README_data_and_documentation_summary"
  } else {print("Incorrect file identifier")}
  
  filename <- paste("serl_", name, "_", version_id, sep = "")
  return(filename)
}

