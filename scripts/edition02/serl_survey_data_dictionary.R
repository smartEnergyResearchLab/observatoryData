# This code prepares the table of questions and response codes for the
# SERL surveys (all waves) as part of the 2020-12 SERL Observatory Data Release.

# Code developed by Ellen Webborn, UCL using R version 4.0.3 (2020-10-10)
# Most recent edits made 2020-11-16

# For the latest versions of all code and documentation please visit the SERL
#  GitHub page https://github.com/smartEnergyResearchLab


# Setup -------------------------------------------------------------------


# load relevant libraries 
library(data.table)
library(stringr)

source("D:/Users/ucldata/observatory_release_pre_processing/observatoryData/scripts/edition02/setup_edition02.R")
# load files
questions <- fread(questions_table_file)
poss_resp <- fread(response_codes_file)
load(survey_file)

# Functions ---------------------------------------------------------------

add.variable <- function(var, vals, labs, q, 
                         ft = FALSE, t = "Derived",
                         dt = survey_questions) {
  n <- length(vals)
  tmp <- data.table(Variable = rep(var, n),
                    Value = vals,
                    Label = labs,
                    i.Label = rep(q, n),
                    freeText = ft,
                    Type = rep(t, n))
  output <- rbind(dt, tmp)
  return(output)
}

# record the new response -4 indicating 'unusable' due to an error found
record.unusable.response <- function(var, table.to.edit = survey_questions) {
  table.to.edit <- rbind(table.to.edit, table.to.edit[Variable == var][1])
  i.last <- nrow(table.to.edit)
  table.to.edit[i.last, Value := "-4"]
  table.to.edit[i.last, Label := "Unusable"]
  setkey(table.to.edit, Variable)
  table.to.edit
}

swap.var.names <- function(old_names, new_names, dt = survey_questions) {
  if(length(old_names) != length(new_names)) {
    return("Error: old_names and new_names must have the same length")
  }
  for(i in 1:length(old_names)) {
    dt[Variable == old_names[i], Variable := new_names[i]]
  }
  return(dt)
} 


# Initial processing ------------------------------------------------------



# get rid of NAs formed from merged cells in the original
for(i in 2:nrow(poss_resp)) {
  if(is.na(poss_resp[i,1]) == TRUE | poss_resp[i, 1] == "") {
    poss_resp[i,1] <- poss_resp[i-1,1]
  }
}

# get rid of superfluous extra row
poss_resp <- poss_resp[-331,]

# Combine
setkey(questions, Variable)
setkey(poss_resp, Variable)
survey_questions <- poss_resp[questions]


### Identify (manually) which questions may have more answers than the set provided e.g. free text
survey_questions[, freeText := FALSE]
freeText_variabes = c("A3_Other", "A5","A16_Other", "B3", "B5", "B6", "C1", "C2", "C3", "C4",
                      "QUINTILE", "LSOA", "REGION")
survey_questions[Variable %in% freeText_variabes, freeText := TRUE]
survey_questions[startsWith(Variable, "C2_") |
                   startsWith(Variable, "C3"), 
                 freeText := TRUE]


# Add new response option to the survey questions
survey_questions <- record.unusable.response("B5")
survey_questions <- record.unusable.response("B6")

# Issue that survey responses are not all integers according to the questions doc, but they are in the data
survey_questions[str_detect(Value, "a"), Value := str_replace(Value, "a", "")]
survey_questions[str_detect(Value, ".00"), Value := str_replace(Value, ".00", "")]
survey_questions[, Value := as.integer(Value)]


# Deal with labels written incorrectly
survey_questions[Value == -2, Label := "No answer"]
survey_questions[Value == -1, Label := "Don't know"]
survey_questions[Value == -1 & Variable == "A5", 
                 Label := "Don't know/doesn't apply"]
survey_questions[Label == "Donâ€™t know", Label := "Don't know"]
survey_questions[Label == "Dont know / Doesnt apply", 
                 Label := "Don't know / Doesn't apply"]
survey_questions[Value == -1 & Variable == "C4", 
                 Label := "Don't know/prefer not to say"]
survey_questions[Value == 3 & Label == "Rarely â€“ only if I/we really have to", 
                 Label := "Rarely - only if I/we really have to"]
survey_questions[Value == 4 & Label == "Varies - whenever itâ€™s needed", 
                 Label := "Varies - whenever it's needed"]
survey_questions[Value == 5 & Label == "Varies â€“ depends on temperature or other reasons", 
                 Label := "Varies - depends on temperature or other reasons"]
survey_questions[Label == "a 'No answer'", Label := "No answer"]
survey_questions[, i.Label := str_replace(i.Label, " dont ", " don't ")]


# Amend/add labels and variable names
survey_questions[Label == "Cymraeg", Label := "Welsh"] # May be confusing to use the Welsh for Welsh
swap.var.names(c("L1", "Collection_Method"),
               c("Language", "Collection_method"))



# Add column to show if a variable is primary or derived
survey_questions[, Type := "Primary"]

# Remove those not in use
survey_questions <- survey_questions[!Variable %in% c("Respondent_ID",
                                                      "Respondent_Serial"), ]

# Check for variable name changes in the new data
setdiff(survey_questions$Variable,
        colnames(survey_reordered))


setdiff(colnames(survey_reordered),
       survey_questions$Variable)

# Correct typos, improve descriptions


survey_questions[Variable == "A901", 
                 i.Label := "Do you adjust your heating - When especially cold"]

survey_questions[Variable == "A10",
                 i.Label := "How do you adjust heating controls when house is unoccupied"]

survey_questions[, i.Label := stringr::str_replace_all(i.Label, "accomodation", "accommodation")]

survey_questions[Variable == "B1013", 
                 i.Label := "Appliances in accommodation - Cooling - air conditioning unit"]

survey_questions <- survey_questions[Variable != "CELL"]



# Add new variables
survey_questions <- add.variable(var = "PUPRN", 
                     vals = "",
                     labs = "",
                     q = "Pseudonymised participant identifier",
                     t = "Assigned")

# survey_questions <- add.variable(var = "Region", 
#                                  vals = "",
#                                  labs = "",
#                                  q = "Region in GB",
#                                  t = "Assigned")
# 
# survey_questions <- add.variable(var = "LSOA", 
#                                  vals = "",
#                                  labs = "",
#                                  q = "Lower Super Output Area in GB",
#                                  t = "Assigned")
# 
# survey_questions <- add.variable(var = "IMD_quintile", 
#                                  vals = "",
#                                  labs = "",
#                                  q = "Index of multiple deprivation quintile (1 is most deprived, 5 is least deprived)",
#                                  t = "Assigned")

survey_questions <- add.variable(var = "Recorded_date", 
                                 vals = "",
                                 labs = "",
                                 q = "Date survey was recorded by participants (if online) or entered online from postal response",
                                 t = "Assigned")

survey_questions <- add.variable(var = "Survey_version", 
                                 vals = c("Wave1", "Wave2", "Wave3"),
                                 labs = c("Wave 1 (pilot)", "Wave 2", "Wave 3"),
                                 q = "Version of the survey received",
                                 t = "Assigned")

survey_questions <- add.variable(var = "A3_sum", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of response options selected for A3 (A301 + A302 + .. + A310)",
                                 t = "Derived")

survey_questions <- add.variable(var = "A3_err", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("'No central heating' and another option selected",
                                          "Otherwise"),
                                 q = "Clear error in A3 response",
                                 t = "Derived")

survey_questions <- add.variable(var = "A301_edit", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("A301 was edited from 1 to 0 since A3_err",
                                          "No edits to A301"),
                                 q = "Has A301 been edited",
                                 t = "Derived")

survey_questions <- add.variable(var = "A4_sum", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of response options selected for A4 (A401 + A402 + .. + A406)",
                                 t = "Derived")

survey_questions <- add.variable(var = "A4_err", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("'None of these' and another option selected",
                                          "Otherwise"),
                                 q = "Clear error in A4 response",
                                 t = "Derived")

survey_questions <- add.variable(var = "A405_edit", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("A405 was edited from 1 to 0 since A4_err",
                                          "No edits to A405"),
                                 q = "Has A405 been edited",
                                 t = "Derived")

survey_questions <- add.variable(var = "A5_C_text", 
                                 vals = "",
                                 labs = "Degrees C",
                                 q = "Temperature controller set at for late afternoons or evenings",
                                 t = "Primary",
                                 ft = TRUE)

survey_questions <- add.variable(var = "A5_F_text", 
                                 vals = "",
                                 labs = "Degrees F",
                                 q = "Temperature controller set at for late afternoons or evenings",
                                 t = "Primary",
                                 ft = TRUE)

survey_questions <- add.variable(var = "A5_degC", 
                                 vals = "",
                                 labs = "",
                                 q = "Temperature in deg C, converted from deg F if > 35 (survey 1) or if given in deg F (later surveys)",
                                 t = "Derived",
                                 ft = TRUE)

survey_questions <- add.variable(var = "A5_edit", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("A5 was edited due to non-integer response or wrong units",
                                          "No edits to A5"),
                                 q = "Has A5 been edited",
                                 t = "Derived")

survey_questions <- add.variable(var = "A9_sum", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of response options selected for A9 (A901 + A902 + .. + A907)",
                                 t = "Derived")

survey_questions <- add.variable(var = "A12_Taps_sum", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of response options selected for A12 (taps)",
                                 t = "Derived")

survey_questions <- add.variable(var = "A12_Shower_sum", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of response options selected for A12 (shower)",
                                 t = "Derived")

survey_questions <- add.variable(var = "A16_sum", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of response options selected for A16 (A1601 + A1602 + .. + A1610)",
                                 t = "Derived")

survey_questions <- add.variable(var = "A16_edit", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("A16 was edited if 'not applicable' or 'not considering any changes' were selected with other options (excludes 'other')",
                                          "No edits to A16"),
                                 q = "Has A16 been edited",
                                 t = "Derived")

survey_questions <- add.variable(var = "B5_err", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("B5 < B6 (fewer rooms than bedrooms) or B5 == 0 (no rooms)",
                                          "B5 >= B6 and B5 != 0"),
                                 q = "Clear error in B5 response",
                                 t = "Derived")

survey_questions <- add.variable(var = "B6_err", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("B5 < B6 (fewer rooms than bedrooms)",
                                          "B5 >= B6"),
                                 q = "Clear error in B6 response",
                                 t = "Derived")

survey_questions <- add.variable(var = "B10_sum", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of response options selected for B10 (B1001 + B1002 + .. + B1014)",
                                 t = "Derived")

survey_questions <- add.variable(var = "C1_new", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of occupants after error correction",
                                 t = "Derived",
                                 ft = TRUE)

survey_questions <- add.variable(var = "C2_sum", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of occupants from C2 responses",
                                 t = "Derived")

survey_questions <- add.variable(var = "C2_sum_diff", 
                                 vals = "",
                                 labs = "",
                                 q = "C2_sum - C1_new",
                                 t = "Derived")

survey_questions <- add.variable(var = "C2_error", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("C2_sum_dif != 0 after obvious error correction", 
                                          "C2_sum_diff == 0"),
                                 q = "Clear error in C2 response",
                                 t = "Derived")

survey_questions <- add.variable(var = "C2_tot_child", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of occupants aged under 16",
                                 t = "Derived",
                                 ft = TRUE)

survey_questions <- add.variable(var = "C2_tot_adult", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of occupants aged 16+",
                                 t = "Derived",
                                 ft = TRUE)

survey_questions <- add.variable(var = "C2_tot_65_plus", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of occupants aged 65+",
                                 t = "Derived",
                                 ft = TRUE)

survey_questions <- add.variable(var = "C2_all_65_plus", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("All aged 65+", "Not all aged 65+"),
                                 q = "All occupants aged 65+",
                                 t = "Derived",
                                 ft = TRUE)

survey_questions <- add.variable(var = "C3_sum", 
                                 vals = "",
                                 labs = "",
                                 q = "Number of response options selected for C3 (C307 + C302 + .. + C307)",
                                 t = "Derived")

survey_questions <- add.variable(var = "C3_sum_diff", 
                                 vals = "",
                                 labs = "",
                                 q = "C3_sum - C2_tot_adult",
                                 t = "Derived")

survey_questions <- add.variable(var = "C3_error", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("C3_sum_dif != 0 and C2_tot_adult calculatable", 
                                          "Otherwise"),
                                 q = "Clear error in C3 response given adults reported",
                                 t = "Derived")

survey_questions <- add.variable(var = "C3_edit", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("C3_sum_diff == 0 and C3 and C2 both answered", 
                                          "Otherwise"),
                                 q = "Has C3 been edited",
                                 t = "Derived")

survey_questions <- add.variable(var = "None_working", 
                                 vals = c("TRUE", "FALSE", "NA"),
                                 labs = c("No adults working", "Some adults working", "Not possible to determine"),
                                 q = "No adults work",
                                 t = "Derived")

survey_questions <- add.variable(var = "C4_error", 
                                 vals = c(TRUE, FALSE),
                                 labs = c("C4 > C1_new and C1 answered (more qualified than occupants)", 
                                          "Otherwise"),
                                 q = "Clear error in C4 response given occupants reported",
                                 t = "Derived")

survey_questions <- add.variable(var = "D1_new", 
                                 vals = survey_questions[Variable == "D1" & Value != -9, Value],
                                 labs = survey_questions[Variable == "D1" & Value != -9, Label],
                                 q = "Individual's age group, filled from C1 if single occupier",
                                 t = "Derived")

survey_questions <- add.variable(var = "D2_new", 
                                 vals = survey_questions[Variable == "D2" & Value != -9, Value],
                                 labs = survey_questions[Variable == "D2" & Value != -9, Label],
                                 q = "Individual's gender, filled from C2 if single occupier",
                                 t = "Derived")

survey_questions <- add.variable(var = "D3_new", 
                                 vals = survey_questions[Variable == "D3" & Value != -9, Value],
                                 labs = survey_questions[Variable == "D3" & Value != -9, Label],
                                 q = "Individual's working status, filled from C3 if single occupier",
                                 t = "Derived")


survey_questions[Variable == "Collection_method" & Value == "1", Value := "Online"]
survey_questions[Variable == "Collection_method" & Value == "2", Value := "Postal"]


setdiff(survey_questions$Variable,
        colnames(survey_reordered))


setdiff(colnames(survey_reordered),
        survey_questions$Variable)
# both the same

# Remove codes left in by mistake
survey_questions <- survey_questions[!(Variable == "D2" & Value == -1)]
survey_questions <- survey_questions[!(Variable == "D3" & Value == -1)]


order_in_survey <- data.table(Variable = colnames(survey_reordered),
                              ordering = 1:ncol(survey_reordered))
setkey(order_in_survey, Variable)
setkey(survey_questions, Variable)

survey_questions <- order_in_survey[survey_questions]
setkey(survey_questions, ordering)
unique(survey_questions$Variable)


# Create public version ---------------------------------------------------

data_dictionary <- copy(survey_questions)
data_dictionary[, ordering := NULL]

data_dictionary[!Variable%in%colnames(survey_reordered)] # None, all there
colnames(survey_reordered)[!colnames(survey_reordered)%in% data_dictionary$Variable] # None, all there

setnames(data_dictionary, 
         c("Label",
           "i.Label",
           "freeText"),
         c("ValueDescription",
           "QuestionOrMeaning",
           "FreeText"))

# Save --------------------------------------------------------------------
fwrite(data_dictionary, 
       file = paste(location_processed, survey_dictionary_output_name, ".csv", sep = ""))

save(survey_questions, 
     file = paste(location_RData, survey_dictionary_output_name, ".RData", sep = ""))



