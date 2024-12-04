#########################################################################
#
# Program: p001_FunctionsFile
# Purpose: The purpose of this program is to contain all functions for
#           the scope of this survival data analysis
# Author:  Sarah Bird
#
#########################################################################



generate_Edu <- function(edu_var) {
  case_when(
    edu_var %in% c("0", "1", "2", "3", "4", "5", "6", "7") ~ "Less than High School Diploma",
    edu_var %in% c("8") ~ "High School Diploma",
    edu_var %in% c("10", "11") ~ "Some College",
    edu_var == "9" ~ "Business/Technical School",
    edu_var == "12" ~ "Associate Degree",
    edu_var == "13" ~ "Bachelor's Degree",
    edu_var == "14" ~ "Masters Degree",
    edu_var == "15" ~ "Professional or Doctorate Degree",
    edu_var == "16" ~ "Don't Know",
    TRUE ~ "99"
  )
}

generate_Income <- function(income_var) {
  case_when(
    income_var %in% c("1", "2") ~ "$0-$24,999",
    income_var == "3" ~ "$25,000-$49,999",
    income_var == "4" ~ "$50,000-$74,999",
    income_var == "5" ~ "$75,000+",
    income_var %in% c("6", "7") ~ "Don't know or Refused"
  )
}

generate_Race <- function(race_var) {
  case_when(
    race_var == "1" ~ "Non-Hispanic White",
    race_var == "2" ~ "Hispanic",
    race_var == "3" ~ "Non-Hispanic Black",
    race_var == "4" ~ "Non-Hispanic Other"
  )
}

generate_Sex <- function(sex_var) {
  case_when(
    child_sex == "F" ~ "Female",
    child_sex == "M" ~ "Male"
  )
}

# Create function we'll use in generation of tables
writeTex <- function(table, destination, texname) {
  # Create the full LaTeX document
  full_tex <- c(
    "\\documentclass{article}",
    "\\usepackage{booktabs}",
    "\\usepackage{graphicx}",
    "\\usepackage[table]{xcolor}",  # Add this line for coloring cells
    "\\usepackage[labelformat=empty]{caption}",
    "\\begin{document}",
    "",
    "% LaTeX table",
    table,
    "",
    "\\end{document}"
  )
  # Write the full LaTeX document to a file
  writeLines(full_tex, paste0(destination, "/", texname))
}



