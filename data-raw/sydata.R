
library("data.table")
library("usethis")

programs <- data.table::fread("data-raw/programs.csv")
column_specifications <- data.table::fread("data-raw/column_specifications.csv") 
program_output_files <- data.table::fread("data-raw/program_output_files.csv") 



usethis::use_data(programs, column_specifications, program_output_files,
                  overwrite = TRUE, internal = TRUE)
