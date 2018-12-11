


## - modify data-raw/program_output_files.csv by hand
## - use this script to update the external data in 
##   data/program_output_files.rda

program_output_files <- read.table(
  file = "data-raw/program_output_files.csv",
  sep = ";",
  dec = ",",
  header = TRUE,
  colClasses = c("character", "character", "logical")
)



usethis::use_data(program_output_files, overwrite = TRUE)





