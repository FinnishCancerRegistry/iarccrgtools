


## - modify data-raw/column_specifications.csv by hand
## - use this script to update the external data in 
##   data/column_specifications.rda

column_specifications <- read.table(
  file = "data-raw/column_specifications.csv",
  sep = ";",
  dec = ",",
  header = TRUE,
  stringsAsFactors = FALSE
)



usethis::use_data(column_specifications, overwrite = TRUE)



