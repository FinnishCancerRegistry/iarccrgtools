
## use this scipt to update the external data in data/progran_guides.rda

program_guides <- read.table(file = "data-raw/program_guides.csv",
                             sep = ";",
                             dec = ",",
                             header = TRUE,
                             colClasses = rep("character", 3))



usethis::use_data(program_guides, overwrite = TRUE)

