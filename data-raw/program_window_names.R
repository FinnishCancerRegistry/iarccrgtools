


program_window_names <- data.frame(data.table::fread(
  input = "data-raw/program_window_names.csv"
))



usethis::use_data(program_window_names, internal = FALSE, overwrite = TRUE)

