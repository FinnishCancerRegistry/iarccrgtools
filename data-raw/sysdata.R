
library("data.table")
library("usethis")

tools <- data.table::fread("data-raw/tools.csv")
column_specifications <- data.table::fread("data-raw/column_specifications.csv")
tool_output_files <- data.table::fread("data-raw/tool_output_files.csv")
tool_parameter_contents <- data.table::fread(
  "data-raw/tool_parameter_contents.csv"
)

usethis::use_data(tools, column_specifications, tool_output_files,
                  tool_parameter_contents,
                  overwrite = TRUE, internal = TRUE)

