
library("data.table")
library("usethis")

tools <- data.table::fread("data-raw/tools.csv")
column_specifications <- data.table::fread("data-raw/column_specifications.csv")
tool_output_files <- data.table::fread("data-raw/tool_output_files.csv")
tool_parameter_contents <- data.table::fread(
  "data-raw/tool_parameter_contents.csv"
)

message("re-generate multiple_primary_validation_result? it is only used in ",
        "a unit test so if that passes you dont need to. [y/n]")
if (readline(": ") == "y") {
  e <- new.env()
  source("data-raw/auto.R", local = e)
  multiple_primary_validation_result <- e[["auto"]]
} else {
  multiple_primary_validation_result <-
    iarccrgtools:::multiple_primary_validation_result
}

usethis::use_data(tools,
                  column_specifications,
                  tool_output_files,
                  tool_parameter_contents,
                  multiple_primary_validation_result,
                  overwrite = TRUE, internal = TRUE)
