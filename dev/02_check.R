
library("devtools")

chk <- devtools::check()
print(chk)

devtools::load_all()

if (grepl("[.]exe$", iarccrgtools::iarc_exe_path_get(), ignore.case = TRUE)) {
  source("tests/testthat/test_auto.R")
}
