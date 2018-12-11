context("column info")



test_that("all column name sets appear in column_specifications", {
  
  col_specs <- get_program_definition_data("column_specifications")
  
  req_nms <- paste0("set_", tools_program_colnameset_names())
  
  expect_true(
    all(req_nms %in% names(col_specs))
  )
  
})







