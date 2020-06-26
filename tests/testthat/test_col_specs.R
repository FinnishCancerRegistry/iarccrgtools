context("column info")



test_that("all column name sets appear in column_specifications", {

  col_specs <- get_exported_dataset("column_specifications")

  req_nms <- paste0("set_", tool_colnameset_names())

  expect_true(
    all(req_nms %in% names(col_specs))
  )

})







