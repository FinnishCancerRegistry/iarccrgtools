
if (interactive()) {
  testthat::test_that("automatic use works", {
    dir_path <- tempdir()
    if (!dir.exists(dir_path)) {
      dir.create(dir_path)
    } else {
      unlink(dir_path, recursive = TRUE, force = TRUE)
      dir.create(dir_path)
    }
    iarccrgtools::iarc_workdir_set(dir_path)

    tool_name <- "multiple_primary"
    col_nm_set <- paste0("all_", tool_name)
    iarc_df <- iarccrgtools::tool_colnameset_example_dataset(
      col_nm_set, n.rows = 10L
    )
    iarc_df[["icdo3_topography"]] <- sub(
      "(?=[0-9]$)",
      ".",
      paste0("C", iarc_df[["icdo3_topography"]]),
      perl = TRUE
    )
    if ("subject_id" %in% names(iarc_df)) {
      iarc_df[["subject_id"]][1:3] <- 1L
    }
    if ("record_order" %in% names(iarc_df)) {
      iarc_df[["record_order"]][1:3] <- 1:3
    }

    obs <- iarccrgtools:::interface_with_tool(
      iarc_df, tool.name = tool_name, clean = FALSE, how = "automatically"
    )
    unlink(dir_path, recursive = TRUE, force = TRUE)
    
    exp <- get_internal_dataset("multiple_primary_validation_result")

    testthat::expect_equal(obs, exp)
  })
}