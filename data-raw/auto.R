
devtools::load_all()
dir_path <- tempdir()
if (!dir.exists(dir_path)) {
  dir.create(dir_path)
} else {
  unlink(dir_path, recursive = TRUE, force = TRUE)
  dir.create(dir_path)
}
iarccrgtools::iarc_workdir_set(dir_path)

tool_name <- "multiple_primary"
iarc_df <- iarccrgtools::tool_colnameset_example_dataset(
  paste0("mandatory_", tool_name), n.rows = 10L
)
if ("subject_id" %in% names(iarc_df)) {
  iarc_df[["subject_id"]][1:3] <- 1L
}
if ("record_order" %in% names(iarc_df)) {
  iarc_df[["record_order"]][1:3] <- 1:3
}

auto <- iarccrgtools:::interface_with_tool(
  iarc_df, tool.name = tool_name, clean = FALSE, how = "automatically"
)
unlink(dir_path, recursive = TRUE, force = TRUE)
