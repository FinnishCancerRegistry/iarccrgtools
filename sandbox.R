binary_string_to_integer <- function(x) {
  stopifnot(
    is.character(x),
    nchar(x) == 8
  )
  bits <- as.integer(strsplit(x, split = "")[[1L]])
  as.integer(sum((2L * bits) ^ (8:1)))
}

integer_to_binary_string <- function(x) {
  stopifnot(
    is.integer(x),
    x <= 511L,
    x >= 0L
  )
  paste0(binaryLogic::as.binary(x), collapse = "")
}

devtools::load_all()
dir_path <- "tmp"
if (!dir.exists(dir_path)) {
  dir.create(dir_path)
} else {
  unlink(dir_path, recursive = TRUE, force = TRUE)
  dir.create(dir_path)
}
iarccrgtools::iarc_workdir_set(dir_path)

tool_name <- "multiple_primary"
iarc_df <- iarccrgtools::tool_colnameset_example_dataset(
  paste0("mandatory_", tool_name), n.rows = 1e4L
)
if ("subject_id" %in% names(iarc_df)) {
  iarc_df[["subject_id"]][1:3] <- 1L
}
if ("record_order" %in% names(iarc_df)) {
  iarc_df[["record_order"]][1:3] <- 1:3
}

auto <- iarccrgtools:::interface_with_tool(iarc_df, tool.name = tool_name,
                                           clean = FALSE, how = "automatically")
unlink(dir_path, recursive = TRUE, force = TRUE)
dir.create(dir_path)

manu <- iarccrgtools:::interface_with_tool(iarc_df, tool.name = tool_name,
                                          clean = FALSE, how = "interactively")
print(all.equal(auto, manu))
unlink(dir_path, recursive = TRUE, force = TRUE)
