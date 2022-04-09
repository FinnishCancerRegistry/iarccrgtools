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
iarccrgtools::set_tools_work_dir(dir_path)

tool_name <- "icdo3_to_icd10"
df1 <- iarccrgtools::create_example(paste0("mandatory_", tool_name), n.rows = 2L)
wat1 <- iarccrgtools::interface_with_tool(df1, tool.name = tool_name,
                                          clean = FALSE, how = "automatically")
unlink(dir_path, recursive = TRUE, force = TRUE)
unlink(parameter_file_path())
dir.create(dir_path)
# stop("halt")

df2 <- df1
# df2$subject_id <- 10L + df2$subject_id
wat2 <- iarccrgtools::interface_with_tool(df2, tool.name = tool_name,
                                          clean = FALSE, how = "interactively")
print(all.equal(wat1, wat2))
print(parameter_file_read()[3L])
unlink(dir_path, recursive = TRUE, force = TRUE)
unlink(parameter_file_path())
