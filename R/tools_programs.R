







#' @title Column Names
#' @name column_names
#' @description
#' Utilities for handling sets of required column names for each
#' IARC CRG Tools tool.
NULL

#' @describeIn column_names each column name set has its own name. This function
#' returns all the names of the sets.
#' @export
tool_colnameset_names <- function() {
  tool_nms <- tool_clean_names()
  tool_set_nms <- paste0(c("all_", "mandatory_", "optional_"),
                         rep(tool_nms, each = 3))
  c(tool_set_nms, "all")
}

#' @describeIn column_names returns a set of column name given the name of the
#' set itself.
#' @param set.nm string; name of a column set; only those returned by
#' \code{tool_colnameset_names} are allowed
#' @export
tool_colnameset <- function(
  set.nm
  ) {
  assert_tools_colnameset_name(set.nm)

  col_specs <- get_internal_dataset("column_specifications")
  col_specs_col_nm <- paste0("set_", set.nm)

  if (!col_specs_col_nm %in% names(col_specs)) {
    raise_internal_error(
      "Expected 'column_specifications' to have column with name ",
      deparse(col_specs_col_nm), " but it didn't."
    )
  }

  col_specs[["column_name"]][col_specs[[col_specs_col_nm]]]

}

#' @describeIn column_names returns the required class for each column by
#' column name
#' @param col.nms character string vector; names of columns; must be subset of
#' items returned by \code{tool_colnameset("all")}
#' @export
tool_column_classes <- function(
  col.nms
) {
  stopifnot(
    is.character(col.nms)
  )
  col_specs <- get_internal_dataset("column_specifications")

  cn <- col_specs[["column_name"]]
  cc <- col_specs[["class"]]

  cc[match(col.nms, cn, nomatch = NA_integer_)]
}

#' @describeIn column_names returns an integer vector of default column widths
#' for fixed-width format
#' @export
tool_column_fwf_widths <- function(
  col.nms
) {
  stopifnot(
    is.character(col.nms)
  )
  col_specs <- get_internal_dataset("column_specifications")

  cn <- col_specs[["column_name"]]
  wi <- as.integer(col_specs[["fwf_width"]])


  wi[match(col.nms, cn, nomatch = NA_integer_)]
}

#' @describeIn column_names returns short plain English explanation of column
#' @export
tool_column_infos <- function(
  col.nms
) {
  stopifnot(
    is.character(col.nms)
  )
  col_specs <- get_internal_dataset("column_specifications")

  cn <- col_specs[["column_name"]]
  ci <- col_specs[["info"]]


  ci[match(col.nms, cn, nomatch = NA_integer_)]

}








tool_output_file_paths <- function(
  dir,
  tool.name
) {
  assert_tool(tool.name)
  assert_dir_path(dir)

  tool_files_df <- get_internal_dataset("tool_output_files")
  is_in_tool <- tool_files_df$clean_name == tool.name
  tool_file_suffixes <- tool_files_df$file_name_suffix[is_in_tool]
  tool_file_is_table <- tool_files_df$is_table[is_in_tool]

  file_paths <- paste0(dir, "\\", tool.name, tool_file_suffixes)
  file_paths <- normalize_path(file_paths)
  names(file_paths) <- rep("is_not_table", length(file_paths))
  names(file_paths)[tool_file_is_table] <- rep("is_table",
                                               sum(tool_file_is_table))
  file_paths
}





tool_input_file_path <- function(
  dir,
  tool.name
) {
  assert_tool(tool.name)
  assert_dir_path(dir)

  normalize_path(paste0(dir, "\\", tool.name, "_input.txt"))
}

tool_cache_sha_file_path <- function(
  dir,
  tool.name
) {
  assert_tool(tool.name)
  assert_dir_path(dir)
  paste0(dir, "\\", tool.name, "_input_cache_sha.txt")
}




tool_window_name <- function(
  tool.name
) {
  assert_tool(tool.name)
  pwn <- get_internal_dataset("tool_window_names")
  pwn[pwn$tool_name == tool.name, "tool_window_name"]
}












