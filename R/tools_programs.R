







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



#' @export
#' @title IARC CRG Tools Settings Templates
#' @description Copy a settings file template
#' (see \code{\link{tools_settings_files}})
#' into a local directory. The settings may not be fully what they should
#' be for your use-case.
#' @template dir_path
#' @template tool_name
#'
#' @details
#'
#' If a file with the same name as the one copied by this function already
#' exists in the directory, this function will ask whether to overwrite it
#' or not.
#'
get_tools_settings_template <- function(
  dir.path = get_tool_work_dir(tool.name),
  tool.name
  ) {
  assert_dir_path(dir.path)
  dir.path <- normalize_path(paste0(dir.path, "/"))
  assert_tool(tool.name)

  file_paths_in_pkg <- paste0(tool.name, ".", c("dfi", "frm"))

  src_file_paths <- lapply(file_paths_in_pkg, function(file_path_in_pkg) {
    src_file_path <- system.file(
      file_path_in_pkg,
      package = "iarccrgtools"
    )
    if (src_file_path == "") {
      src_file_path <- NULL
    }
    src_file_path
  })
  names(src_file_paths) <- file_paths_in_pkg
  src_file_paths <- unlist(src_file_paths)

  file_names <- names(src_file_paths)
  tgt_file_paths <- paste0(dir.path, file_names)

  names(src_file_paths) <- names(tgt_file_paths) <- file_names

  lapply(file_names, function(file_name) {
    tgt <- tgt_file_paths[file_name]
    if (file.exists(tgt)) {
      ow <- ask_yes_no(
        "Target file ", deparse(tgt), " already exists. Overwrite?"
      )
      if (!ow) {
        return(NULL)
      }
    }
    file.copy(from = src_file_paths[file_name],
              to = tgt_file_paths[file_name],
              overwrite = TRUE)
    NULL
  })

  invisible(NULL)
}





tool_output_file_paths <- function(
  dir = get_tool_work_dir(tool.name),
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
  dir = get_tool_work_dir(tool.name),
  tool.name
) {
  assert_tool(tool.name)
  assert_dir_path(dir)

  paste0(dir, "\\", tool.name, "_input.txt")

}

tool_cache_sha_file_path <- function(
  dir = get_tool_work_dir(tool.name),
  tool.name
) {
  assert_tool(tool.name)
  assert_dir_path(dir)
  paste0(dir, "\\", tool.name, "input_cache_sha.txt")
}




tool_window_name <- function(
  tool.name
) {
  assert_tool(tool.name)
  pwn <- get_internal_dataset("tool_window_names")
  pwn[pwn$tool_name == tool.name, "tool_window_name"]
}












