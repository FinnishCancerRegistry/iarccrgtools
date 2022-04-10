
#' @title Tool-specific Metadata
#' @description
#' Tools to access tool-specific metadata. You will almost certainly not need
#' to use any of these functions.
#' @name tool_specific_metadata
NULL

#' @export
#' @rdname tool_specific_metadata
#' @template tool_name
#' @section Functions:
#' `[iarccrgtools::tool_menu_name]` gives the name of the menu in IARC CRG Tools
#' for the given `tool.name`.
#' @examples
#'
#' # iarccrgtools::tool_menu_name
#' stopifnot(
#'   iarccrgtools::tool_menu_name("check") == "Tools"
#' )
tool_menu_name <- function(tool.name) {
  dt <- get_internal_dataset("tools")
  stopifnot(
    length(tool.name) == 1L,
    is.character(tool.name),
    tool.name %in% dt[["clean_name"]]
  )

  menu_nms <- dt[["menu_name"]]
  names(menu_nms) <- dt[["clean_name"]]
  unname(menu_nms[tool.name])
}

#' @export
#' @rdname tool_specific_metadata
#' @template tool_name
#' @section Functions:
#' `[iarccrgtools::tool_menu_item_name]` gives the name of the item in the
#' appropriate menu in IARC CRG Tools for the given `tool.name`.
#' @examples
#'
#' # iarccrgtools::tool_menu_item_name
#' stopifnot(
#'   iarccrgtools::tool_menu_item_name("check") == "IARC/IACR Check"
#' )
tool_menu_item_name <- function(tool.name) {
  df <- get_internal_dataset("tools")
  if (!tool.name %in% df[["clean_name"]]) {
    raise_internal_error("clean_name = ", tool.name, " not in allowed clean ",
                         "names: ", deparse(df[["clean_name"]]))
  }
  df[["real_name"]][df[["clean_name"]] == tool.name]
}

#' @export
#' @rdname tool_specific_metadata
#' @template tool_name
#' @section Functions:
#' `[iarccrgtools::tool_names]` gives the names of all tools.
#' @examples
#'
#' # iarccrgtools::tool_names
#' stopifnot(
#'   c("check", "icdo3_to_icd10") %in% iarccrgtools::tool_names()
#' )
tool_names <- tool_clean_names <- function() {
  get_internal_dataset("tools")[["clean_name"]]
}

#' @export
#' @rdname tool_specific_metadata
#' @template tool_name
#' @section Functions:
#' `[iarccrgtools::tool_colnameset_name_to_tool_name]` gets the name of the tool
#' from the column name set name.
#' @examples
#'
#' # iarccrgtools::tool_colnameset_name_to_tool_name
#' stopifnot(
#'   iarccrgtools::tool_colnameset_name_to_tool_name("mandatory_check") ==
#'     "check"
#' )
#' @template colnameset_name
tool_colnameset_name_to_tool_name <- function(colnameset.name) {
  lapply(colnameset.name, assert_tools_colnameset_name)
  sub("(^all_)|(^optional_)|(^mandatory_)", "", colnameset.name)
}






#' @export
#' @rdname tool_specific_metadata
#' @template tool_name
#' @section Functions:
#' `[iarccrgtools::tool_colnameset_names]` returns all names of column name
#' sets.
#' @examples
#'
#' # iarccrgtools::tool_colnameset_names
#' stopifnot(
#'   "all_check" %in% iarccrgtools::tool_colnameset_names()
#' )
tool_colnameset_names <- function() {
  tool_nms <- tool_clean_names()
  tool_set_nms <- paste0(c("all_", "mandatory_", "optional_"),
                         rep(tool_nms, each = 3))
  c(tool_set_nms, "all")
}


#' @export
#' @rdname tool_specific_metadata
#' @template tool_name
#' @section Functions:
#' `[iarccrgtools::tool_colnameset]` returns all column names for the given
#' column name set name.
#' @examples
#'
#' # iarccrgtools::tool_colnameset
#' stopifnot(
#'   "sex" %in% iarccrgtools::tool_colnameset("all_check")
#' )
tool_colnameset <- function(
    colnameset.name
) {
  assert_tools_colnameset_name(colnameset.name)

  col_specs <- get_internal_dataset("column_specifications")
  col_specs_col_nm <- paste0("set_", colnameset.name)

  if (!col_specs_col_nm %in% names(col_specs)) {
    raise_internal_error(
      "Expected 'column_specifications' to have column with name ",
      deparse(col_specs_col_nm), " but it didn't."
    )
  }

  col_specs[["column_name"]][col_specs[[col_specs_col_nm]]]

}


#' @export
#' @rdname tool_specific_metadata
#' @template tool_name
#' @section Functions:
#' `[iarccrgtools::tool_example_dataset]` returns a `data.table`,
#' a fake example of what the data should look like for a given
#' `colnameset.name`.
#' @examples
#'
#' # iarccrgtools::tool_colnameset_example_dataset
#' ac <- iarccrgtools::tool_colnameset_example_dataset("all_check", 20L)
#' stopifnot(
#'   "sex" %in% names(ac),
#'   !duplicated(ac[["subject_id"]]),
#'   vapply(names(ac), function(ac_col_nm) {
#'     inherits(ac[[ac_col_nm]], iarccrgtools::tool_column_classes(ac_col_nm))
#'   }, logical(1L))
#' )
#' @param n.rows `[integer]` (default `10L`)
#'
#' How many times should the example row (only one is defined) be repeated?
#' Each resulting row will have its own `subject_id` and `record_id`.
tool_colnameset_example_dataset <- function(
    colnameset.name,
    n.rows = 10L
) {
  stopifnot(
    length(n.rows) == 1,
    n.rows %% 1 == 0,
    n.rows > 0
  )
  col_nms <- tool_colnameset(colnameset.name)
  df <- data.frame(
    subject_id = 1L,
    record_id = 1L,
    record_order = 1L,
    sex = 1L,
    icd9 = "5020",
    icd10 = "5020",
    icdo1_topography = "5020",
    icdo1_histology = "8522",
    icdo1_grade = 1L,
    icdo2_topography = "502",
    icdo2_histology = "8522",
    icdo3_topography = "502",
    icdo3_histology = "8522",
    icdo3_behavior = 1L,
    icdo3_grade = 1L,
    basis = 1L,
    bi_date = as.Date("1950-12-31"),
    dg_date = as.Date("2000-12-31"),
    dg_age = 50L,
    stringsAsFactors = FALSE
  )
  df <- df[rep(1L, n.rows), col_nms]
  df[["record_id"]] <- 1:nrow(df)
  df[["subject_id"]] <- 1:nrow(df)
  return(df[])
}


#' @export
#' @rdname tool_specific_metadata
#' @template tool_name
#' @section Functions:
#' `[iarccrgtools::tool_column_classes]` returns the appropriate class for each
#' supplied column name.
#' @examples
#'
#' # iarccrgtools::tool_column_classes
#' stopifnot(
#'   iarccrgtools::tool_column_classes("sex") == "integer"
#' )
#' @param col.nms `[character]` (no default)
#'
#' Names of columns.
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

#' @export
#' @rdname tool_specific_metadata
#' @template tool_name
#' @section Functions:
#' `[iarccrgtools::tool_column_fwf_widths]` returns the column width for each
#' given column name (width in the text file which IARC CRG Tools uses).
#' @examples
#'
#' # iarccrgtools::tool_column_fwf_widths
#' stopifnot(
#'   iarccrgtools::tool_column_fwf_widths("sex") == 1
#' )
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

#' @export
#' @rdname tool_specific_metadata
#' @template tool_name
#' @section Functions:
#' `[iarccrgtools::tool_column_explanation]` gives as short explanation for each
#' given column name.
#' @examples
#'
#' # iarccrgtools::tool_column_explanation
#' stopifnot(
#'   iarccrgtools::tool_column_explanation("dg_date") == "Date of diagnosis"
#' )
tool_column_explanation <- function(
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
  file_paths <- filesystem_path_normalise(file_paths)
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

  filesystem_path_normalise(paste0(dir, "\\", tool.name, "_input.txt"))
}









