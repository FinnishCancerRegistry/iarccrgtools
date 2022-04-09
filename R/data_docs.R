

#' @title IARC CRG Tools Settings
#' @name tools_settings_files
#' @description
#' IARC CRG Tools can use settings defined on an earlier run of one of its
#' tools. This page documents the relationship of these settings files
#' with this R package.
#'
#' @section Manual use:
#'
#' This mainly pertains to using \code{\link{interact_with_tool}}.
#' When using IARC CRG Tools manually, settings files are not mandatory.
#' They just make your life easier. Inspect the help pages of IARC CRG Tools
#' for more information.
#'
#' @section Automatic use:
#'
#' This pertains to using \code{automate_tool}.
#' You need to have a pre-defined settings file for the IARC CRG Tools program
#' you want to use before using \code{automate_tool}.
#'
#' Each tool must have its own settings file. The file extension will be
#' either .dfi or .frm. The name of the file must be the name of the program.
#' E.g.
#' \code{"iarc_check.dfi"}. The settings files must be stored in the
#' working directory set by \code{\link{set_tools_work_dir}}.
#'
#' This R package has pre-defined "sensible defaults" for certain tools,
#' which you can fetch into a specific folder using
#' \code{\link{tool_settings_copy}}.
#'

NULL







#' @md
#' @title Program Definitions
#' @description
#' Program definitions dataset.
#' @format
#' A data.frame with these character string columns:
#' - `clean_name`: name of tool as understood by the functions in this
#'   package
#' - `real_name`: name of tool in IARC CRG Tools menu
#' - `executable_name`: name of executable for corresponding tool
#' - `window_name`: name of executable's window in Windows
#' @family tool_definition_data
"tools"

#' @md
#' @title Program Definitions
#' @description data.frame specifying output files for each
#' IARC CRG Tools tool.
#' @format
#' A data.frame with these columns:
#' - `tool_name`: character string column; name of the program
#' - `file_name_suffix`: character string column; suffix pasted to each output
#'   file name; in other words the output files are assumed to have these
#'   suffixes
#' - `is_table`: logical column; `TRUE` if the output file is a table,
#'   `FALSE` if it is non-tabular text (such as a log file)
#' @family tool_definition_data
"tool_output_files"

#' @md
#' @title Program Definitions
#' @description data.frame of specifications for columns used in various
#' functions
#' @format
#' A data.frame with character string columns
#' - `column_name` name of column
#' - `class` expected class of column when using functions in this package
#' - `info` short plain English explanation of column purpose and contents
#'
#' as well as a number of logical (TRUE/FALSE) columns. The names of these
#' logical columns follow the convention `"set_SETNAME"` where `SETNAME`
#' is one of the items returned by \code{\link{tool_colnameset_names}}.
#' Each such logical columns is `TRUE` when that column indicated in
#' `column_name` is included in that set of column names.
#'
#' @family tool_definition_data
"column_specifications"
