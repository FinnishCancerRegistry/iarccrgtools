




ask_yes_no <- function(
  ...
) {
  stopifnot(
    vapply(list(...), is.character, logical(1))
  )

  cat(paste0(..., collapse = ""),
      "1: yes",
      "2: no",
      "c: cancel",
      sep = "\n")

  allowed_answers <- c("1", "2", "c")
  answer <- ""
  while (!answer %in% allowed_answers) {
    answer <- readline(prompt = ": ")
    answer <- tolower(answer)
  }

  switch(answer,
         "1" = TRUE,
         "2" = FALSE,
         c = stop("Cancelled.", call. = FALSE))
}





ask_to_proceed <- function(
  query
) {
  if (!ask_yes_no(query)) {
    stop("Cancelled.", call. = FALSE)
  }
}




raise_internal_error <- function(
  ...
) {
  requireNamespace("utils")
  error_msg <- paste0(
    "Internal error: ",
    ...,
    " If you see ",
    "this, please complain to the package maintainer ",
    utils::maintainer("iarccrgtools"), "."
  )
  stop(error_msg, call. = FALSE)

}




#' @title IARC CRG Tools Settings
#' @name tools_settings_files
#' @description
#' IARC CRG Tools can use settings defined on an earlier run of one of its
#' programs. This page documents the relationship of these settings files
#' with this R package.
#'
#' @section Manual use:
#'
#' This mainly pertains to using \code{\link{use_tools_interactively}}.
#' When using IARC CRG Tools manually, settings files are not mandatory.
#' They just make your life easier. Inspect the help pages of IARC CRG Tools
#' for more information.
#'
#' @section Automatic use:
#'
#' This pertains to using \code{\link{use_tools_automatically}}.
#' You need to have a pre-defined settings file for the IARC CRG Tools program
#' you want to use before using \code{\link{use_tools_automatically}}.
#'
#' Each program must have its own settings file. The file extension will be
#' either .dfi or .frm. The name of the file must be the name of the program
#' (one of the items given by \code{\link{tools_program_names}}). E.g.
#' \code{"iarc_check.dfi"}. The settings files must be stored in the
#' working directory set by \code{\link{set_tools_working_dir}}.
#'
#' This R package has pre-defined "sensible defaults" for certain programs,
#' which you can fetch into a specific folder using
#' \code{\link{get_tools_settings_template}}.
#'

NULL





dir_of_path <- function(path) {
  ## - if is existing dir, return path
  ## - if is existing (non-directory) file path, return dir of this file
  ## - if looks like file path, return implied dir
  ## - else return path and throw warning
  stopifnot(
    length(path) == 1,
    is.character(path)
  )
  has_ext <- grepl(
    pattern = "\\.[[:alpha:]]{1,10}^",
    x = path
  )
  if (dir.exists(path)) {
    return(path)
  } else if (file.exists(path) || has_ext) {
    return(dirname(path))
  }
  warning("Could not guess directory for path ", deparse(path), "; ",
          "you may encounter a strange error. If path is a file, ensure it ",
          "ends with an extension such as .txt. If path is a directory, ",
          "ensure that it exists.")
  
}





normalize_path <- function(path) {
  
  normalizePath(path = path, winslash = "\\", mustWork = FALSE)
  
}





str_extract_all <- function(x, pattern, ...) {
  m <- gregexpr(pattern = pattern, text = x, ...)
  regmatches(x = x, m = m)
}
str_extract <- function(x, pattern, ...) {
  m <- regexpr(pattern = pattern, text = x, ...)
  regmatches(x = x, m = m)
}




file_ext <- function(file) {
  str_extract(file, pattern = "(?<=\\.)\\w{1,}$", perl = TRUE)
}





#' @title iarccrgtools: Using IARC CRG Tools via R
#' @name iarccrgtools
#' @docType package
#' @description 
#' Contains functions to make using IARC CRG Tools easier with R,
#' including writing and reading IARC CRG Tools input/output files and
#' guided or automatic use of IARC CRG Tools itself.
#' @details
#' 
#' Main attractions include
#' \code{\link{use_tools_automatically}} and 
#' \code{\link{use_tools_interactively}} for calling IARC CRG Tools directly
#' from R and for helping with input/output from and to R.
#' 
NULL




get_program_definition_data <- function(
  data.nm
) {
  stopifnot(
    length(data.nm) == 1,
    is.character(data.nm),
    data.nm %in% c("program_guides", "program_output_files", 
                   "column_specifications"),
    data.nm %in% ls(as.environment("package:iarccrgtools"))
  )
  get(data.nm, pos = "package:iarccrgtools")
  
}





#' @md
#' @title Program Definitions
#' @description data.frame of commands for assisted or 
#' automated use of IARC CRG Tools.
#' @format 
#' A data.frame with these character string columns:
#' - `program_name`: name of the program
#' - `command`: computer-readable specification of keystrokes and certain special
#'   commands (see below) that are executed in the given order to run an 
#'   IARC CRG Tools program from start to finish; mainly relevant for
#'   \code{\link{use_tools_automatically}}
#' - `instruction`: plain English explanation of each step; these are shown
#'   when you call \code{\link{use_tools_interactively}}, so they should
#'   tell you quite clearly what you need to do.
#' @family program_definition_data
"program_guides"

#' @md
#' @title Program Definitions
#' @description data.frame specifying output files for each
#' IARC CRG Tools program.
#' @format 
#' A data.frame with these columns:
#' - `program_name`: character string column; name of the program
#' - `file_name_suffix`: character string column; suffix pasted to each output 
#'   file name; in other words the output files are assumed to have these 
#'   suffixes
#' - `is_table`: logical column; `TRUE` if the output file is a table, 
#'   `FALSE` if it is non-tabular text (such as a log file)
#' @family program_definition_data
"program_output_files"

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
#' is one of the items returned by \code{\link{tools_program_colnameset_names}}.
#' Each such logical columns is `TRUE` when that column indicated in 
#' `column_name` is included in that set of column names.
#' 
#' @family program_definition_data
"column_specifications"














