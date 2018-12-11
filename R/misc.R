




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
#' @template tools_settings_for_automatic_use
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






















