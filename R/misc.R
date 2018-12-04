




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
























