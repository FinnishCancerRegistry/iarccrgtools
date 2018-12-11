




#' @title IARC CRG Tools Executable Path
#' @name exe_path
#' @description
#' Set and get path to IARC CRG Tools executable.
#' @details
#' The executable is attempted to be discovered automatically by
#' \code{\link{guess_tools_exe_path}} at start-up of this package, so with
#' any luck you don't need to set it by hand.
#'
#' Executable path will be stored for the duration of the ongoing R session.
#' If this package is somehow unloaded or reloaded, the setting will reset.
NULL

#' @describeIn exe_path sets path to .exe
#' @param path path to IARC CRG Tools executable
#' @export
set_tools_exe_path <- function(path) {
  assert_file_path(path)
  assign(x = "path", value = path, envir = exe_path_env)
  invisible(NULL)
}

#' @describeIn exe_path gets currently saved path to .exe;
#' value will be \code{NA_character_} if no path has been set yet.
#' @export
get_tools_exe_path <- function() {
  path <- exe_path_env$path
  if (is.na(path) || is.null(path)) {
    stop("Path to IARC CRG Tools executable not defined. ",
         "See ?set_tools_exe_path")
  } else if (!file.exists(path)) {
    stop("Path to IARC CRG Tools executable ill-defined; the specified ",
         "file does not exist in path ", deparse(path), ". Ensure the ",
         "path actually points to the executable.")
  }
  path
}

#' @describeIn exe_path tries to guess where the executable is based on
#' typical installation directories
#' @export
guess_tools_exe_path <- function() {
  dir_set <- c(
    "C:\\Program Files (x86)\\IARCcrgTools",
    "C:\\Program Files\\IARCcrgTools"
  )

  dir_exists <- dir.exists(dir_set)
  if (!any(dir_exists)) {
    stop("Could not guess path to IARC CRG Tools executable. You need to ",
         "set it by hand using set_tools_exe_path.")
  }
  dir <- dir_set[dir_exists][1]

  exe_nm <- dir(dir, pattern = "^IARCcrgTools\\.exe$",
                ignore.case = TRUE, full.names = FALSE)

  if (length(exe_nm) != 1) {
    stop("Could not guess path to IARC CRG Tools executable. You need to ",
         "set it by hand using set_tools_exe_path.")
  }

  exe_path <- paste0(dir, "\\", exe_nm)
  exe_path <- normalize_path(exe_path)
  exe_path
}

exe_path_env <- new.env(parent = emptyenv())
exe_path_env$path <- tryCatch(guess_tools_exe_path(),
                              error = function(e) e,
                              warning = function(w) w)







