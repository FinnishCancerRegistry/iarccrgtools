#' @title IARC CRG Tools Executable Path
#' @name exe_path
#' @description
#' Set and get path to IARC CRG Tools executable.
#' @details
#' The executable is attempted to be discovered automatically by
#' [iarccrgtools::guess_tools_exe_path] at start-up of this package, so with
#' any luck you don't need to set it by hand.
#'
#' Executable path will be stored for the duration of the ongoing R session.
#' If this package is somehow unloaded or reloaded, the setting will reset.
NULL

iarc_exe_file_name <- function() {
  "IARCcrgTools.EXE"
}

exe_path_env <- new.env(parent = emptyenv())

#' @describeIn exe_path Sets path to IARC CRG Tools main executable.
#' @param path `[character]` (no defaukt)
#' 
#' Path to IARC CRG Tools main executable.
#' @export
iarc_exe_path_set <- function(path) {
  assert_file_path(path)
  assign(x = "path", value = path, envir = exe_path_env)
  assign(x = "dir", value = filesystem_dir_of_path(path), envir = exe_path_env)
  invisible(NULL)
}
#' @describeIn exe_path Deprecated.
#' @export
set_tools_exe_path <- function(path) {
  # @codedoc_comment_block news("iarccrgtools::set_tools_exe_path", "2023-03-28", "0.3.0")
  # `iarccrgtools::set_tools_exe_path` no longer usable ---
  # use `iarccrgtools::iarc_exe_path_set``.
  # @codedoc_comment_block news("iarccrgtools::set_tools_exe_path", "2023-03-28", "0.3.0")
  stop("set_tools_exe_path no longer usable --- use iarc_exe_path_set.")
}

#' @describeIn exe_path Get currently set path to IARC CRG Tools main
#' executable.
#' @export
iarc_exe_path_get <- function() {
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
#' @describeIn exe_path Deprecated.
#' @export
get_tools_exe_path <- function() {
  # @codedoc_comment_block news("iarccrgtools::get_tools_exe_path", "2023-03-28", "0.3.0")
  # `iarccrgtools::get_tools_exe_path` no longer usable ---
  # use `iarccrgtools::iarc_exe_path_get``.
  # @codedoc_comment_block news("iarccrgtools::get_tools_exe_path", "2023-03-28", "0.3.0")
  stop("get_tools_exe_path no longer usable --- use iarc_exe_path_get.")
}

#' @describeIn exe_path Guesses location of IARC CRG Tools main executable
#' based on usual installation locations. This will work if you have
#' Installed IARC CRG Tools in either `C:/Program Files (x86)/IARCcrgTools"`
#' or `"C:/Program Files/IARCcrgTools"`.
#' @export
iarc_exe_path_guess <- function() {
   dir_set <- c(
    "C:\\Program Files (x86)\\IARCcrgTools",
    "C:\\Program Files\\IARCcrgTools"
  )
  dir_set <- filesystem_path_normalise(dir_set)
  dir_exists <- dir.exists(dir_set)
  if (!any(dir_exists)) {
    stop("Could not guess path to IARC CRG Tools executable. ",
         "Either IARC CRG Tools is not installed or you need to set this ",
         "by hand using iarc_exe_path_set.")
  }
  dir <- dir_set[dir_exists][1]
  exe_path <- paste0(dir, "/", iarc_exe_file_name())
  exe_path <- filesystem_path_normalise(exe_path)
  if (!file.exists(exe_path)) {
    stop("Could not guess path to IARC CRG Tools executable. ",
         "Either IARC CRG Tools is not installed or you need to set this ",
         "by hand using iarc_exe_path_set.")
  }
  return(exe_path)
}
#' @describeIn exe_path Deprecated.
#' @export
guess_tools_exe_path <- function() {
  # @codedoc_comment_block news("iarccrgtools::guess_tools_exe_path", "2023-03-28", "0.3.0")
  # `iarccrgtools::guess_tools_exe_path` no longer usable ---
  # use `iarccrgtools::iarc_exe_path_guess``.
  # @codedoc_comment_block news("iarccrgtools::guess_tools_exe_path", "2023-03-28", "0.3.0")
  stop("guess_tools_exe_path no longer usable --- use iarc_exe_path_guess")
}

#' @describeIn exe_path Deprecated.
#' @export
iarc_exe_dir_path_guess <- function() {
  # @codedoc_comment_block news("iarccrgtools::iarc_exe_dir_path_guess", "2023-03-28", "0.3.0")
  # `iarccrgtools::iarc_exe_dir_path_guess` no longer usable.
  # @codedoc_comment_block news("iarccrgtools::iarc_exe_dir_path_guess", "2023-03-28", "0.3.0")
  stop("iarc_exe_dir_path_guess no longer usable")
}
#' @describeIn exe_path Deprecated.
#' @export
guess_tools_exe_dir_path <- function() {
  # @codedoc_comment_block news("iarccrgtools::guess_tools_exe_dir_path", "2023-03-28", "0.3.0")
  # `iarccrgtools::guess_tools_exe_dir_path` no longer usable.
  # @codedoc_comment_block news("iarccrgtools::guess_tools_exe_dir_path", "2023-03-28", "0.3.0")
  stop("guess_tools_exe_dir_path no longer usable.")
}

iarc_installation_dir_path <- function() {
  dp <- filesystem_dir_of_path(iarc_exe_path_get())
  if (is.na(dp) || !dir.exists(dp)) {
    raise_internal_error(
      "No such directory: ", deparse(dp), "."
    )
  }
  dp
}

#' @describeIn exe_path Deprecated.
#' @export
get_tools_install_dir_path <- function() {
  # @codedoc_comment_block news("iarccrgtools::get_tools_install_dir_path", "2023-03-28", "0.3.0")
  # `iarccrgtools::get_tools_install_dir_path` no longer usable.
  # @codedoc_comment_block news("iarccrgtools::get_tools_install_dir_path", "2023-03-28", "0.3.0")
  stop("get_tools_install_dir_path no longer usable")
}

#' @describeIn exe_path Deprecated.
#' @export
get_tool_exe_dir_path <- function() {
  # @codedoc_comment_block news("iarccrgtools::get_tool_exe_dir_path", "2023-03-28", "0.3.0")
  # `iarccrgtools::get_tool_exe_dir_path` no longer usable.
  # @codedoc_comment_block news("iarccrgtools::get_tool_exe_dir_path", "2023-03-28", "0.3.0")
  stop("get_tool_exe_dir_path no longer usable")
}

exe_path_env_guess <- function() {
  exe_path_env$path <- tryCatch(iarc_exe_path_guess(),
                                error = function(e) NA_character_,
                                warning = function(w) NA_character_)
  invisible(NULL)
}
exe_path_env_guess()
