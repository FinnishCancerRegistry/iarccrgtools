#' @name work_dir
#' @title IARC CRG Tools Working Directory
#' @description
#' Get and set directory where input and output files of IARC CRG Tools
#' are stored.
#' @details
#' The working directory is where files for IARC CRG Tools and created by
#' IARC CRG Tools should live. This is not the same directory where the
#' executable for IARC CRG Tools is. Instead, the working directory is
#' recommended to be created by you manually in advance. You may also use a
#' temporary directory if you don't want to store any results
#' (see [base::tempdir]).
#'
#' The tools working directory set by `iarc_workdir_set` will itself be
#' populated by tool-specific directories, e.g. `"my_dir/check/"` will contain
#' results for the "check" tool.
NULL

#' @describeIn work_dir Set working directory, directory where IARC CRG Tools
#' input and output data will be written.
#' @param dir `[character]` (no default)
#' 
#' Path to an existing directory. This will be the working directory.
#' 
#' @export
iarc_workdir_set <- function(dir) {
  assert_dir_path(dir)
  dir <- filesystem_path_normalise(dir)
  assign(x = "path", value = dir, envir = wd_env)
}
#' @describeIn work_dir Deprecated.
#' @export
set_tools_work_dir <- function(dir) {
  # @codedoc_comment_block news("iarccrgtools::set_tools_work_dir", "2023-03-28", "0.3.0")
  # `iarccrgtools::set_tools_work_dir` no longer usable ---
  # use `iarccrgtools::iarc_workdir_set``.
  # @codedoc_comment_block news("iarccrgtools::set_tools_work_dir", "2023-03-28", "0.3.0")
  stop("set_tools_work_dir no longer usable --- use iarc_workdir_set")
}

#' @describeIn work_dir Get currently set working directory for IARC CRG Tools.
#' @export
iarc_workdir_get <- function() {
  if (identical(wd_env$path, FALSE)) {
    stop("Working directory for IARC CRG Tools not set --- ",
         "see ?iarc_workdir_set")
  } else if (!dir.exists(wd_env$path)) {
    stop("Supplied IARC CRG Tools root working directory does not exist: ",
         deparse(wd_env$path), "; see ?iarc_workdir_set")
  }
  wd_env$path
}
wd_env <- new.env(parent = emptyenv())
wd_env$path <- FALSE

#' @describeIn work_dir Deprecated.
#' @export
get_tools_work_dir <- function() {
  # @codedoc_comment_block news("iarccrgtools::get_tools_work_dir", "2023-03-28", "0.3.0")
  # `iarccrgtools::get_tools_work_dir` no longer usable ---
  # use `iarccrgtools::iarc_workdir_get``.
  # @codedoc_comment_block news("iarccrgtools::get_tools_work_dir", "2023-03-28", "0.3.0")
  stop("get_tools_work_dir no longer usable --- use iarc_workdir_get")
}

iarc_toolworkdir_get <- function(tool.name, hash) {
  assert_tool(tool.name = tool.name)
  stopifnot(
    is.character(hash),
    length(hash) == 1,
    !is.na(hash)
  )
  dir_path <- iarccrgtools::iarc_workdir_get()
  dir_path <- filesystem_path_normalise(paste0(dir_path, "\\", tool.name))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
  dir_path <- filesystem_path_normalise(
    paste0(dir_path, "/", "hash_", substring(hash, 1, 6))
  )
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
  return(dir_path)
}

#' @describeIn work_dir Deprecated.
#' @export
#' @template tool_name
#' @param hash `[character]` (no default)
#'
#' Hash of an input dataset; get working directory for `tool.name` and this
#' dataset.
get_tool_work_dir <- function(tool.name, hash) {
  # @codedoc_comment_block news("iarccrgtools::get_tool_work_dir", "2023-03-28", "0.3.0")
  # `iarccrgtools::get_tool_work_dir` no longer usable.
  # @codedoc_comment_block news("iarccrgtools::get_tool_work_dir", "2023-03-28", "0.3.0")
  stop("get_tool_work_dir no longer usable")
}
