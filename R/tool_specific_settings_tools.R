

#' @title IARC CRG Tools Settings
#' @description
#' Tools to handle settings files included in this package.
#' You almost certainly do not need to use any of this functions yourself.
#' Instead, other functions in this package use them.
#' @name tool_settings

#' @export
#' @rdname tool_settings
#' @section Functions:
#' `[iarccrgtools::tool_settings_source_dir_path]` returns a string, the
#' location of the settings files inside this R package.
#' @examples
#'
#' # iarccrgtools::tool_settings_source_dir_path
#' stopifnot(
#'   dir.exists(iarccrgtools::tool_settings_source_dir_path())
#' )
#'
tool_settings_source_dir_path <- function() {
  dir_path <- paste0(
    system.file(package = "iarccrgtools"),
    "/tool_settings/"
  )
  if (!dir.exists(dir_path)) {
    stop("Internal error: could not find dir containing tool settings. ",
         "Complain to the package maintainter if you see this. ",
         "This is what system.file gave: ", deparse(dir_path))
  }
  return(filesystem_path_normalise(dir_path))
}

#' @export
#' @rdname tool_settings
#' @section Functions:
#' `[iarccrgtools::tool_settings_availability]` returns a `data.table`
#' with columns `file_name`, `file_path`, `colnameset_name`,
#' `tool_name`. This dataset describes for which column name sets a settings
#' file is available in this R package.
#' @examples
#'
#' # iarccrgtools::tool_settings_availability
#' stopifnot(
#'   is.data.frame(iarccrgtools::tool_settings_availability())
#' )
#'
tool_settings_availability <- function() {
  src_dir_path <- tool_settings_source_dir_path()
  file_names <- dir(src_dir_path)
  file_paths <- filesystem_path_normalise(paste0(src_dir_path, "\\", file_names))
  tool_colnameset_names <- sub("[.].+$", "", file_names)
  tool_names <- tool_colnameset_name_to_tool_name(tool_colnameset_names)
  data.table::data.table(
    file_name = file_names,
    file_path = file_paths,
    colnameset_name = tool_colnameset_names,
    tool_name = tool_names
  )
}

#' @export
#' @rdname tool_settings
#' @template colnameset_name
#' @section Functions:
#' `[iarccrgtools::tool_settings_are_available]` returns a `TRUE`
#' if a settings file is available for `colnameset.name` within this R package.
#' @examples
#'
#' # iarccrgtools::tool_settings_are_available
#' stopifnot(
#'   iarccrgtools::tool_settings_are_available("mandatory_check"),
#'   iarccrgtools::tool_settings_are_available("all_multiple_primary"),
#'   iarccrgtools::tool_settings_are_available("mandatory_icdo3_to_icd10")
#' )
#'
tool_settings_are_available <- function(colnameset.name) {
  tsa <- tool_settings_availability()
  colnameset.name %in% tsa[["colnameset_name"]]
}


#' @export
#' @rdname tool_settings
#' @template dir_path
#' @template colnameset_name
#' @section Functions:
#' `[iarccrgtools::tool_settings_copy]` copies the settings file(s) from within
#' this R package to `tgt.dir.path`.
#' @param tgt.dir.path `[character]` (no default)
#'
#' Path to an existing directory.
#' @param verbose `[logical]` (default `TRUE`)
#'
#' If `TRUE`, a message is emitted concerning what was copied where, if
#' anything.
tool_settings_copy <- function(
  tgt.dir.path,
  colnameset.name,
  verbose = TRUE
) {
  assert_dir_path(tgt.dir.path)
  tgt.dir.path <- filesystem_path_normalise(paste0(tgt.dir.path, "\\"))
  assert_tools_colnameset_name(colnameset.name)

  tsa <- tool_settings_availability()
  tsa <- tsa[tsa[["colnameset_name"]] == colnameset.name, ]
  lapply(seq_along(tsa[[1]]), function(i) {
    src_file_path <- tsa[["file_path"]][i]
    tool_name <- tsa[["tool_name"]][i]
    tgt_file_path <- filesystem_path_normalise(paste0(
      tgt.dir.path, "\\", tool_name, "_input.", filesystem_file_path_extension(src_file_path)
    ))
    if (file.exists(tgt_file_path)) {
      if (verbose) {
        message("* iarccrgtools::tools_settings_copy: settings file ",
                deparse(tgt_file_path), " already existed, did not copy ",
                "settings file from R package")
      }
      return(NULL)
    }
    file.copy(from = src_file_path,
              to = tgt_file_path,
              overwrite = TRUE)
    if (verbose) {
      message("* iarccrgtools::tools_settings_copy: copied ",
              deparse(src_file_path), " to ",
              deparse(tgt_file_path))
    }
    NULL
  })

  invisible(NULL)
}
