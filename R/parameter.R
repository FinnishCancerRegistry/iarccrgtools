

parameter_dir_path_virtual <- function() {
  filesystem_path_normalise(paste0(
    Sys.getenv("LOCALAPPDATA"),
    "\\VirtualStore\\Program Files (x86)\\IARCcrgTools\\pgm\\"
  ))
}

parameter_file_path <- function() {
  # @codedoc_comment_block iarccrgtools:::parameter_file_path
  # `iarccrgtools:::parameter_file_path` returns a string, the path to the
  # parameter file. The parameter file is assumed to live in either the
  # dir given by `[iarccrgtools::get_tool_exe_dir_path]`, if it is writable,
  # or in `${iarccrgtools:::parameter_dir_path_virtual()}` otherwise.
  # @codedoc_comment_block iarccrgtools:::parameter_file_path
  dir_path <- iarccrgtools::get_tool_exe_dir_path()
  if (!filesystem_dir_path_is_writable(dir_path)) {
    virtual_dir_path <- parameter_dir_path_virtual()
    if (!dir.exists(virtual_dir_path)) {
      result <- tryCatch(dir.create(virtual_dir_path, recursive = TRUE),
                         error = function(e) e)
      if (inherits(result, "error")) {
        stop(
          "Could not select location of parameter.dat --- dir ",
          deparse(virtual_dir_path), " did not exist and could not be created ",
          ", and ",
           deparse(dir_path),
          " is not writeable; see ?iarccrgtools::interact_with_tool"
        )
      }
    }
    dir_path <- virtual_dir_path
  }
  file_path <- filesystem_path_normalise(paste0(dir_path, "\\parameter.dat"))
  return(file_path)
}



parameter_file_read <- function() {
  # @codedoc_comment_block iarccrgtools:::parameter_file_read
  # `iarccrgtools:::parameter_file_read` reads the parameter file at the path
  # given by `parameter_file_path()` using `readLines` and returns the result.
  # @codedoc_comment_block iarccrgtools:::parameter_file_read
  path <- parameter_file_path()
  readLines(path)
}


parameter_file_write <- function(x, verbose = TRUE) {
  # @codedoc_comment_block iarccrgtools:::parameter_file_write
  # @param x `[character]` (no default)
  #
  # Text to write.
  #
  # @param verbose `[logical]` (default `TRUE`)
  #
  # If `TRUE`, emits a message explaining where the parameter file was written
  # to.
  #
  # @section Functions:
  # `iarccrgtools:::parameter_file_read` writes `x` into the path given by
  # `parameter_file_path()`.
  # @return
  # `iarccrgtools:::parameter_file_read` always returns `NULL` invisibly.
  # @codedoc_comment_block iarccrgtools:::parameter_file_write
  file_path <- parameter_file_path()
  if (verbose) {
    message(
      "* iarccrgtools::parameter_file_write: writing these contents to ",
      deparse(file_path), ":\n",
      paste0("  ", x, collapse = "\n")
    )
  }
  writeLines(text = x, con = file_path)
  invisible(NULL)
}

parameter_contents_are_available <- function(colnameset.name) {
  # @codedoc_comment_block iarccrgtools:::parameter_contents_are_available
  # @template colanameset_name
  #
  # @section Functions:
  # `iarccrgtools:::parameter_contents_are_available` returns `TRUE` if
  # pre-defined parameter contents are available within this package for
  # the given `colnameset.name`, and `FALSE` otherwise.
  # @return
  # `iarccrgtools:::parameter_contents_are_available` returns `TRUE` if
  # pre-defined parameter contents are available within this package for
  # the given `colnameset.name`, and `FALSE` otherwise.
  # @codedoc_comment_block iarccrgtools:::parameter_contents_are_available
  assert_tools_colnameset_name(colnameset.name)
  tpc <- get_internal_dataset("tool_parameter_contents")
  return(colnameset.name %in% tpc[["colnameset_name"]])
}

parameter_file_contents <- function(
  colnameset.name,
  tool.work.dir
) {
  # @codedoc_comment_block iarccrgtools:::parameter_file_contents
  # @template colanameset_name
  # @param tool.work.dir `[character]` (no default)
  #
  # Directory for input and output files for IARC CRG Tools, for this
  # particular tool / dataset.
  #
  # @section Functions:
  # `iarccrgtools:::parameter_file_contents` returns a string vector, the
  # contents of the parameter file for the given `colnameset.name`
  # and `tool.work.dir`.
  # @return
  # `iarccrgtools:::parameter_file_contents` returns a string vector, the
  # contents of the parameter file for the given `colnameset.name`
  # and `tool.work.dir`.
  # @codedoc_comment_block iarccrgtools:::parameter_file_contents
  assert_tools_colnameset_name(colnameset.name)
  assert_dir_path(tool.work.dir)
  tpc <- get_internal_dataset("tool_parameter_contents")
  if (!colnameset.name %in% tpc[["colnameset_name"]]) {
    stop("Parameter file contents not implemented for ",
         deparse(colnameset.name), "; column name sets for which this is ",
         "implemented: ", paste0(tpc[["colnameset_name"]], collapse = ", "))
  }
  tool_name <- iarccrgtools::tool_colnameset_name_to_tool_name(colnameset.name)
  input_file_path <- tool_input_file_path(
    dir = tool.work.dir,
    tool.name = tool_name
  )
  output_file_paths <- tool_output_file_paths(
    dir = tool.work.dir,
    tool.name = tool_name
  )
  output_file_path <- output_file_paths[grepl("output.txt$", output_file_paths)]
  parameter_line <- tpc[["parameter_line"]][
    tpc[["colnameset_name"]] == colnameset.name
  ]
  return(c(input_file_path, output_file_path, parameter_line))
}



