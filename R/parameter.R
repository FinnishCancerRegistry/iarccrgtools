




parameter_file_path <- function() {
  dir_path <- iarccrgtools::get_tool_exe_dir_path()
  if (!dir.exists(dir_path) || !filesystem_dir_path_is_writable(dir_path)) {
    virtual_dir_path <- filesystem_path_normalise(paste0(
      Sys.getenv("LOCALAPPDATA"),
      "\\VirtualStore\\Program Files (x86)\\IARCcrgTools\\pgm\\"
    ))
    if (!dir.exists(virtual_dir_path)) {
      stop("Could not determine location of parameter.dat --- dir ",
           deparse(virtual_dir_path), " does not exist and ",
           deparse(dir_path),
           " is not writeable; ",
           "this is an internal error, if you see this you should complain to ",
           "the package maintainer; a work-around may be to run R as an admin")
    }
    dir_path <- virtual_dir_path
  }
  file_path <- filesystem_path_normalise(paste0(dir_path, "\\parameter.dat"))
  return(file_path)
}



parameter_file_read <- function() {
  path <- parameter_file_path()
  readLines(path)
}


parameter_file_write <- function(x, verbose = TRUE) {
  file_path <- parameter_file_path()
  if (verbose) {
    message(
      "* iarccrgtools::parameter_file_write: writing these contents to ",
      deparse(file_path), ":\n",
      paste0("  ", x, collapse = "\n")
    )
  }
  writeLines(text = x, con = file_path)
}

parameter_contents_are_available <- function(colnameset.name) {
  assert_tools_colnameset_name(colnameset.name)
  tpc <- get_internal_dataset("tool_parameter_contents")
  return(colnameset.name %in% tpc[["colnameset_name"]])
}

parameter_file_contents <- function(
  colnameset.name,
  tool.work.dir
) {
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



