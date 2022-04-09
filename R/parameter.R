




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


parameter_file_write <- function(text) {
  writeLines(text = text, con = parameter_file_path())
}


parameter_file_contents <- function(

) {
  stop("TODO")
}



