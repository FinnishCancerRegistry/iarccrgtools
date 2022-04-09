




parameter_file_path <- function() {
  if (is_writable(iarccrgtools::get_tools_install_dir_path())) {
    path <- paste0(iarccrgtools::get_tools_install_dir_path(),
                   "/pgm/parameter.dat")
  } else {
    path <- paste0(
      Sys.getenv("LOCALAPPDATA"),
      "/Program Files (x86)/IARCcrgTools/pgm/"
    )
    if (!dir.exists(path)) {
      stop("Could not determine location of parameter.dat --- ",
           deparse(path), " does not exist and ",
           deparse(iarccrgtools::get_tools_install_dir_path()),
           " is not writeable; ",
           "this is an internal error, if you see this you should complain to ",
           "the package maintainer; a work-around may be to run R as an admin")
    }
    path <- paste0(parameter.dat, "parameter.dat")
  }
  path <- normalize_path(path)
  path
}



read_parameter_file <- function() {
  path <- parameter_file_path()
  readLines(path)
}


write_parameter_file <- function(text) {
  writeLines(text = text, con = parameter_file_path())
}


create_parameter_contents <- function() {
  stop("TODO")
}



