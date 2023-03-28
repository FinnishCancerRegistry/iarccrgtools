
iarc_toolexe_dir_path <- function() {
  dp <- filesystem_path_normalise(
    paste0(iarc_installation_dir_path(), "\\pgm\\")
  )
  if (is.na(dp) || !dir.exists(dp)) {
    raise_internal_error(
      "No such directory: ", deparse(dp), "."
    )
  }
  dp
}

iarc_toolexe_names <- function() {
  dt <- get_internal_dataset("tools")
  nms <- dt[["executable_name"]]
  names(nms) <- dt[["clean_name"]]
  nms
}

iarc_toolexe_path <- function(tool.name) {
  assert_tool(tool.name)
  exe_dir_path <- filesystem_path_normalise(iarc_toolexe_dir_path())
  exe_nm <- iarc_toolexe_names()[tool.name]
  exe_path <- filesystem_path_normalise(paste0(exe_dir_path, exe_nm))
  if (!file.exists(exe_path)) {
    raise_internal_error(
      "Path to executable ", deparse(unname(exe_path)), " does not exist. "
    )
  }
  exe_path
}

iarc_toolexe_call <- function(tool.name) {
  assert_is_character_nonNA_atom(tool.name)
  std_out_file_path <- tempfile(fileext = "std_out.txt")
  std_err_file_path <- tempfile(fileext = "std_err.txt")
  on.exit(file.remove(c(std_out_file_path, std_err_file_path)))
  iarc_toolexe_path <- iarc_toolexe_path(tool.name)
  status_code <- system2(iarc_toolexe_path,
                         stdout = std_out_file_path,
                         stderr = std_err_file_path)
  std_err <- readLines(std_err_file_path)
  std_out <- readLines(std_out_file_path)
  if (!status_code %in% c(0L, 13L)) {
    warning("Calling ", deparse(iarc_toolexe_path), " resulted in status code ",
            status_code, ", but 0 or 13 was expected. stderr was: ",
            deparse(std_err), ". stdout was: ", deparse(std_out))
  }
  return(mget(c("status_code", "std_err", "std_out")))
}
