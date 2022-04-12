
tool_exe_names <- function() {
  dt <- get_internal_dataset("tools")
  nms <- dt[["executable_name"]]
  names(nms) <- dt[["clean_name"]]
  nms
}


tool_exe_path <- function(tool.name) {
  assert_tool(tool.name)
  exe_dir_path <- filesystem_path_normalise(iarccrgtools::get_tool_exe_dir_path())
  exe_nm <- tool_exe_names()[tool.name]
  exe_path <- filesystem_path_normalise(paste0(exe_dir_path, exe_nm))
  if (!file.exists(exe_path)) {
    raise_internal_error(
      "Path to executable ", deparse(unname(exe_path)), " does not exist. "
    )
  }
  exe_path
}


tool_exe_call <- function(tool.name) {
  assert_is_character_nonNA_atom(tool.name)
  std_out_file_path <- tempfile(fileext = "std_out.txt")
  std_err_file_path <- tempfile(fileext = "std_err.txt")
  on.exit(file.remove(c(std_out_file_path, std_err_file_path)))
  tool_exe_path <- tool_exe_path(tool.name)
  status_code <- system2(tool_exe_path,
                         stdout = std_out_file_path,
                         stderr = std_err_file_path)
  std_err <- readLines(std_err_file_path)
  std_out <- readLines(std_out_file_path)
  if (!status_code %in% c(0L, 13L)) {
    warning("Calling ", deparse(tool_exe_path), " resulted in status code ",
            status_code, ", but 0 or 13 was expected. stderr was: ",
            deparse(std_err), ". stdout was: ", deparse(std_out))
  }
  return(mget(c("status_code", "std_err", "std_out")))
}




