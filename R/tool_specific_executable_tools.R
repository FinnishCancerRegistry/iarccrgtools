
tool_exe_names <- function() {
  dt <- get_internal_dataset("tools")
  nms <- dt[["executable_name"]]
  names(nms) <- dt[["clean_name"]]
  nms
}


tool_exe_path <- function(tool.name) {
  assert_tool(tool.name)
  exe_dir_path <- filesystem_path_normalise(get_tool_exe_dir_path())
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
  system2(tool_exe_path(tool.name), stdout = TRUE, stderr = TRUE)
}




