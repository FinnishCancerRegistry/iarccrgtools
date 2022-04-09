
tool_exe_names <- function() {
  dt <- get_internal_dataset("tools")
  nms <- dt[["executable_name"]]
  names(nms) <- dt[["clean_name"]]
  nms
}



tool_exe_call <- function(tool.name) {
  assert_is_character_nonNA_atom(tool.name)
  stopifnot(
    tool.name %in% tool_names()
  )

  exe_dir_path <- normalize_path(get_tools_exe_dir_path())
  exe_nm <- tool_exe_names()[tool.name]
  exe_path <- normalize_path(paste0(exe_dir_path, "\\pgm\\", exe_nm))
  system2(exe_path, stdout = TRUE, stderr = TRUE)
}




