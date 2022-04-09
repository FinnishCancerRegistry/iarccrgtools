
tool_executable_names <- function() {
  dt <- get_internal_dataset("tools")
  nms <- dt[["executable_name"]]
  names(nms) <- dt[["clean_name"]]
  nms
}

tool_clean_names <- function() {
  get_internal_dataset("tools")[["clean_name"]]
}

tool_real_names <- function() {
  df <- get_internal_dataset("tools")
  nms <- df[["real_name"]]
  names(nms) <- df[["clean_name"]]
  nms
}

tool_real_name_of_clean_name <- function(clean_name) {
  df <- get_internal_dataset("tools")
  if (!clean_name %in% df[["clean_name"]]) {
    raise_internal_error("clean_name = ", clean_name, " not in allowed clean ",
                         "names: ", deparse(df[["clean_name"]]))
  }
  df[["real_name"]][df[["clean_name"]] == clean_name]
}

tool_menu_name <- function(clean.name) {
  dt <- get_internal_dataset("tools")
  stopifnot(
    length(clean.name) == 1L,
    is.character(clean.name),
    clean.name %in% dt[["clean_name"]]
  )

  menu_nms <- dt[["menu_name"]]
  names(menu_nms) <- dt[["clean_name"]]
  unname(menu_nms[clean.name])
}

call_tool_executable <- function(tool_name) {
  assert_is_character_nonNA_atom(tool_name)
  stopifnot(
    tool_name %in% tool_clean_names()
  )

  exe_dir_path <- normalize_path(get_tools_exe_dir_path())
  exe_nm <- tool_executable_names()[tool_name]
  exe_path <- normalize_path(paste0(exe_dir_path, "\\pgm\\", exe_nm))
  system2(exe_path, stdout = TRUE, stderr = TRUE)
}




