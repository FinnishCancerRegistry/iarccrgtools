
program_executable_names <- function() {
  dt <- get_internal_dataset("programs")
  nms <- dt[["executable_name"]]
  names(nms) <- dt[["clean_name"]]
  nms
}

program_window_names <- function() {
  dt <- get_internal_dataset("programs")
  nms <- dt[["window_name"]]
  names(nms) <- dt[["clean_name"]]
  nms
}

program_clean_names <- function() {
  get_internal_dataset("programs")[["clean_name"]]
}

program_real_names <- function() {
  get_internal_dataset("programs")[["real_name"]]
}

program_real_name_of_clean_name <- function(clean_name) {
  df <- get_internal_dataset("programs")
  if (!clean_name %in% df[["clean_name"]]) {
    raise_internal_error("clean_name = ", clean_name, " not in allowed clean ",
                         "names: ", deparse(df[["clean_name"]]))
  }
  df[["real_name"]][df[["clean_name"]] == clean_name]
}

call_program_executable <- function(program_name) {
  assert_is_character_nonNA_atom(program_name)
  stopifnot(
    program_name %in% program_clean_names()
  )
  
  exe_dir_path <- normalize_path(get_tools_exe_dir_path())
  exe_nm <- program_executable_names()[program_name]
  exe_path <- normalize_path(paste0(exe_dir_path, "\\pgm\\", exe_nm))
  system2(exe_path, stdout = TRUE, stderr = TRUE)
}




