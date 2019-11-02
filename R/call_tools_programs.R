
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

call_program_executable <- function(program_name) {
  assert_is_character_nonNA_atom(program_name)
  stopifnot(
    program_name %in% program_names()
  )
  
  exe_dir_path <- normalize_path(get_tools_exe_dir_path())
  exe_nm <- program_executable_names()[program_name]
  exe_path <- normalize_path(paste0(exe_dir_path, "\\", exe_nm))
  stop("TODO: actually probably need to write .bat to cd to exe_dir_path ",
       "and then call the exe_nm")
  system2(exe_path)
}




