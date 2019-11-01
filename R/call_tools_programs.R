

tools_program_names <- function() {
  c(
    "ICD-10+ICD-O-2 -> ICD-O-2",
    "ICD-10 -> ICD-O-2",
    "ICD-9+ICD-O-1 -> ICD-O-2",
    "ICD-9 -> ICD-O-2",
    "ICD-O-1(FTE) -> ICD-O-2",
    "IARC/IACR Check",
    "IARC/IACR Multiple Primary",
    "ICD-O-1 -> ICD-O-2",
    "ICD-O-2 -> ICD-10",
    "ICD-O-2 -> ICD-9",
    "ICD-O-2 -> ICD-O-3",
    "ICD-O-3 -> ICD-10"
  )
}

tools_program_executable_names <- function() {
  exe_nms <- c(
    "10O2toO2.EXE", "10toO2.EXE", "9O1toO2.EXE", "9toO2.EXE", "FTEtoO2.EXE",  
    "IARCcheck.EXE", "Multiple.EXE", 
    "O1toO2.EXE", "O2to10.EXE",  "O2to9.EXE", "O2toO3.EXE", "O3to10.EXE"
  )
  names(exe_nms) <- tools_program_names()
  exe_nms
}

call_tools_program <- function(program_name) {
  assert_is_character_nonNA_atom(program_name)
  stopifnot(
    program_name %in% tools_program_names()
  )
  
  exe_dir_path <- normalize_path(get_tools_exe_dir_path())
  exe_nm <- tools_program_executable_names()[program_name]
  exe_path <- normalize_path(paste0(exe_dir_path, "\\", exe_nm))
  stop("TODO: actually probably need to write .bat to cd to exe_dir_path ",
       "and then call the exe_nm")
  system2(exe_path)
}




