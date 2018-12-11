




#' @title IARC CRG Tools Program Names
#' @description
#' This function simply returns a haracter string vector of IARC CRG Tools
#' program names supported by this R package.
#' @return A character string vector.
#' @export
tools_program_names <- function() {
  sort(unique(program_guides$program_name))
}





tools_program_commands <- function(program.name) {
  assert_tools_program(program.name)
  is_in_program <- program_guides$program_name == program.name
  ks <- program_guides[["command"]][is_in_program]
  names(ks) <- program_guides[["instruction"]][is_in_program]
  ks
}

tools_program_instructions <- function(program.name) {
  assert_tools_program(program.name)
  
  is_in_program <- program_guides$program_name == program.name
  program_guides[["instruction"]][is_in_program]

}




tools_program_colnameset_names <- function() {
  prog_nms <- tools_program_names()
  prog_set_nms <- paste0(c("all_", "mandatory_", "optional_"),
                         rep(prog_nms, each = 3))
  c(prog_set_nms, "all")
}
tools_program_colnameset <- function(set.nm) {
  assert_tools_colnameset_name(set.nm)

  icdo3_thb <- c("icdo3_topography", "icdo3_histology", "icdo3_behaviour")
  icdo3_all <- c(icdo3_thb, "icdo3_grade")

  mandatory_icdo3_to_icd10 <- c(
    "record_id", "sex",
    icdo3_thb
  )
  optional_icdo3_to_icd10 <- "icdo3_grade"
  all_icdo3_to_icd10 <- c(mandatory_icdo3_to_icd10, optional_icdo3_to_icd10)


  mandatory_iarc_check <- c(
    "record_id", "sex",
    icdo3_thb,
    "basis", "dg_date", "bi_date", "dg_age"
  )
  optional_iarc_check <- c(
    "icdo3_grade"
  )
  all_iarc_check <- c(mandatory_iarc_check, optional_iarc_check)


  mandatory_iarc_multiple_primary <- c(
    "subject_id", "multi_no", "sex", icdo3_thb, "dg_date"
  )
  optional_iarc_multiple_primary <- character(0)
  all_iarc_multiple_primary <- c(mandatory_iarc_multiple_primary,
                                 optional_iarc_multiple_primary)



  all <- unique(unlist(mget(setdiff(ls(), "set.nm"))))

  if (!set.nm %in% ls()) {
    raise_internal_error(
      "No set of column names defined for set.nm = ",
      deparse(set.nm), " although it is allowed by ",
      "tools_program_colnameset_names."
    )
  }



  get(set.nm, inherits = FALSE)
}




#' @export
#' @title IARC CRG Tools Settings Templates
#' @description Copy a settings file template
#' (see \code{\link{tools_settings_files}})
#' into a local directory. The settings may not be fully what they should
#' be for your use-case.
#' @template dir_path
#' @template program_name
#'
#' @details
#'
#' If a file with the same name as the one copied by this function already
#' exists in the directory, this function will ask whether to overwrite it
#' or not.
#'
get_tools_settings_template <- function(
  dir.path = get_tools_working_dir(),
  program.name
  ) {
  assert_dir_path(dir.path)
  dir.path <- normalize_path(paste0(dir.path, "/"))
  assert_tools_program(program.name)

  file_paths_in_pkg <- paste0(program.name, ".", c("dfi", "frm"))

  src_file_paths <- lapply(file_paths_in_pkg, function(file_path_in_pkg) {
    src_file_path <- system.file(
      file_path_in_pkg,
      package = "iarccrgtools"
    )
    if (src_file_path == "") {
      src_file_path <- NULL
    }
    src_file_path
  })
  names(src_file_paths) <- file_paths_in_pkg
  src_file_paths <- unlist(src_file_paths)

  file_names <- names(src_file_paths)
  tgt_file_paths <- paste0(dir.path, file_names)

  names(src_file_paths) <- names(tgt_file_paths) <- file_names

  lapply(file_names, function(file_name) {
    tgt <- tgt_file_paths[file_name]
    if (file.exists(tgt)) {
      ow <- ask_yes_no(
        "Target file ", deparse(tgt), " already exists. Overwrite?"
      )
      if (!ow) {
        return(NULL)
      }
    }
    file.copy(from = src_file_paths[file_name],
              to = tgt_file_paths[file_name],
              overwrite = TRUE)
    NULL
  })

  invisible(NULL)
}





tools_program_output_file_paths <- function(
  dir = get_tools_working_dir(),
  program.name
) {
  assert_tools_program(program.name)
  assert_dir_path(dir)
  
  prog_files_df <- program_output_files
  is_in_program <- prog_files_df$program_name == program.name
  prog_file_suffixes <- prog_files_df$file_name_suffix[is_in_program]
  prog_file_is_table <- prog_files_df$is_table[is_in_program]
  
  file_paths <- paste0(dir, "\\", program.name, prog_file_suffixes)
  names(file_paths) <- rep("is_not_table", length(file_paths))
  names(file_paths)[prog_file_is_table] <- rep("is_table", 
                                               sum(prog_file_is_table))
  file_paths
}





tools_program_input_file_path <- function(
  dir = get_tools_working_dir(),
  program.name
) {
  assert_tools_program(program.name)
  assert_dir_path(dir)
  
  paste0(dir, "\\", program.name, "_input.txt")
  
}








