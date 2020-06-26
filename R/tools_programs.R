




#' @title IARC CRG Tools Program Names
#' @description
#' This function simply returns a haracter string vector of IARC CRG Tools
#' program names supported by this R package.
#' @return A character string vector.
#' @export
tools_program_names <- function() {
  stop("use program_clean_names() instead")
}





tools_program_commands <- function(program.name) {
  assert_tools_program(program.name)
  program_guides <- get_internal_dataset("program_guides")
  is_in_program <- program_guides$program_name == program.name
  ks <- program_guides[["command"]][is_in_program]
  names(ks) <- program_guides[["instruction"]][is_in_program]
  ks
}

tools_program_instructions <- function(program.name) {
  assert_tools_program(program.name)

  program_guides <- get_internal_dataset("program_guides")
  is_in_program <- program_guides$program_name == program.name
  program_guides[["instruction"]][is_in_program]

}




#' @title Column Names
#' @name column_names
#' @description
#' Utilities for handling sets of required column names for each
#' IARC CRG Tools program.
NULL

#' @describeIn column_names each column name set has its own name. This function
#' returns all the names of the sets.
#' @export
tools_program_colnameset_names <- function() {
  prog_nms <- tools_program_names()
  prog_set_nms <- paste0(c("all_", "mandatory_", "optional_"),
                         rep(prog_nms, each = 3))
  c(prog_set_nms, "all")
}

#' @describeIn column_names returns a set of column name given the name of the
#' set itself.
#' @param set.nm string; name of a column set; only those returned by
#' \code{tools_program_colnameset_names} are allowed
#' @export
tools_program_colnameset <- function(
  set.nm
  ) {
  assert_tools_colnameset_name(set.nm)

  col_specs <- get_internal_dataset("column_specifications")
  col_specs_col_nm <- paste0("set_", set.nm)

  if (!col_specs_col_nm %in% names(col_specs)) {
    raise_internal_error(
      "Expected 'column_specifications' to have column with name ",
      deparse(col_specs_col_nm), " but it didn't."
    )
  }

  col_specs[["column_name"]][col_specs[[col_specs_col_nm]]]

}

#' @describeIn column_names returns the required class for each column by
#' column name
#' @param col.nms character string vector; names of columns; must be subset of
#' items returned by \code{tools_program_colnameset("all")}
#' @export
tools_program_column_classes <- function(
  col.nms
) {
  stopifnot(
    is.character(col.nms)
  )
  col_specs <- get_internal_dataset("column_specifications")

  cn <- col_specs[["column_name"]]
  cc <- col_specs[["class"]]


  cc[match(col.nms, cn, nomatch = NA_integer_)]

}

#' @describeIn column_names returns short plain English explanation of column
#' @export
tools_program_column_infos <- function(
  col.nms
) {
  stopifnot(
    is.character(col.nms)
  )
  col_specs <- get_internal_dataset("column_specifications")

  cn <- col_specs[["column_name"]]
  ci <- col_specs[["info"]]


  ci[match(col.nms, cn, nomatch = NA_integer_)]

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

  prog_files_df <- get_internal_dataset("program_output_files")
  is_in_program <- prog_files_df$program_name == program.name
  prog_file_suffixes <- prog_files_df$file_name_suffix[is_in_program]
  prog_file_is_table <- prog_files_df$is_table[is_in_program]

  file_paths <- paste0(dir, "\\", program.name, prog_file_suffixes)
  file_paths <- normalize_path(file_paths)
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





tools_program_window_name <- function(
  program.name
) {
  assert_tools_program(program.name)
  pwn <- get_internal_dataset("program_window_names")
  pwn[pwn$program_name == program.name, "program_window_name"]
}












