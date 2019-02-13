




#' @importFrom data.table setDF setattr
collect_tools_data <- function(
  data,
  program.name
) {
  requireNamespace("data.table")
  assert_tools_program(program.name)
  assert_tools_data(data = data, program.name = program.name)
  mandatory_col_nms <- tools_program_colnameset(
    paste0("mandatory_", program.name)
  )
  optional_col_nms <- tools_program_colnameset(
    paste0("optional_", program.name)
  )

  used_set_name <- paste0("all_", program.name)
  miss_opt_col_nms <- setdiff(optional_col_nms, names(data))
  if (length(miss_opt_col_nms)) {
    used_set_name <- paste0("mandatory_", program.name)
    message("Following optional columns were not found in data: ",
            deparse(miss_opt_col_nms),
            ". The program will still probably work.")
  }
  found_opt_col_nms <- setdiff(optional_col_nms, miss_opt_col_nms)

  col_nms <- c(mandatory_col_nms, found_opt_col_nms)
  df <- data.table::setDF(mget(col_nms, as.environment(data)))
  data.table::setattr(df, "colnameset_name", used_set_name)
  df
}





use_tools <- function(
  tools.data,
  program.name,
  how = c("interactively", "automatically")[1],
  clean = TRUE,
  verbose = TRUE
) {
  stopifnot(
    length(how) == 1,
    how %in% c("interactively", "automatically")
  )
  assert_tools_program(program.name)
  assert_tools_data(tools.data, program.name)
  assert_is_logical_nonNA_atom(clean)

  df <- collect_tools_data(data = tools.data, program.name = program.name)
  colnameset_name <- attributes(df)[["colnameset_name"]]
  if (is.null(colnameset_name)) {
    raise_internal_error("Could not retrieve implied colnameset name for data.")
  }

  input_path <- tools_program_input_file_path(program.name = program.name)

  if (verbose) {
    message("* Writing table to '", input_path, "'...\n", sep = "")
  }

  write_tools_data(x = df, file = input_path, colnameset.nm = colnameset_name,
                   verbose = verbose)
  col_nms <- names(df)
  rm("df")

  switch(
    how,
    automatically = {
      message("* calling tools automatically...")
      call_tools_program(
        program.name = program.name,
        exe.path = get_tools_exe_path(),
        working.dir = get_tools_working_dir(),
        wait.check.interval = 30L,
        wait.max.time = 60L*60L,
        verbose = verbose
      )
    },
    interactively = {
      message(
        "* Open IARC CRG Tools and follow the instructions. Ensure that the",
        "input path is", input_path, "and the output path is",
        output_path
      )
      instructions <- tools_program_instructions(program.name)
      inst_no <- formatC(seq_along(instructions),
                         digits = nchar(length(instructions)),
                         flag = " ")
      instructions <- paste0("  ", inst_no, ": ", instructions, collapse = "\n")
      message(instructions)

      proceed <- ask_yes_no(
        "* Once IARC CRG Tools has finished, select 'yes' to proceed. ",
        "The files produced by IARC CRG Tools will next be read into R. ",
        "You can cancel by selecting 'no' or 'cancel'."
      )

      if (!proceed) {
        stop("Cancelled.")
      }
    }
  )

  if (verbose) {
    message("* reading tools results")
  }

  data_list <- read_tools_results(
    program.name = program.name,
    input.col.nms = col_nms
  )

  if (clean) {
    rm_files <- c(tools_program_output_file_paths(program.name = program.name),
                  input_path)
    rm_files <- rm_files[file.exists(rm_files)]
    file.remove(rm_files)
  }


  if (verbose) {
    message("* all done!")
  }


  data_list


}





#' @title IARC CRG Tools R Interface
#' @description
#' Open IARC CRG Tools and simulate keystrokes to run the program from start
#' to finish.
#' @template tools_data
#' @template program_name
#' @template verbose
#' @details
#'
#' See \code{\link{use_tools_interactively}} for the manual but more foolproof
#' method.
#'
#' This function requires that you are able to execute .vbs scripts.
#' IARC CRG Tools is opened and keystrokes are sent using such scripts.
#' You can test this using \code{\link{can_call_vbs}}.
#'
#' Before using this function for the first time you need to run
#' \code{\link{use_tools_interactively}} to build a settings file for future
#' use into the working directory set using \code{\link{set_tools_working_dir}}.
#' This must be done for each program separately. Read more about the
#' settings files here: \code{\link{tools_settings_files}}.
#'
#' After the settings file is in place for the intended program, from then on
#' you can run this function with the same working directory set and the
#' program runs from start to finish automatically. The data is first
#' written into the set working directory, then IARC CRG Tools is run, and
#' the resulting files are read into R.
#'
#' This function should not be considered fool-proof. Currently this function
#' assumes that IARC CRG Tools has finished its computations when the
#' file size of the program output has not increased in 30 seconds. In edge
#' cases this may be incorrect. Additionally, no error-recovery logic
#' has been written for this function in case IARC CRG Tools is interrupted
#' or raises an error otherwise. Hence do not rely on this function for critical
#' processes. However the approach used here appears to work
#' fine in practice when IARC CRG Tools is not interrupted.
#' @seealso \code{\link{use_tools_interactively}}
#' @export
use_tools_automatically <- function(
  tools.data,
  program.name,
  verbose = TRUE
) {
  use_tools(tools.data, program.name, verbose = verbose, how = "automatically")
}





#' @title IARC CRG Tools R Interface
#' @description
#' Simplifies using IARC CRG Tools by preparing the data in a suitable
#' format and reading the data into R when appropriate. However, you still need
#' to use IARC CRG Tools manually.
#' @template tools_data
#' @template program_name
#' @template verbose
#' @details
#'
#' See \code{\link{use_tools_automatically}} for the automatic but
#' less foolproof method.
#'
#' This function saves the supplied data in a format useful for IARC CRG Tools
#' into the working directory set via \code{\link{set_tools_working_dir}},
#' prompts you to use the intended IARC CRG Tools program manually,
#' and reads the data back into R once you give the OK that IARC CRG Tools
#' has finished its computations.
#'
#' @export
use_tools_interactively <- function(
  tools.data,
  program.name,
  verbose
) {
  use_tools(tools.data, program.name, verbose = verbose, how = "interactively")
}


















