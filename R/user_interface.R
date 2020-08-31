




#' @importFrom data.table setDF setattr
collect_tools_data <- function(
  data,
  tool.name
) {
  assert_tool(tool.name)
  assert_tools_data(data = data, tool.name = tool.name)
  mandatory_col_nms <- tool_colnameset(
    paste0("mandatory_", tool.name)
  )
  optional_col_nms <- tool_colnameset(
    paste0("optional_", tool.name)
  )
  
  used_set_name <- paste0("all_", tool.name)
  miss_opt_col_nms <- setdiff(optional_col_nms, names(data))
  if (length(miss_opt_col_nms)) {
    used_set_name <- paste0("mandatory_", tool.name)
    message("* collect_tools_data: ",
            "the following optional columns were not found in data: ",
            deparse(miss_opt_col_nms),
            "; the tool will still probably work, but in a limited manner")
  }
  found_opt_col_nms <- setdiff(optional_col_nms, miss_opt_col_nms)
  
  col_nms <- c(mandatory_col_nms, found_opt_col_nms)
  col_nms <- intersect(tool_colnameset("all"), col_nms) # to sort them
  df <- data.table::setDF(mget(col_nms, as.environment(data)))
  data.table::setattr(df, "colnameset_name", used_set_name)
  df
}



#' @title IARC CRG Tools R Interface
#' @description
#' Open IARC CRG Tools and simulate keystrokes to run the tool from start
#' to finish.
#' @template tools_data
#' @template tool_name
#' @template verbose
#' @details
#'
#' See \code{\link{interact_with_tool}} for the manual but more foolproof
#' method.
#'
#' This function requires that you are able to execute .vbs scripts.
#' IARC CRG Tools is opened and keystrokes are sent using such scripts.
#' You can test this using \code{\link{can_call_vbs}}.
#'
#' Before using this function for the first time you need to run
#' \code{\link{interact_with_tool}} to build a settings file for future
#' use into the working directory set using \code{\link{set_tools_work_dir}}.
#' This must be done for each tool separately. Read more about the
#' settings files here: \code{\link{tools_settings_files}}.
#'
#' After the settings file is in place for the intended tool, from then on
#' you can run this function with the same working directory set and the
#' tool runs from start to finish automatically. The data is first
#' written into the set working directory, then IARC CRG Tools is run, and
#' the resulting files are read into R.
#'
#' This function should not be considered fool-proof. Currently this function
#' assumes that IARC CRG Tools has finished its computations when the
#' file size of the tool output has not increased in 30 seconds. In edge
#' cases this may be incorrect. Additionally, no error-recovery logic
#' has been written for this function in case IARC CRG Tools is interrupted
#' or raises an error otherwise. Hence do not rely on this function for critical
#' processes. However the approach used here appears to work
#' fine in practice when IARC CRG Tools is not interrupted.
#' @seealso \code{\link{interact_with_tool}}
#' @name interface_with_tool


#' @rdname interface_with_tool
#' @export
#' @param how `[character]` (mandatory, default `"interactively"`)
#' - `"interactively"`: the user must open IARC CRG Tools manually and interact
#'   with it according to instructions, but the dataset is written to disk
#'   and read back into R by this function
#' - `"automatically"`: like `"interactively"`, but the appropriate tool
#'   is attempted to be called without any user interaction
#' @param clean `[logical]` (optional, default `FALSE`)
#' 
#' - `TRUE`: all input and output files for IARC CRG Tools will be removed
#'   from disk after the results have been read into R
#' - `FALSE`: all files are let to be in peace
#' @param verbose `[logical]` (optional, default `FALSE`)
#' 
#' - `TRUE`: emit messages which are not needed for correct usage but which
#'   tell at what stage the is process and can be helpful in debugging
#' - `FALSE`: only necessary messages are emitted
interface_with_tool <- function(
  data,
  tool.name,
  how = c("interactively", "automatically")[1],
  clean = FALSE,
  verbose = FALSE
) {
  stopifnot(
    length(how) == 1,
    how %in% c("interactively", "automatically")
  )
  if (how == "automatically") {
    stop("how = 'automatically' under development")
  }
  assert_tool(tool.name)
  assert_tools_data(data, tool.name)
  assert_is_logical_nonNA_atom(clean)
  
  df <- collect_tools_data(data = data, tool.name = tool.name)
  colnameset_name <- attributes(df)[["colnameset_name"]]
  if (is.null(colnameset_name)) {
    raise_internal_error("Could not retrieve implied colnameset name for data.")
  }
  
  input_path <- tool_input_file_path(tool.name = tool.name)
  
  if (verbose) {
    message("* interface_with_tool: selected columns; first five row of working table: ")
    print(head(df))
    message("* interface_with_tool: Writing table to '", input_path, "'...")
  }
  
  write_tools_data(x = df, file = input_path, colnameset.nm = colnameset_name,
                   verbose = verbose)
  col_nms <- names(df)
  rm("df")
  
  switch(
    how,
    automatically = {
      message("* interface_with_tool: calling tools automatically...")
      call_tool(
        tool.name = tool.name,
        tool.exe.path = get_tool_exe_path(),
        working.dir = get_tool_dir(tool.name),
        wait.check.interval = 30L,
        wait.max.time = 60L * 60L,
        verbose = verbose
      )
    },
    interactively = {
      output_path <- tool_output_file_paths(tool.name = tool.name)[1L]
      message("* interface_with_tool: calling tools interactively...")
      message(
        "- open IARC CRG Tools\n",
        "- start the tool titled ", 
        deparse(tool_real_name_of_clean_name(tool.name)), "\n",
        "- supply this as input path: ", input_path, "\n",
        "- supply this as output path: ", output_path, "\n",
        "- choose columns when prompted; the columns in the input file are ",
        "in order the following:\n  ", deparse(col_nms), "\n",
        "- if applicable, make sure you select ",
        "\"Creates one record per type of error ",
        "(ie the same case may appear several times).\" or any other setting",
        "with the same effect\n",
        "- choose other settings as is appropriate for your dataset and run ",
        "the tool\n",
        "- once it has finished, (press OK in IARC CRG Tools and) select ",
        "'yes' in the prompt below"
      )
      proceed <- ask_yes_no(
        "- select 'yes' to proceed and read the results into R ",
        "or cancel by selecting 'no' or 'cancel'."
      )
      
      if (!proceed) {
        stop("Cancelled.")
      }
    }
  )
  
  if (verbose) {
    message("* interface_with_tool: reading tools results")
  }
  
  data_list <- read_tools_results(
    tool.name = tool.name,
    input.col.nms = col_nms
  )
  
  if (clean) {
    if (verbose) {
      message("* interface_with_tool: clean = TRUE, deleting input and output datasets ",
              "from disk")
    }
    rm_files <- c(tool_output_file_paths(tool.name = tool.name),
                  input_path)
    rm_files <- rm_files[file.exists(rm_files)]
    file.remove(rm_files)
  }
  
  if (verbose) {
    message("* interface_with_tool: finished")
  }
  
  data_list
}




#' @rdname interface_with_tool
#' @export
automate_tool <- function(
  data,
  tool.name,
  clean = FALSE,
  verbose = FALSE
) {
  interface_with_tool(
    data = data, 
    tool.name = tool.name, 
    clean = clean,
    verbose = verbose, 
    how = "automatically"
  )
}





#' @rdname interface_with_tool
#' @export
interact_with_tool <- function(
  data,
  tool.name,
  clean = FALSE,
  verbose = FALSE
) {
  interface_with_tool(
    data = data, 
    tool.name = tool.name, 
    clean = clean, 
    verbose = verbose, 
    how = "interactively"
  )
}



#' @importFrom data.table data.table setkeyv setDT :=
#' @export
#' @rdname interface_with_tool
#' @param record.ids `[integer]` (mandatory, no default)
#' 
#' IDs of records for which to retrieve any record-specific results from
#' `tool.results`
#' @param tool.results `[list]` (mandatory, no default)
#' 
#' list of tables and/or log texts as output by one of the interface functions
#' to IARC CRG Tools (e.g. [interact_with_tool])
connect_tool_results_to_observations <- function(
  record.ids,
  tool.results
) {
  stopifnot(
    is.integer(record.ids),
    
    inherits(tool.results, "list")
  )
  
  is_usable_df <- vapply(tool.results, function(x) {
    is.data.frame(x) && all(c("record_id", "tool_text") %in% names(x))
  }, logical(1L))
  if (!any(is_usable_df)) {
    stop("none of the elements of tool.results is a data.frame with the ",
         "columns 'record_id' and 'tool_text'; ",
         "most likely there are no results to process")
  }
  wh_usable_df <- which(is_usable_df)
  dt <- data.table::data.table(record_id = record.ids)
  data.table::setkeyv(dt, "record_id")
  
  lapply(wh_usable_df, function(df_no) {
    df <- tool.results[[df_no]]
    result_dt <- data.table::setDT(list(
      record_id = df[["record_id"]],
      text = df[["tool_text"]]
    ))
    if (all(is.na(result_dt[["text"]]))) {
      return(NULL)
    }
    if (any(duplicated(result_dt, by = "record_id"))) {
      result_dt <- result_dt[
        j = list(text = paste0(text, collapse = "; ")),
        keyby = "record_id"
      ]
    }
    text_col_nm <- names(tool.results)[df_no]
    i.text <- NULL # to appease R CMD CHECK
    dt[
      i = result_dt,
      on = "record_id",
      j = (text_col_nm) := i.text
    ]
    NULL
  })
  return(dt[])
}














