




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
    message("* iarccrgtools::collect_tools_data: ",
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




# @param how `[character]` (mandatory, default `"interactively"`)
# - `"interactively"`: the user must open IARC CRG Tools manually and interact
#   with it according to instructions, but the dataset is written to disk
#   and read back into R by this function
# - `"automatically"`: like `"interactively"`, but the appropriate tool
#   is attempted to be called without any user interaction
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

  
  sha_file_path <- tool_cache_sha_file_path(tool.name = tool.name)
  if (file.exists(sha_file_path)) {
    cache_sha <- readLines(sha_file_path, n = 1L)
  } else {
    cache_sha <- digest::digest(runif(n = 1L), algo = "sha256")
  }
  current_sha <- digest::digest(df, algo = "sha256")
  read_cached_results <- FALSE
  if (identical(cache_sha, current_sha)) {
    message("* iarccrgtools::interface_with_tool: looks like a dataset ",
            "identical to the one you have supplied already exists in ", 
            deparse(input_path),
            ". would you like to skip using IARC CRG Tools",
            "and read in the output files that resulted from your previous ",
            "time? select 'yes' to read the previous results into R without ",
            "using IARC CRG Tools, 'no' to proceed with writing the input ",
            "on the hard drive and running IARC CRG Tools, or 'cancel' ",
            "to abort this program.")
    read_cached_results <- ask_yes_no()
  }
  
  col_nms <- names(df)
  if (!read_cached_results) {
    if (verbose) {
      message("* iarccrgtools::interface_with_tool: selected columns; first five row of working table: ")
      print(head(df))
      message("* iarccrgtools::interface_with_tool: Writing table to '", input_path, "'...")
    }
    write_tools_data(x = df, file = input_path, colnameset.nm = colnameset_name,
                     verbose = verbose)
    writeLines(current_sha, sha_file_path)
    
    rm(list = "df")
    switch(
      how,
      automatically = {
        message("* iarccrgtools::interface_with_tool: calling tools ",
                "automatically...")
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
        message("* iarccrgtools::interface_with_tool: calling tools ",
                "interactively...")
        message(
          "- open IARC CRG Tools\n",
          "- start ",
          deparse(tool_menu_name(tool.name)), " -> ",
          deparse(tool_real_name_of_clean_name(tool.name)), "\n",
          "- supply this as input path: ", input_path, "\n",
          "- supply this as output path: ", output_path, "\n",
          "- choose columns when prompted; the columns in the input file are ",
          "in order the following:\n  ", deparse(col_nms), "\n",
          "- if applicable, make sure you select ",
          "\"Creates one record per type of error ",
          "(ie the same case may appear several times).\" or any other setting",
          " with the same effect\n",
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
  }
  

  if (verbose) {
    message("* iarccrgtools::interface_with_tool: reading tools results")
  }

  data_list <- read_tools_results(
    tool.name = tool.name,
    input.col.nms = col_nms
  )

  if (clean) {
    if (verbose) {
      message("* iarccrgtools::interface_with_tool: clean = TRUE, deleting ",
              "input and output datasets from disk")
    }
    rm_files <- c(tool_output_file_paths(tool.name = tool.name),
                  input_path)
    rm_files <- rm_files[file.exists(rm_files)]
    file.remove(rm_files)
  }

  if (verbose) {
    message("* iarccrgtools::interface_with_tool: finished")
  }

  data_list
}




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





#' @title IARC CRG Tools R Interface
#' @description
#' Write data for IARC CRG Tools and read it back into R after the run
#' has finished.
#' @template tools_data
#' @template tool_name
#' @param clean `[logical]` (optional, default `FALSE`)
#'
#' - `TRUE`: all input and output files for IARC CRG Tools will be removed
#'   from disk after the results have been read into R
#' - `FALSE`: all files are left to be in peace
#' @template verbose
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
#' @rdname interact_with_tool
#' @param record.ids `[integer]` (mandatory, no default)
#'
#' IDs of records for which to retrieve any record-specific results from
#' `tool.results`
#' @param tool.results `[list]` (mandatory, no default)
#'
#' list of tables and/or log texts as output by one of the interface functions
#' to IARC CRG Tools (e.g. [interact_with_tool])
#' @details
#' - `connect_tool_results_to_observations` returns a `data.table` with
#'   `length(record.ids)` rows; it has column
#'   `record_id` and additional columns depending on results in
#'   `tool.results`; this function goes through each object in `tool.results`
#'   and if an object is a `data.table` with columns `record_id` and `tool_text`,
#'   each record appearing in that `data.table` is marked in the output
#'   `data.table` in a logical column (e.g. ` in_multiple_primary_input.exl`)
#'   and any text in `tool_text` is collected into a separate column
#'   (e.g. `multiple_primary_input.exl`); therefore the columns in the output of
#'   `connect_tool_results_to_observations` vary by tool used.
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
    if (any(duplicated(result_dt, by = "record_id"))) {
      result_dt <- result_dt[
        j = list(text = paste0(text, collapse = "; ")),
        keyby = "record_id"
      ]
    }
    text_col_nm <- names(tool.results)[df_no]
    in_result_set_col_nm <- paste0("in_", text_col_nm)
    dt[, (in_result_set_col_nm) := FALSE]
    dt[
      i = result_dt,
      on = "record_id",
      j = (in_result_set_col_nm) := TRUE
    ]
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














