iarc_dataset <- function(
    data,
    tool.name
) {
  assert_tool(tool.name)
  assert_tools_data(data = data, tool.name = tool.name)
  mandatory_col_nms <- iarccrgtools::tool_colnameset(
    paste0("mandatory_", tool.name)
  )
  optional_col_nms <- iarccrgtools::tool_colnameset(
    paste0("optional_", tool.name)
  )

  used_set_name <- paste0("all_", tool.name)
  miss_opt_col_nms <- setdiff(optional_col_nms, names(data))
  if (length(miss_opt_col_nms)) {
    used_set_name <- paste0("mandatory_", tool.name)
  }
  found_opt_col_nms <- setdiff(optional_col_nms, miss_opt_col_nms)

  col_nms <- c(mandatory_col_nms, found_opt_col_nms)
  col_nms <- intersect(iarccrgtools::tool_colnameset("all"), col_nms) # sort
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
  # assertions -----------------------------------------------------------------
  stopifnot(
    length(how) == 1,
    how %in% c("interactively", "automatically")
  )
  assert_tool(tool.name)
  assert_tools_data(data, tool.name)
  assert_is_logical_nonNA_atom(clean)

  # data -----------------------------------------------------------------------
  # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
  # First, a subset of columns from `data` is collected based on `tool.name`.
  # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
  df <- iarc_dataset(data = data, tool.name = tool.name)
  colnameset_name <- attributes(df)[["colnameset_name"]]
  if (is.null(colnameset_name)) {
    raise_internal_error("Could not retrieve implied colnameset name for data.")
  }

  # cache ----------------------------------------------------------------------
  # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
  # Then the cache is checked for pre-existing results for the given
  # `data` and `tool.name`. See e.g. `[iarccrgtools::cache_metadata_read]`.
  # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
  current_hash <- iarccrgtools::cache_hash(data)
  tool_work_dir_path <- iarc_toolworkdir_get(tool.name, current_hash)
  input_file_path <- tool_input_file_path(
    dir = tool_work_dir_path,
    tool.name = tool.name
  )
  cache_metadata <- iarccrgtools::cache_metadata_read()
  cache_hash <- cache_metadata[["hash"]][
    cache_metadata[["input_file_path"]] == input_file_path
  ]
  read_cached_results <- FALSE
  if (identical(cache_hash, current_hash)) {
    # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
    # If pre-existing results are found, the user is prompted whether to use
    # the pre-existing results from disk and skip IARC CRG Tools altogether,
    # or to proceed to running IARC CRG Tools (again).
    # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
    message("* iarccrgtools::interface_with_tool: looks like a dataset ",
            "identical to the one you have supplied already exists in ",
            deparse(input_file_path),
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
    # write --------------------------------------------------------------------
    if (verbose) {
      message("* iarccrgtools::interface_with_tool: selected columns; ",
              "first five row of working table: ")
      print(utils::head(df))
      message("* iarccrgtools::interface_with_tool: ",
              "Writing table to '", input_file_path, "'...")
    }
    # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
    # If there were no cached results / the user did not want to read them,
    # `[iarccrgtools::iarc_input_write]` is called.
    # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
    iarccrgtools::iarc_input_write(
      x = df, file = input_file_path, 
      colnameset.name = colnameset_name,
      verbose = verbose
    )
    rm(list = "df")
    # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
    # Cache metadata is then updated by calling
    # `[iarccrgtools::cache_metadata_append_or_replace]`.
    # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
    iarccrgtools::cache_metadata_append_or_replace(
      hash = current_hash,
      working.dir = tool_work_dir_path,
      input.file.path = input_file_path
    )

    # settings -----------------------------------------------------------------
    # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
    # iarccrgtools attempts to write (sensible, default) parameters
    # (e.g. path to input / output) for use by IARC CRG Tools.
    # The user has the responsibility to make sure that the parameters
    # are correct for their dataset. You will see them when you run IARC CRG
    # Tools.
    # @codedoc_insert_comment_block details(iarccrgtools:::parameter_file_write)
    # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
    if (parameter_contents_are_available(colnameset_name)) {
      parameter_file_write(parameter_file_contents(
        colnameset.name = colnameset_name,
        tool.work.dir = tool_work_dir_path
      ))
    }
    on.exit(unlink(parameter_file_path(), force = TRUE))
    if (iarccrgtools::tool_settings_are_available(colnameset_name)) {
      # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
      # If R package iarccrgtools contains pre-defined (default) settings
      # (e.g. positions of specific columns in the file on disk), those are
      # written into the dir given by `[iarc_toolworkdir_get]`.
      # The user has the responsibility to make sure that the settings
      # are correct for their dataset. You will see them when you run IARC CRG
      # Tools.
      # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
      iarccrgtools::tool_settings_copy(
        tgt.dir.path = tool_work_dir_path,
        colnameset.name = colnameset_name
      )
    }

    # call ---------------------------------------------------------------------
    switch(
      how,
      automatically = {
        message("* iarccrgtools::interface_with_tool: calling tools ",
                "automatically...")

        iarc_toolexe_call(tool.name = tool.name)
      },
      interactively = {
        # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
        # With the data and parameters in place, the user is next informed what
        # they have to do in IARC CRG Tools.
        # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
        output_path <- tool_output_file_paths(
          tool.name = tool.name,
          dir = iarc_toolworkdir_get(tool.name, hash = current_hash)
        )[1L]
        message("* iarccrgtools::interface_with_tool: calling tools ",
                "interactively...")
        message(
          "- open IARC CRG Tools\n",
          "- start ",
          deparse(iarccrgtools::tool_menu_name(tool.name)), " -> ",
          deparse(iarccrgtools::tool_menu_item_name(tool.name)), "\n",
          "- supply this as input path: ", input_file_path, "\n",
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

  # read -----------------------------------------------------------------------
  if (verbose) {
    message("* iarccrgtools::interface_with_tool: reading tools results")
  }
  # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
  # When permission is given to read the data into R,
  # `[iarccrgtools::iarc_output_read]` is called.
  # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
  data_list <- iarccrgtools::iarc_output_read(
    tool.name = tool.name,
    input.col.nms = col_nms,
    hash = current_hash
  )

  # clean ----------------------------------------------------------------------
  if (clean) {
    if (verbose) {
      message("* iarccrgtools::interface_with_tool: clean = TRUE. Deleting ",
              "dir ", deparse(tool_work_dir_path))
    }
    # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
    # Finally, if `clean = TRUE`, `[iarccrgtools::cache_clean_hash]` for the
    # hash of the dataset given by the user.
    # @codedoc_comment_block details(iarccrgtools:::interface_with_tool)
    iarccrgtools::cache_clean_hash(current_hash)
  }

  # done -----------------------------------------------------------------------
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
#' @eval c(
#'   codedoc::codedoc_lines(
#'     "^examples\\(iarccrgtools::interact_with_tool\\)$"
#'   ),
#'   codedoc::codedoc_lines(
#'     "^details\\(iarccrgtools::interact_with_tool\\)$"
#'   )
#' )
interact_with_tool <- function(
    data,
    tool.name,
    clean = FALSE,
    verbose = FALSE
) {
  # @codedoc_comment_block news("iarccrgtools::interact_with_tool", "2022-11-03", "0.2.28")
  # Document what steps `iarccrgtools::interact_with_tool` performs, in
  # particular caching and parameter/setting files.
  # @codedoc_comment_block news("iarccrgtools::interact_with_tool", "2022-11-03", "0.2.28")
  
  # @codedoc_comment_block news("iarccrgtools::interact_with_tool", "2023-03-29", "0.3.0")
  # `iarccrgtools::interact_with_tool` now accepts `icdo3_topography` value with
  # up to five characters.
  #
  # `iarccrgtools::interact_with_tool` now no longer requires `subject_id`,
  # `record_id` to be of class integer --- it can be any class. Remember that
  # the class of each column in results read into R is ultimately decided by
  # `data.table::fread`.
  # @codedoc_comment_block news("iarccrgtools::interact_with_tool", "2023-03-29", "0.3.0")
  
  # @codedoc_comment_block details(iarccrgtools::interact_with_tool)
  # @details
  # `iarccrgtools::interact_with_tool` performs the following steps.
  # @codedoc_insert_comment_block details(iarccrgtools:::interface_with_tool)
  # @codedoc_comment_block details(iarccrgtools::interact_with_tool)
  
  # @codedoc_comment_block examples(iarccrgtools::interact_with_tool)
  # @examples
  #
  # # iarccrgtools::interact_with_tool
  # \dontrun{
  # @codedoc_comment_block R_package_example(iarccrgtools)
  # dir_path <- tempdir()
  # iarccrgtools::iarc_workdir_set(dir_path)
  #
  # tool_name <- "check"
  # subset <- "mandatory"
  # iarc_df <- iarccrgtools::tool_colnameset_example_dataset(
  #   paste0(subset, "_", tool_name), n.rows = 10L
  # )
  # results <- iarccrgtools::interact_with_tool(
  #   iarc_df, tool.name = tool_name, clean = TRUE
  # )
  #
  # result_df <- iarccrgtools::connect_tool_results_to_observations(
  #   record.ids = iarc_df[["record_id"]], tool.results = results
  # )
  # @codedoc_comment_block R_package_example(iarccrgtools)
  # }
  # @codedoc_comment_block examples(iarccrgtools::interact_with_tool)

  interface_with_tool(
    data = data,
    tool.name = tool.name,
    clean = clean,
    verbose = verbose,
    how = "interactively"
  )
}

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
#' `iarccrgtools::connect_tool_results_to_observations`
#' returns a `data.table` with
#' `length(record.ids)` rows; it has column
#' `record_id` and additional columns depending on results in
#' `tool.results`; this function goes through each object in `tool.results`
#' and if an object is a `data.table` with columns `record_id` and `tool_text`,
#' each record appearing in that `data.table` is marked in the output
#' `data.table` in a logical column (e.g. ` in_multiple_primary_input.exl`)
#' and any text in `tool_text` is collected into a separate column
#' (e.g. `multiple_primary_input.exl`); therefore the columns in the output of
#' `connect_tool_results_to_observations` vary by tool used.
#' @importFrom data.table .SD :=
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
        j = list(text = paste0(.SD[[1]], collapse = "; ")),
        .SDcols = "text",
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
