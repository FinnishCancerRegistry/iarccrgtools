




dir_is_writable <- function(
  dir.path
) {
  assert_dir_path(dir.path)

  tf <- tempfile(pattern = "file_", tmpdir = dir.path, fileext = ".tmp")

  on.exit({
    if (file.exists(tf)) {
      file.remove(tf)
    }
  })

  test <- tryCatch(
    {
      writeLines("test string", con = tf)
      TRUE
    },
    error = function(e) e,
    warning = function(w) w
  )

  if (!identical(test, TRUE)) {
    test <- FALSE
  }
  test
}




#' @title Example Datasets
#' @description
#' Create example datasets with correct column names and column data types
#' and nonsense contents.
#' @param set.nm `[character]` (mandatory, no default)
#' 
#' one of the values given in the output of 
#' `[iarccrgtools::tool_colnameset_names()]`
#' @param n.rows `[integer]` (mandatory, default `10L`)
#' 
#' number of rows to have in the created example dataset; only one row has been
#' defined and that is repeated this many times; the purpose of having a larger
#' example dataset of this kind is mainly to test memory use
#' @export
create_example <- function(
  set.nm,
  n.rows = 10L
) {
  stopifnot(
    length(n.rows) == 1,
    n.rows %% 1 == 0,
    n.rows > 0
  )
  col_nms <- tool_colnameset(set.nm = set.nm)

  df <- data.frame(
    subject_id = 1L,
    record_id = 1L,
    record_order = 1L,
    sex = 1L,
    icd9 = "5020",
    icd10 = "5020",
    icdo1_topography = "5020",
    icdo1_histology = "8522",
    icdo1_grade = 1L,
    icdo2_topography = "502",
    icdo2_histology = "8522",
    icdo3_topography = "502",
    icdo3_histology = "8522",
    icdo3_behavior = 1L,
    icdo3_grade = 1L,
    basis = 1L,
    bi_date = as.Date("1950-12-31"),
    dg_date = as.Date("2000-12-31"),
    dg_age = 50L,
    stringsAsFactors = FALSE
  )


  df <- df[rep(1L, n.rows), col_nms]
  if ("record_id" %in% names(df)) {
    df[["record_id"]] <- 1:nrow(df)
  }
  if ("subject_id" %in% names(df)) {
    df[["subject_id"]] <- 1:nrow(df)
  }
  return(df[])
}





#' @name work_dir
#' @title IARC CRG Tools Working Directory
#' @description
#' Get and set directory where input and output files of IARC CRG Tools
#' are stored.
#' @details
#' The working directory is where files for IARC CRG Tools and created by
#' IARC CRG Tools should live. This is not the same directory where the
#' executable for IARC CRG Tools is. Instead, the working directory is 
#' recommended to be created by you manually in advance. You may also use a 
#' temporary directory if you don't want to store any results 
#' (see [base::tempdir]).
#' 
#' The tools working directory set by `set_tools_work_dir` will itself be
#' populated by tool-specific directories, e.g. `"my_dir/check/"` will contain
#' results for the "check" tool.
NULL

#' @describeIn work_dir sets working directory
#' @param dir string; path to a directory
#' @export
set_tools_work_dir <- function(dir) {
  assert_dir_path(dir)
  dir <- normalize_path(dir)
  assign(x = "path", value = dir, envir = wd_env)
}

#' @describeIn work_dir gets current root working directory as string
#' @export
get_tools_work_dir <- function() {
  if (identical(wd_env$path, FALSE)) {
    stop("Working directory for IARC CRG Tools not set --- ",
         "see ?set_tools_work_dir")
  } else if (!dir.exists(wd_env$path)) {
    stop("Supplied IARC CRG Tools root working directory does not exist: ",
         deparse(wd_env$path), "; see ?set_tools_work_dir")
  }
  wd_env$path
}
wd_env <- new.env(parent = emptyenv())
wd_env$path <- FALSE

#' @describeIn work_dir gets the working directory of an individual tool under
#' the main working directory set by `set_tools_work_dir`
#' @export
#' @template tool_name
get_tool_work_dir <- function(tool.name) {
  assert_tool(tool.name = tool.name)
  work_dir <- get_tools_work_dir()
  dir <- normalize_path(paste0(work_dir, "\\", tool.name))
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  return(dir)
}




#' @title Write File to Use as IARC CRG Tools Input
#' @description
#' A text file is written in a fixed field format.
#' @param x a data.frame
#' @template colnameset_name
#' @param file `[character]` (mandatory, no default)
#' 
#' path where `x` will be written to; if the file already exists, this
#' function prompts you whether to overwrite or not; in non-interactive
#' use
#' @param overwrite `[NULL, logical]` (optional, default `NULL`)
#' 
#' - `NULL`: if `file` already exists, user is prompted whether to overwrite;
#'   in non-interactive mode (see `?interactive`) a pre-existing file causes
#'   an error
#' - `TRUE`: any pre-existing file is overwritten without prompting
#' - `FALSE` any pre-existing file is overwritten causes an error
#' @param verbose `[logical]` (mandatory, no default)
#' 
#' if `TRUE`, the function emits messages during it's run to let you know
#' what's happening
#' @param ... 
#' 
#' arguments passed to [write_fwf];
#' e.g. try \code{nThread = x} where \code{x} is a desired number of cores to
#' use when writing
#' 
#' @importFrom data.table fwrite setDF
#' @export
write_tools_data <- function(
  x,
  colnameset.nm = tool_colnameset_names()[1],
  file = tempfile(fileext = ".txt", tmpdir = get_tools_work_dir()),
  overwrite = NULL,
  verbose = FALSE,
  ...
) {
  assert_is_logical_nonNA_atom(verbose)
  assert_dataframe(x)
  col_nms <- tool_colnameset(colnameset.nm)
  assert_names(x, expected.names = col_nms, arg.nm = "x")
  assert_write_file_path(path = file)
  
  if (file.exists(file) && is.null(overwrite)) {
    if (!interactive()) {
      stop("* write_tools_data: File ", deparse(file), " already existed ",
           "so aborted. see ?write_tools_data")
    }
    ow <- ask_yes_no("* write_tools_data: File ", deparse(file),
                     " already exists. overwrite?")
    if (!ow) {
      message("* write_tools_data: Cancelled writing table to ",
              deparse(file), ".")
      return(invisible(NULL))
    }
  }
  
  if (verbose) {
    message("* write_tools_data: collecting and transforming data...")
  }
  x <- data.table::setDT(mget(col_nms, as.environment(x)))
  
  is_Date_col <- vapply(x, inherits, logical(1L), what = "Date")
  date_col_nms <- names(x)[is_Date_col]
  lapply(date_col_nms, function(col_nm) {
    col <- x[[col_nm]]
    data.table::set(
      x,
      j = col_nm,
      value = NULL
    )    
    data.table::set(
      x,
      j = col_nm,
      value = format(col, "%Y%m%d")
    )
    invisible(NULL)
    
  })
  
  is_double_col <- vapply(x, is.double, logical(1L))
  is_double_col <- is_double_col & vapply(x, is.numeric, logical(1L))
  double_col_nms <- names(x)[is_double_col]
  lapply(double_col_nms, function(col_nm) {
    col <- x[[col_nm]]
    data.table::set(
      x,
      j = col_nm,
      value = NULL
    )    
    data.table::set(
      x,
      j = col_nm,
      value = as.integer(round(col))
    )
    invisible(NULL)
  })
  data.table::setcolorder(x, col_nms)

  ## to ensure that, when IARC CRG Tools writes a new column, when it is read
  ## into R, the new column will not mix with the last column in x.
  data.table::set(
    x = x,
    j = ".__this_is_a_buffer_column_yo__.",
    value = ""
  )
  
  if (verbose) {
    message("* write_tools_data: ready to write to disk; first five rows ",
            "of current dataset:")
    print(head(x))
  }
  
  if (verbose) {
    message("* write_tools_data: writing...")
    t_write <- proc.time()
  }
  widths <- tool_column_fwf_widths(
    setdiff(names(x), ".__this_is_a_buffer_column_yo__.")
  )
  widths <- c(widths, 0L)
  write_fwf(
    x = x, 
    widths = widths,
    path = file,
    sep = ";",
    dec = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE,
    ...
  )
  if (verbose) {
    message(
      "* write_tools_data: done writing; ", data.table::timetaken(t_write)
    )
  }
  invisible(NULL)
}





n_file_lines <- function(path) {
  assert_file_path(path)

  f <- file(path, open="rb")
  r <- as.raw(10L)
  n_lines <- 0L
  while (length(chunk <- readBin(f, "raw", 65536)) > 0) {
    n_lines <- n_lines + sum(chunk == r)
  }
  close(f)
  n_lines
}





#' @md
#' @title IARC CRG Tools Results
#' @description Read IARC CRG Tools results into R.
#' @template tool_name
#' @param input.col.nms `NULL` (default) or a character string vector of column
#' names; when not `NULL`, allows setting column names on the tables that were
#' read. See Details.
#' @param verbose if TRUE, this function gives messages during its process
#' @return
#' A list of data.frames (where the read file was a table) and/or character
#' string vectors (where the read file was a non-table file such as a log file).
#' The names of the list correspond to the names of the files that were read.
#' @details
#' When `input.col.nms` is a character string vector, it is attempted to be
#' used as the set of column names for any tables that are read by this
#' function. You should supply the same column names that were in the file
#' saved for use by IARC CRG Tools. If IARC CRG Tools adds a column in addition
#' to the ones you had in input data, that column will gain the name
#' `"tool_text"` automatically.
#' @importFrom data.table setnames
#' @export
read_tools_results <- function(
  tool.name,
  input.col.nms = NULL,
  verbose = TRUE
) {
  assert_tool(tool.name = tool.name)
  dir <- get_tool_work_dir(tool.name = tool.name)

  file_paths <- tool_output_file_paths(tool.name = tool.name, dir = dir)

  output_list <- lapply(seq_along(file_paths), function(i) {

    file_path <- file_paths[i]
    path_type <- names(file_paths)[i]

    if (verbose) {
      message("* read_tools_results: attempting to read file from ",
              deparse(unname(file_path)))
    }

    if (!file.exists(file_path)) {
      if (verbose) {
        message("* read_tools_results: file '", file_path, "' did not exist; ",
                "returning NULL")
      }
      return(NULL)
    }
    n_lines <- n_file_lines(path = file_path)
    if (n_lines == 0) {
      if (verbose) {
        message("* read_tools_results: file '", file_path, "' had zero rows; ",
                "returning NULL")
      }
      return(NULL)
    }

    if (identical(path_type, "is_table")) {
      out <- read_matching_table_rows(
        file = file_path, 
        pattern = "^[0-9]*;"
      )
      if (is.null(out) || nrow(out) == 0) {
        out <- data.frame(NULL)
      } else {

        if (!is.null(input.col.nms)) {

          if (ncol(out) == length(input.col.nms)+1L) {
            input.col.nms <- c(input.col.nms, "tool_text")
          }

          if (length(input.col.nms) == ncol(out)) {
            data.table::setnames(out, old = names(out), new = input.col.nms)
          }
        }

      }


    } else if (identical(path_type, "is_not_table")) {
      out <- readLines(file_path)
    } else {
      raise_internal_error(
        "file_path = ", deparse(file_path), " file type was not defined. "
      )
    }
    if (verbose) {
      message("* read_tools_results: file successfully read ")
    }



    out
  })
  output_list[] <- lapply(output_list, function(obj) {
    if (is.data.frame(obj) && nrow(obj) == 0) {
      return(NULL)
    }
    obj
  })

  names(output_list) <- basename(file_paths)
  output_list
}





readlines_from_to <- function(
  file,
  from = 1,
  to = n_file_lines(file),
  ...
) {
  assert_file_path(file)
  stopifnot(
    length(from) == 1,
    length(to) == 1,
    from %% 1 == 0,
    to %% 1 == 0,
    from > 0,
    to > 0,
    from <= to
  )

  file_con <- file(file, "r")
  row <- 0L
  while (row < from-1L) {
    readLines(file_con, n = 1L, ...)
    row <- row+1L
  }

  lines <- readLines(file_con, n = to+1L-from)
  close(file_con)
  lines
}




guess_table_header_and_footer_sizes <- function(
  file,
  n.guessing.lines = 100,
  data.regex = "^[0-9]+[;]"
) {
  assert_file_path(file)
  stopifnot(
    length(n.guessing.lines) == 1,
    n.guessing.lines > 0
  )
  n_lines <- n_file_lines(file)

  if (n_lines %in% 0:1) {
    return(c(0L, 0L))
  }

  n.guessing.lines <- min(n.guessing.lines, n_lines)
  header <- readLines(file, n = n.guessing.lines)
  footer <- readlines_from_to(file = file,
                              from = n_lines+1L - n.guessing.lines,
                              to = n_lines)
  hf <- list(header, footer)

  n_hf_lines <- vapply(seq_along(hf), function(i) {
    lines <- hf[[i]]

    is_data <- grepl(
      data.regex,
      lines
    )

    tgt_value <- switch(as.character(i),
                        "1" = 0L,
                        "2" = sum(is_data))
    add <- switch(as.character(i),
                  "1" = 0L,
                  "2" = -1L)
    sum(cumsum(is_data) == tgt_value) + add
  }, integer(1))

  n_hf_lines
}



#' @importFrom data.table fread
read_matching_table_rows <- function(
  file,
  pattern,
  ...
) {
  assert_file_path(file)
  stopifnot(
    is.character(pattern), length(pattern) == 1L, !is.na(pattern)
  )
  
  dir <- dir_of_path(file)
  file_without_dir <- sub(
    pattern = dir, replacement = "", x = file, fixed = TRUE
  )
  file_without_dir <- sub("^[\\/]+", "", file_without_dir)
  cmd <- paste0(
    "cd \"", dir,"\" && ",
    "findstr /r \"", pattern, "\" \"", file_without_dir, "\""
  )
  dt <- tryCatch(
    data.table::fread(cmd = cmd, ...),
    error = function(e) e,
    warning = function(w) w
  )
  if (inherits(dt, c("error", "warning"))) {
    lines <- readLines(file)
    do_read <- grepl(pattern, lines)
    dt <- data.table::fread(text = lines[do_read], ...)
  }
  
  return(dt[])
}



#' @importFrom data.table fread
read_table_without_header_and_footer <- function(
  file,
  n.header.lines = NULL,
  n.footer.lines = NULL,
  ...
) {
  requireNamespace("data.table")

  if (is.null(n.header.lines) || is.null(n.footer.lines)) {
    n_hf_lines <- guess_table_header_and_footer_sizes(file = file)
    if (is.null(n.header.lines)) {
      n.header.lines <- n_hf_lines[1]
    }
    if (is.null(n.footer.lines)) {
      n.footer.lines <- n_hf_lines[2]
    }
  }
  stopifnot(
    n.header.lines >= 0,
    length(n.header.lines) == 1,
    n.header.lines %% 1 == 0,
    n.footer.lines >= 0,
    length(n.footer.lines) == 1,
    n.footer.lines %% 1 == 0
  )
  assert_file_path(file)


  arg_list <- list(
    file = file,
    sep = ";", dec = ",", header = FALSE, skip = n.header.lines,
    nrow = n_file_lines(file) - n.header.lines - n.footer.lines,
    quote = '"',
    blank.lines.skip = TRUE
  )
  ddd <- list(...)
  arg_list[names(ddd)] <- ddd
  do.call(data.table::fread, arg_list)
}























