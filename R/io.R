
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
#' @param fwrite_arg_list `[NULL, list]`
#'
#' Passed to [fwf::fwf_write] arg `fwrite_arg_list`. The following defaults are
#' internally:
#' - `sep = ";"`
#' - `dec = ","`
#' - `quote = FALSE`
#' - `row.names = FALSE`
#' - `col.names = FALSE`
#'
#' @export
iarc_input_write <- function(
  x,
  colnameset.name = tool_colnameset_names()[1],
  file = tempfile(fileext = ".txt", tmpdir = iarccrgtools::iarc_workdir_get()),
  overwrite = NULL,
  verbose = FALSE,
  fwrite_arg_list = NULL
) {
  assert_is_logical_nonNA_atom(verbose)
  assert_dataframe(x)
  col_nms <- iarccrgtools::tool_colnameset(colnameset.name)
  assert_names(x, expected.names = col_nms, arg.nm = "x")
  assert_write_file_path(path = file)

  if (file.exists(file) && is.null(overwrite)) {
    if (!interactive()) {
      stop("* iarc_input_write: File ", deparse(file), " already existed ",
           "so aborted. see ?iarc_input_write")
    }
    ow <- ask_yes_no("* iarc_input_write: File ", deparse(file),
                     " already exists. overwrite?")
    if (!ow) {
      message("* iarccrgtools::iarc_input_write: Cancelled writing table to ",
              deparse(file), ".")
      return(invisible(NULL))
    }
  }

  if (verbose) {
    message(
      "* iarccrgtools::iarc_input_write: collecting and transforming data..."
    )
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
    message("* iarccrgtools::iarc_input_write: ready to write to disk; ",
            "first five rows of current dataset:")
    print(utils::head(x))
  }

  if (verbose) {
    message("* iarccrgtools::iarc_input_write: writing...")
    t_write <- proc.time()
  }

  expected_widths <- iarccrgtools::tool_column_fwf_widths(
    setdiff(names(x), ".__this_is_a_buffer_column_yo__.")
  )
  expected_widths <- c(expected_widths, 0L)
  observed_max_widths <- vapply(x, function(col) max(nchar(col)), integer(1L))
  names(expected_widths) <- names(x)
  names(observed_max_widths) <- names(x)
  lapply(names(x), function(col_nm) {
    if (observed_max_widths[col_nm] > expected_widths[col_nm]) {
      stop("column ", deparse(col_nm), " is wider (has more characters; ",
           "see ?nchar) than expected: expected ", expected_widths[col_nm],
           " but the longest value as a string in the column was of length ",
           observed_max_widths[col_nm],
           ". as an example of this misspecification, the column 'basis' must ",
           "only have single-digit values, and if your data contains multi-",
           "digit values for 'basis', then column 'basis' would cause the ",
           "current error. (the actual violating column was given above ",
           "and may or may not be 'basis'). please modify your column ",
           deparse(col_nm), " and try again.")
    }
    NULL
  })

  user_fwrite_arg_list <- as.list(fwrite_arg_list)
  fwrite_arg_list <- list(
    sep = ";",
    dec = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE
  )
  fwrite_arg_list[names(user_fwrite_arg_list)] <- user_fwrite_arg_list
  fwf::fwf_write(
    x = x,
    path = file,
    widths = expected_widths,
    fwrite_arg_list = fwrite_arg_list
  )
  if (verbose) {
    message(
      "* iarccrgtools::iarc_input_write: done writing; ",
      data.table::timetaken(t_write)
    )
  }
  invisible(NULL)
}

#' @describeIn iarc_input_write Deprecated --- use
#' `iarccrgtools::iarc_input_write`.
#' @export
write_tools_data <- function(
  x,
  colnameset.name = tool_colnameset_names()[1],
  file = tempfile(fileext = ".txt", tmpdir = iarccrgtools::iarc_workdir_get()),
  overwrite = NULL,
  verbose = FALSE,
  fwrite_arg_list = NULL
) {
  stop("Deprecated --- use iarccrgtools::iarc_input_write")
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
#' @title IARC CRG Tools Output
#' @description Read IARC CRG Tools output into R.
#' @template tool_name
#' @param hash `[character]` (no default)
#' Hash of input dataset to read into R. See `[iarccrgtools::cache_hash]`.
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
#' @export
iarc_output_read <- function(
  tool.name,
  hash,
  input.col.nms = NULL,
  verbose = TRUE
) {
  assert_tool(tool.name = tool.name)
  dir <- iarc_toolworkdir_get(tool.name = tool.name, hash = hash)

  file_paths <- tool_output_file_paths(tool.name = tool.name, dir = dir)

  output_list <- lapply(seq_along(file_paths), function(i) {

    file_path <- file_paths[i]
    path_type <- names(file_paths)[i]

    if (verbose) {
      message(
        "* iarccrgtools::iarc_output_read: attempting to read file from ",
        deparse(unname(file_path))
      )
    }

    if (!file.exists(file_path)) {
      if (verbose) {
        message("* iarccrgtools::iarc_output_read: file '", file_path,
                "' did not exist; ",
                "returning NULL")
      }
      return(NULL)
    }
    n_lines <- n_file_lines(path = file_path)
    if (n_lines == 0) {
      if (verbose) {
        message("* iarccrgtools::iarc_output_read: file '", file_path,
                "' had zero rows; ",
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
      message("* iarccrgtools::iarc_output_read: file successfully read ")
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

#' @describeIn iarc_output_read Deprecated --- use
#' `iarccrgtools::iarc_output_read`.
#' @export
read_tools_results <- function(
  tool.name,
  hash,
  input.col.nms = NULL,
  verbose = TRUE
) {
  stop("Deprecated --- use iarccrgtools::iarc_output_read")
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
  row_no <- 0L
  while (row_no < from - 1L) {
    readLines(file_con, n = 1L, ...)
    row <- row + 1L
  }

  lines <- readLines(file_con, n = to + 1L - from)
  close(file_con)
  lines
}


read_matching_table_rows <- function(
  file,
  pattern,
  ...
) {
  assert_file_path(file)
  stopifnot(
    is.character(pattern), length(pattern) == 1L, !is.na(pattern)
  )

  dir <- filesystem_dir_of_path(file)
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
