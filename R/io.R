




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




get_data_template <- function(
  set.nm,
  n.rows = 10L
) {
  stopifnot(
    length(n.rows) == 1,
    n.rows %% 1 == 0,
    n.rows > 0
  )
  col_nms <- tools_program_colnameset(set.nm = set.nm)

  df <- data.frame(
    subject_id = "0000000001",
    record_id = "0000000001",
    multi_no = "001",
    sex = "1",
    icd9 = "0001",
    icd10 = "0001",
    icdo1_topography = "0001",
    icdo1_histology = "0001",
    icdo1_grade = "1",
    icdo2_topography = "001",
    icdo2_histology = "0001",
    icdo3_topography = "001",
    icdo3_histology = "0001",
    icdo3_behaviour = "1",
    icdo3_grade = "1",
    basis = "1",
    bi_date = "19501231",
    dg_date = "20001231",
    dg_age = "50"
  )


  df[rep(1L, n.rows), col_nms]
}





#' @name work_dir
#' @title IARC CRG Tools Working Directory
#' @description
#' Get and set directory where input and output files of IARC CRG Tools
#' are stored.
NULL

#' @describeIn work_dir sets working directory
#' @param dir string; path to a directory
#' @export
set_tools_working_dir <- function(dir) {
  assert_dir_path(dir)
  dir <- normalize_path(dir)
  assign(x = "path", value = dir, envir = wd_env)
}

#' @describeIn work_dir gets current working directory as string
#' @export
get_tools_working_dir <- function() {
  if (identical(wd_env$path, FALSE)) {
    stop("Working directory for IARC CRG Tools not set --- ",
         "see ?set_tools_working_dir")
  } else if (!dir.exists(wd_env$path)) {
    stop("Supplied IARC CRG Tools working directory does not exist: ",
         deparse(wd_env$path), "; see ?set_tools_working_dir")
  }
  wd_env$path
}
wd_env <- new.env(parent = emptyenv())
wd_env$path <- FALSE





#' @title Write File to Use as IARC CRG Tools Input
#' @description
#' A text file is written in a fixed field format.
#' @param x a data.frame
#' @template colnameset_name
#' @param file string; where file will be saved to
#' @param ... arguments passed to \code{\link[data.table]{fwrite}};
#' e.g. try \code{nThread = x} where \code{x} is a desired number of cores to
#' use when writing
#' @import data.table
#' @export
write_tools_data <- function(
  x,
  colnameset.nm = tools_program_colnameset_names()[1],
  file = tempfile(fileext = ".txt", tmpdir = get_tools_working_dir()),
  ...
) {
  assert_dataframe(x)
  col_nms <- tools_program_colnameset(colnameset.nm)
  assert_names(x, expected.names = col_nms, arg.nm = "x")
  assert_write_file_path(path = file)

  x <- data.table::setDF(mget(col_nms, as.environment(x)))

  ## to ensure that, when IARC CRG Tools writes a new column, when it is read
  ## into R, the new column will not mix with the last column in x.
  x[[".__this_is_a_buffer_column_yo__."]] <- 0L

  if (file.exists(file)) {
    ow <- ask_yes_no("* write_tools_data: File ", deparse(file),
                     " already exists. overwrite?")
    if (!ow) {
      message("* write_tools_data: Cancelled writing table to ",
              deparse(file), ".")
      return(invisible(NULL))
    }
  }

  data.table::fwrite(
    x = x,
    file = file,
    sep = ";",
    dec = ",",
    quote = TRUE,
    row.names = FALSE,
    col.names = FALSE,
    ...
  )
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





#' @title IARC CRG Tools Results
#' @description Read IARC CRG Tools results into R.
#' @template program_name
#' @export
read_tools_results <- function(
  program.name,
  input.col.nms = NULL
) {
  assert_tools_program(program.name = program.name)
  dir <- get_tools_working_dir()

  file_paths <- tools_program_output_file_paths(program.name = program.name,
                                                dir = dir)

  output_list <- lapply(seq_along(file_paths), function(i) {

    file_path <- file_paths[i]
    path_type <- names(file_paths)[i]
    message("* read_tools_results: attempting to read file from ",
            deparse(file_path))

    if (!file.exists(file_path)) {
      message("* read_tools_results: that file did not exist, returning NULL")
      return(NULL)
    }

    if (identical(path_type, "is_table")) {
      out <- read_table_without_header_and_footer(
        file = file_path
      )

      if (is.null(out) || nrow(out) == 0) {
        out <- data.frame(NULL)
      } else {
        ## in write_tools_data a zero is added as padding to enable safe
        ## reading into R. these steps handle this padding column.
        last_col <- names(out)[ncol(out)]
        if (is.character(last_col) && substr(out[[last_col]][1], 1, 2) == "0 ") {
          ## this happens when IARC CRG Tools adds a text column.
          out[[last_col]] <- substring(out[[last_col]], 3, 1e6L)
        } else if (all(out[["last_col"]] == 0L)) {
          out[[last_col]] <- NULL
        }

      if (!is.null(input.col.nms)) {

        if (ncol(out) == length(input.col.nms)+1L) {
          input.col.nms <- c(input.col.nms, "tools_text")
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
        "file_path = ", deparse(file_path), " file path type was not defined. "
      )
    }
    message("* read_tools_results: file successfully read ")


    out
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
  data.regex = "^\"\\d{1,}\";"
) {
  assert_file_path(file)
  stopifnot(
    length(n.guessing.lines) == 1,
    n.guessing.lines > 0
  )
  n_lines <- n_file_lines(file)
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





read_table_without_header_and_footer <- function(
  file,
  n.header.lines = NULL,
  n.footer.lines = NULL,
  ...
) {
  requireNamespace("data.table")

  n_lines <- n_file_lines(file)
  if (n_lines == 0) {
    return(data.frame(NULL))
  }

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
    n.header.lines >= 0, length(n.header.lines) == 1,
    n.header.lines %% 1 == 0,
    n.footer.lines >= 0, length(n.footer.lines) == 1,
    n.footer.lines %% 1 == 0
  )
  assert_file_path(file)


  arg_list <- list(
    file = file,
    sep = ";", dec = ",", header = FALSE, skip = n.header.lines,
    nrow = n_lines - n.header.lines - n.footer.lines,
    quote = '"'
  )
  ddd <- list(...)
  arg_list[names(ddd)] <- ddd
  do.call(data.table::fread, arg_list)
}























