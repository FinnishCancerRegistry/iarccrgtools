




get_data_template <- function(
  program
) {

  df <- data.frame(
    subject_id = "0000000001",
    record_id = "0000000001",
    multi_no = "001",
    sex = "1",
    icd9 = "0001",
    icd10 = "0001",
    icdo1_topo = "0001",
    icdo1_morpho = "0001",
    icdo1_grade = "1",
    icdo2_topo = "001",
    icdo2_morpho = "0001",
    icdo3_topo = "001",
    icdo3_morpho = "0001",
    icdo3_beh = "1",
    icdo3_grade = "1",
    dg_basis = "1",
    bi_date = "20001231",
    dg_date = "20001231",
    dg_age = 21:25
  )

  df
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
set_working_dir <- function(dir = tempdir()) {
  assert_dir_path(dir)
  assign(x = "path", value = dir, envir = wd_env)
}

#' @describeIn work_dir gets current working directory as string
#' @export
get_working_dir <- function() {
  wd_env$path
}
wd_env <- new.env(parent = emptyenv())
wd_env$path <- tempdir()





#' @title Write File to Use as IARC CRG Tools Input
#' @description
#' A text file is written in a fixed field format.
#' @param x a data.frame
#' @param file string; where file will be saved to
#' @param ... arguments passed to \code{\link[data.table]{fwrite}};
#' e.g. try \code{nThread = x} where \code{x} is a desired number of cores to
#' use when writing
#' @import data.table
#' @export
iarccrgtools_write <- function(
  x,
  file = tempfile(fileext = ".txt", tmpdir = get_working_dir()),
  ...
) {
  assert_names(x, expected.names = iarccrgtools_colnameset(), arg.nm = "x")
  assert_dataframe(x)

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

}





## TODO: rewrite these


iarccrgtools <- function(
  x,
  program = iarccrgtools_programs()[1],
  tmp.file = paste0(get_working_dir(), "iarc_check_input.txt"),
  clean = TRUE
) {
  program <- match.arg(program, iarccrgtools_programs())
  stopifnot(
    clean %in% c(TRUE, FALSE), length(clean) == 1
  )

  dt <- x

  iarccrgtools_write(x = dt, file = tmp.file)
  if (clean) {
    on.exit(file.remove(tmp.file))
  }
  message("* wrote file to use as input in IARC check to ", deparse(tmp.file))

  expected_file_nms <- iarccrgtools_program_result_files(program = program)
  message("* you now need to run the '", program,"' program in ",
          "IARC CRG Tools manually. ",
          "This function expects ",
          "the output files to look like this: ",
          deparse(expected_file_nms), ". So make sure they are actually ",
          "created by IARC CRG Tools.")

  ask_to_proceed(paste0(
    "* when IARC check has done the checks, type 'y' here without quotes to ",
    "proceed or 'n' to cancel. (Next the result files will be read into R.)"
  ))

  if (clean) {
    on.exit({
      message("* clean = TRUE, so removing tmp.file and output.files.")
      file.remove(c(tmp.file, expected_file_nms))
    })
  }
  switch(
    program,
    check = iarccrgtools_read_check(files = expected_file_nms),
    multi = iarccrgtools_read_multi(files = expected_file_nms),
    O3to10 = iarccrgtools_read_O3to10(files = expected_file_nms),
    stop("No read function defined for program = ", deparse(program))
  )

}





read_table_without_header_and_footer <- function(
  file,
  ...,
  n.header.lines = 0,
  n.footer.lines = 0
) {
  stopifnot(
    n.header.lines >= 0, length(n.header.lines) == 1,
    n.footer.lines >= 0, length(n.footer.lines) == 1
  )

  if (n.header.lines + n.footer.lines == 0) {
    return(data.table::fread(file = file, ...))
  }

  tmp <- tempfile(fileext = ".txt")
  on.exit(file.remove(tmp))

  lines <- readLines(con = file)
  n_lines <- length(lines)
  wh_drop <- c(seq_len(n.header.lines), n_lines+1 - seq_len(n.footer.lines))
  lines <- lines[-wh_drop]
  if (lines[length(lines)] != "") {
    lines <- c(lines, "")
  }

  writeLines(lines, con = tmp)

  return(data.table::fread(file = tmp, ...))
}





iarccrgtools_read_table <- function(
  file,
  col.names = c("id", "multi", "bi_date", "dg_date", "sex",
                "topo", "morpho", "beh", "iarc_check_msg"),
  n.header.lines = 0,
  n.footer.lines = 0
) {
  read_table_without_header_and_footer(
    file = file,
    n.header.lines = n.header.lines,
    n.footer.lines = n.footer.lines,
    sep = ";", blank.lines.skip = TRUE,
    col.names = col.names
  )
}





iarccrgtools_file_exts <- function() {
  c("log", "txt", "mul", "exl", "err", "chk", "eO3to10", "wO3to10")
}
iarccrgtools_file_ext_regex <- function() {
  exts <- iarccrgtools_file_exts()
  ext_pat <- paste0("(?<=(\\.))", paste0("(", exts, ")", collapse = "|"), "$")
  ext_pat
}
iarccrgtools_file_ext_extract <- function(files) {
  strextract(x = files, pattern = iarccrgtools_file_ext_regex(), perl = TRUE)
}

strextract_file_ext <- function(files) {
  strextract(x = files, pattern = "(?<=\\.).{1,}$", perl = TRUE)
}





iarccrgtools_get_read_fun <- function(
  ext
) {
  ext <- match.arg(ext, iarccrgtools_file_exts())
  if (ext == "log") {

    return(function(file, ...) readLines(con = file, ...))

  } else {

    lens <- switch(
      ext,
      txt = c(0, 0),
      mul = c(11, 3),
      exl = c(2, 1),
      err = c(4, 2),
      chk = c(4, 2),
      eO3to10 = c(3, 1),
      wO3to10 = c(3, 1)
    )

    return(function(file, ...) {
      iarccrgtools_read_table(
        file = file,
        n.header.lines = lens[1],
        n.footer.lines = lens[2],
        ...
      )
    })

  }
}





iarccrgtools_read_file <- function(
  file,
  ...
) {
  stopifnot(
    length(file) == 1,
    is.character(file)
  )

  file_ext <- strextract_file_ext(file)

  read_fun <- iarccrgtools_get_read_fun(ext = file_ext)
  return(read_fun(file = file, ...))

}





iarccrgtools_read_all_files <- function(
  files,
  ...
) {

  exts <- strextract_file_ext(files)
  names(files) <- exts

  check_results <- lapply(exts, function(ext) {
    file <- unname(files[ext])
    if (!file.exists(file)) {
      warning("No such file: ", deparse(file), "; returning NULL for element ",
              deparse(ext), " in output.")
      return(NULL)
    }
    iarccrgtools_read_file(file = file, ...)
  })

  names(check_results) <- exts
  check_results
}





iarccrgtools_programs <- function() {
  c("check", "multi", "O3to10")
}





iarccrgtools_program_result_files <- function(
  program = iarccrgtools_programs()[1]
) {
  program <- match.arg(program, iarccrgtools_programs())
  out <- switch(
    program,
    check = paste0(
      get_working_dir(),
      c("iarc_check_output.log", "iarc_check_output.txt",
        "iarc_check_input.chk", "iarc_check_input.err")
    ),
    multi = paste0(
      get_working_dir(),
      c("iarc_check_output.txt", "iarc_check_input.exl", "iarc_check_input.mul")
    ),
    O3to10 = paste0(
      get_working_dir(),
      c("iarc_check_output.txt",
        "iarc_check_input.eO3to10", "iarc_check_input.wO3to10")
    ),
    stop("No such value for 'program' defined: ", deparse(program))
  )
  out
}





iarccrgtools_read_multi <- function(
  files = iarccrgtools_program_result_files(program = "multi")
) {
  stopifnot(
    length(files) == 3,
    sum(grepl("\\.txt$", files)) == 1,
    sum(grepl("\\.exl$", files)) == 1,
    sum(grepl("\\.mul$", files)) == 1
  )
  iarccrgtools_read_all_files(files = files)
}





iarccrgtools_read_check <- function(
  files = iarccrgtools_program_result_files(program = "check")
) {
  stopifnot(
    is.character(files),
    length(files) == 4,
    sum(grepl("\\.txt$", files)) == 1,
    sum(grepl("\\.log$", files)) == 1,
    sum(grepl("\\.err$", files)) == 1,
    sum(grepl("\\.chk$", files)) == 1
  )
  iarccrgtools_read_all_files(files = files)
}





iarccrgtools_read_O3to10 <- function(
  files = iarccrgtools_program_result_files(program = "O3to10")
) {
  stopifnot(
    is.character(files),
    length(files) == 3,
    sum(grepl("\\.txt$", files)) == 1,
    sum(grepl("\\.eO3to10$", files)) == 1,
    sum(grepl("\\.wO3to10$", files)) == 1
  )
  iarccrgtools_read_all_files(files = files)
}




















