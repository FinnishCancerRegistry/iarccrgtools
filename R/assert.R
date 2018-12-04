




assert_dataframe <- function(x, arg.nm = NULL) {
  if (is.null(arg.nm)) {
    arg.nm <- deparse(substitute(x))
  }
  if (!is.data.frame(x)) {
    stop("Object passed to argument ", deparse(arg.nm), " is not a data.frame")
  }
  invisible(NULL)
}





assert_names <- function(
  x,
  expected.names,
  arg.nm = NULL
) {
  if (is.null(arg.nm)) {
    arg.nm <- deparse(substitute(x))
  }
  if (length(names(x)) == 0) {
    stop("Object supplied to ", deparse(arg.nm), " has no names defined.")
  }

  miss_nms <- setdiff(names(x), expected.names)
  if (length(miss_nms)) {
    stop("Object passed to argument ", deparse(arg.nm), " did not have these ",
         "names: ", paste0(deparse(miss_nms), collapse = ""))
  }
  invisible(NULL)
}




assert_path <- function(
  path,
  path.arg.nm = NULL,
  path.type = c("dir", "file")[1]
) {

  if (is.null(path.arg.nm)) {
    path.arg.nm <- deparse(substitute(path))
  }
  if (length(path) != 1) {
    stop("Argument ", deparse(path.arg.nm), " must of length one; was of ",
         "length ", length(path))
  }
  if (!is.character(path)) {
    stop("Argument ", deparse(path.arg.nm), " is not a character string")
  }
  if (is.na(path)) {
    stop("Argument ", deparse(path.arg.nm), " has value NA")
  }

  test_fun <- switch(path.type, dir = dir.exists, file = file.exists)
  if (!test_fun(path)) {
    stop("No such ", path.type, ": ", deparse(path))
  }

  invisible(NULL)
}

assert_file_path <- function(path, path.arg.nm = NULL) {
  if (is.null(path.arg.nm)) {
    path.arg.nm <- deparse(substitute(path))
  }
  assert_path(path = path, path.arg.nm = path.arg.nm, path.type = "file")
}

assert_dir_path <- function(path, path.arg.nm = NULL) {
  if (is.null(path.arg.nm)) {
    path.arg.nm <- deparse(substitute(path))
  }
  assert_path(path = path, path.arg.nm = path.arg.nm, path.type = "dir")
}

assert_write_dir_path <- function(path, path.arg.nm = NULL) {
  if (is.null(path.arg.nm)) {
    path.arg.nm <- deparse(substitute(path))
  }
  assert_path(path = path, path.arg.nm = path.arg.nm, path.type = "dir")
  if (!dir_is_writable(path)) {
    stop("Directory ", deparse(path), " exists but is not writable; ensure ",
         "you have writing permissions there.")
  }
  invisible(NULL)
}





assert_tools_program <- function(program.name, program.name.arg.nm = NULL) {
  if (is.null(program.name.arg.nm)) {
    program.name.arg.nm <- deparse(substitute(program.name))
  }

  if (!is.character(program.name)) {
    stop("Arg ", deparse(program.name.arg.nm), " must be of class 'character'")
  }
  if (length(program.name) != 1L) {
    stop("Arg ", deparse(program.name.arg.nm), " must be of length 1")
  }
  prog_nms <- tools_program_names()
  if (!program.name %in% prog_nms) {
    stop("Arg ", deparse(program.name.arg.nm), " must be one of these: ",
         deparse(prog_nms))
  }
  invisible(NULL)
}




assert_tools_data <- function(
  data,
  program.name,
  data.arg.nm = NULL,
  program.name.arg.nm = NULL
) {
  if (is.null(data.arg.nm)) {
    data.arg.nm <- deparse(substitute(data))
  }
  if (is.null(program.name.arg.nm)) {
    program.name.arg.nm <- deparse(substitute(program.name))
  }

  if (!is.data.frame(data)) {
    stop("Arg ", deparse(data.arg.nm), " must be a data.frame")
  }
  if (nrow(data) == 0) {
    stop("Arg ", deparse(data.arg.nm), " must have at least one row")
  }
  if (!is.data.frame(data)) {
    stop("Arg ", deparse(data.arg.nm), " must be a data.frame")
  }

  mandatory_col_nms <- tools_program_colnameset(
    paste0("mandatory_", program.name)
  )
  miss_col_nms <- setdiff(mandatory_col_nms, names(data))
  if (length(miss_col_nms)) {
    stop("To use with ", program.name.arg.nm, " = ", deparse(program.name),
         ", data.frame passed to arg ", deparse(data.arg.nm), " must have ",
         "these columns: ", deparse(miss_col_nms))
  }
  invisible(NULL)
}






assert_tools_colnameset_name <- function(set.nm, set.nm.arg.nm = NULL) {
  if (is.null(set.nm.arg.nm)) {
    set.nm.arg.nm <- deparse(substitute(set.nm))
  }
  allowed <- tools_program_colnameset_names()

  if (length(set.nm) != 1) {
    stop("Arg ", deparse(set.nm.arg.nm), " must be of length 1")
  }
  if (!is.character(set.nm)) {
    stop("Arg ", deparse(set.nm.arg.nm), " must be of class 'character'")
  }
  if (!set.nm %in% allowed) {
    stop("Arg ", deparse(set.nm.arg.nm), " must be one of the following: ",
         deparse(allowed), "; Got instead: ", deparse(set.nm))
  }
  invisible(NULL)
}








