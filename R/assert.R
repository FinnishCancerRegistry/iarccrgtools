




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

  miss_nms <- setdiff(expected.names, names(x))
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
  if (!filesystem_dir_path_is_writable(path)) {
    stop("Directory ", deparse(path), " exists but is not writable; ensure ",
         "you have writing permissions there.")
  }
  invisible(NULL)
}
assert_write_file_path <- function(path, path.arg.nm = NULL) {
  if (is.null(path.arg.nm)) {
    path.arg.nm <- deparse(substitute(path))
  }
  implied_dir <- dirname(path)
  assert_write_dir_path(implied_dir, path.arg.nm = path.arg.nm)
}





assert_tool <- function(tool.name, tool.name.arg.nm = NULL) {
  if (is.null(tool.name.arg.nm)) {
    tool.name.arg.nm <- deparse(substitute(tool.name))
  }

  if (!is.character(tool.name)) {
    stop("Arg ", deparse(tool.name.arg.nm), " must be of class 'character'")
  }
  if (length(tool.name) != 1L) {
    stop("Arg ", deparse(tool.name.arg.nm), " must be of length 1")
  }
  tool_nms <- tool_clean_names()
  if (!tool.name %in% tool_nms) {
    stop("Arg ", deparse(tool.name.arg.nm), " must be one of these: ",
         deparse(tool_nms))
  }
  invisible(NULL)
}




assert_tools_data <- function(
  data,
  tool.name,
  data.arg.nm = NULL,
  tool.name.arg.nm = NULL
) {
  if (is.null(data.arg.nm)) {
    data.arg.nm <- deparse(substitute(data))
  }
  if (is.null(tool.name.arg.nm)) {
    tool.name.arg.nm <- deparse(substitute(tool.name))
  }

  if (!is.data.frame(data)) {
    stop("Arg ", deparse(data.arg.nm), " must be a data.frame")
  }
  if (nrow(data) == 0) {
    stop("Arg ", deparse(data.arg.nm), " must have at least one row")
  }

  mandatory_col_nms <- iarccrgtools::tool_colnameset(
    paste0("mandatory_", tool.name)
  )
  miss_col_nms <- setdiff(mandatory_col_nms, names(data))
  if (length(miss_col_nms)) {
    stop("To use with ", tool.name.arg.nm, " = ", deparse(tool.name),
         ", data.frame passed to arg ", deparse(data.arg.nm), " must have ",
         "these columns: ", deparse(miss_col_nms))
  }

  col_nms <- intersect(iarccrgtools::tool_colnameset(paste0("all_", tool.name)), names(data))
  col_classes <- iarccrgtools::tool_column_classes(col_nms)
  lapply(seq_along(col_nms), function(col_no) {
    col_nm <- col_nms[col_no]
    col_class <- col_classes[col_no]
    has_class <- inherits(data[[col_nm]], col_class)
    if (!has_class) {
      stop("column ", deparse(col_nm), " was expected to have class ",
           deparse(col_class), "; instead it had class(es) ",
           deparse(class(data[[col_nm]])))
    }
    NULL
  })

  invisible(NULL)
}






assert_tools_colnameset_name <- function(colnameset.name, colnameset.name.arg.nm = NULL) {
  if (is.null(colnameset.name.arg.nm)) {
    colnameset.name.arg.nm <- deparse(substitute(colnameset.name))
  }
  allowed <- iarccrgtools::tool_colnameset_names()

  if (length(colnameset.name) != 1) {
    stop("Arg ", deparse(colnameset.name.arg.nm), " must be of length 1")
  }
  if (!is.character(colnameset.name)) {
    stop("Arg ", deparse(colnameset.name.arg.nm), " must be of class 'character'")
  }
  if (!colnameset.name %in% allowed) {
    stop("Arg ", deparse(colnameset.name.arg.nm), " must be one of the following: ",
         deparse(allowed), "; Got instead: ", deparse(colnameset.name))
  }
  invisible(NULL)
}




assert_is_logical_nonNA_atom <- function(arg, arg.nm = NULL) {
  if (is.null(arg.nm)) {
    arg.nm <- deparse(substitute(arg))
  }

  pass <- TRUE
  m <- paste0("Arg ", deparse(arg.nm), " was %%BAD&&, but expected %%GOOD%%")
  if (!is.logical(arg)) {
    pass <- FALSE
    m <- sub("%%BAD%%", paste0("of class(es) ", deparse(class(arg))), m)
    m <- sub("%%GOOD%%", "class \"logical\"", m)
  }
  if (length(arg) != 1) {
    pass <- FALSE
    m <- sub("%%BAD%%", paste0("of length ", length(arg)), m)
    m <- sub("%%GOOD%%", "length 1", m)
  }
  if (is.na(arg)) {
    pass <- FALSE
    m <- sub("%%BAD%%", paste0("NA ", length(arg)), m)
    m <- sub("%%GOOD%%", "TRUE/FALSE", m)
  }
  if (!pass) {
    stop(m)
  }
  invisible(NULL)
}



assert_is_character_nonNA_atom <- function(arg, arg.nm = NULL) {
  if (is.null(arg.nm)) {
    arg.nm <- deparse(substitute(arg))
  }

  pass <- TRUE
  m <- paste0("Arg ", deparse(arg.nm), " was %%BAD&&, but expected %%GOOD%%")
  if (!is.character(arg)) {
    pass <- FALSE
    m <- sub("%%BAD%%", paste0("of class(es) ", deparse(class(arg))), m)
    m <- sub("%%GOOD%%", "class \"character\"", m)
  }
  if (length(arg) != 1) {
    pass <- FALSE
    m <- sub("%%BAD%%", paste0("of length ", length(arg)), m)
    m <- sub("%%GOOD%%", "length 1", m)
  }
  if (is.na(arg)) {
    pass <- FALSE
    m <- sub("%%BAD%%", paste0("NA ", length(arg)), m)
    m <- sub("%%GOOD%%", "TRUE/FALSE", m)
  }
  if (!pass) {
    stop(m)
  }
  invisible(NULL)
}

















