




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

assert_file_path <- function(path, path.arg.nm) {
  assert_path(path = path, path.arg.nm = path.arg.nm, path.type = "file")
}

assert_dir_path <- function(path, path.arg.nm) {
  assert_path(path = path, path.arg.nm = path.arg.nm, path.type = "dir")
}
















