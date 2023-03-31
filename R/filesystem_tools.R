filesystem_dir_of_path <- function(path) {
  ## - if is existing dir, return path
  ## - if is existing (non-directory) file path, return dir of this file
  ## - if looks like file path, return implied dir
  ## - else return path and throw warning
  stopifnot(
    length(path) == 1,
    is.character(path)
  )
  has_ext <- filesystem_file_path_extension(path) != ""
  if (dir.exists(path)) {
    return(path)
  } else if (file.exists(path) || has_ext) {
    return(dirname(path))
  }
  warning("Could not guess directory for path ", deparse(path), "; ",
          "you may encounter a strange error. If path is a file, ensure it ",
          "ends with an extension such as .txt. If path is a directory, ",
          "ensure that it exists.")

}

filesystem_path_normalise <- function(path, double.slash = FALSE) {
  stopifnot(
    is.character(path)
  )
  assert_is_logical_nonNA_atom(double.slash)
  path <- normalizePath(path = path, winslash = "\\", mustWork = FALSE)

  is_dir <- dir.exists(path)
  path[is_dir] <- paste0(path[is_dir], "\\")

  path <- gsub("[\\/]+", "\\\\", path, fixed = FALSE)
  if (double.slash) {
    path <- gsub("[\\/]+", "\\\\\\\\", path, fixed = FALSE)
  }

  path <- ifelse(substr(path, 1, 1) == "\\", paste0("\\", path), path)
  path

}

filesystem_file_path_extension <- function(file) {
  out <- str_extract(file, pattern = "(?<=\\.)\\w{1,}$", perl = TRUE)
  if (identical(out, character(0L))) {
    out <- ""
  }
  return(out)
}

filesystem_dir_path_is_writable <- function(
    dir.path
) {
  assert_dir_path(dir.path)

  tf <- tempfile(pattern = "file_", tmpdir = dir.path, fileext = ".tmp")
  on.exit({
    if (file.exists(tf)) {
      unlink(tf, force = TRUE)
    }
  })

  test <- tryCatch(
    expr = {
      writeLines("test string", con = tf)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  return(test)
}
