

filesystem_dir_of_path <- function(path) {
  ## - if is existing dir, return path
  ## - if is existing (non-directory) file path, return dir of this file
  ## - if looks like file path, return implied dir
  ## - else return path and throw warning
  stopifnot(
    length(path) == 1,
    is.character(path)
  )
  has_ext <- grepl(
    pattern = "\\.[[:alnum:]]{1,10}$",
    x = path
  )
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
  path[is_dir] <- paste0(path, "\\")

  path <- gsub("[\\/]+", "\\\\", path, fixed = FALSE)
  if (double.slash) {
    path <- gsub("[\\/]+", "\\\\\\\\", path, fixed = FALSE)
  }

  # e.g. \\\\solaris\\drive\\dir\\
  path <- ifelse(substr(path, 1, 1) == "\\", paste0("\\", path), path)

  path

}



filesystem_file_path_extension <- function(file) {
  str_extract(file, pattern = "(?<=\\.)\\w{1,}$", perl = TRUE)
}




filesystem_file_path_is_writable <- function(
    file.paths
) {
  vapply(file.paths, function(file_path) {
    assert_file_path(file_path)
    bat_lines <- c(
      "2>nul (",
      paste0("  >>\"", file_path, "\" (call )"),
      ") && (echo 1) || (echo 0)"
    )

    tf <- tempfile(pattern = "tmp_iarccrgtools_bat_",
                   fileext = ".bat")

    writeLines(bat_lines, tf)

    output <- system2(
      command = tf,
      stdout = TRUE,
      stderr = TRUE
    )
    output <- output[output %in% c("0", "1")]
    switch(
      output,
      "0" = FALSE,
      "1" = TRUE,
      raise_internal_error("could not determine writability of file ",
                           file_path)
    )
  }, logical(1))
}



filesystem_dir_path_is_writable <- function(
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
