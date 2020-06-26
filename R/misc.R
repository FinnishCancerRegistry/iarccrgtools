




ask_yes_no <- function(
  ...
) {
  stopifnot(
    vapply(list(...), is.character, logical(1))
  )

  message(
    paste0(
      paste0(..., collapse = ""),
      "\n",
      "1: yes\n",
      "2: no\n",
      "c: cancel"
    )
  )

  allowed_answers <- c("1", "2", "c")
  answer <- ""
  while (!answer %in% allowed_answers) {
    answer <- readline(prompt = ": ")
    answer <- tolower(answer)
  }

  switch(
    answer,
    "1" = TRUE,
    "2" = FALSE,
    c = stop("Cancelled by user.", call. = FALSE)
  )
}





ask_to_proceed <- function(
  query
) {
  if (!ask_yes_no(query)) {
    stop("Cancelled.", call. = FALSE)
  }
}




raise_internal_error <- function(
  ...
) {
  requireNamespace("utils")
  error_msg <- paste0(
    "Internal error: ",
    ...,
    " If you see ",
    "this, please complain to the package maintainer ",
    utils::maintainer("iarccrgtools"), "."
  )
  stop(error_msg, call. = FALSE)

}




#' @title IARC CRG Tools Settings
#' @name tools_settings_files
#' @description
#' IARC CRG Tools can use settings defined on an earlier run of one of its
#' programs. This page documents the relationship of these settings files
#' with this R package.
#'
#' @section Manual use:
#'
#' This mainly pertains to using \code{\link{use_tools_interactively}}.
#' When using IARC CRG Tools manually, settings files are not mandatory.
#' They just make your life easier. Inspect the help pages of IARC CRG Tools
#' for more information.
#'
#' @section Automatic use:
#'
#' This pertains to using \code{\link{use_tools_automatically}}.
#' You need to have a pre-defined settings file for the IARC CRG Tools program
#' you want to use before using \code{\link{use_tools_automatically}}.
#'
#' Each program must have its own settings file. The file extension will be
#' either .dfi or .frm. The name of the file must be the name of the program
#' (one of the items given by \code{\link{tools_program_names}}). E.g.
#' \code{"iarc_check.dfi"}. The settings files must be stored in the
#' working directory set by \code{\link{set_tools_working_dir}}.
#'
#' This R package has pre-defined "sensible defaults" for certain programs,
#' which you can fetch into a specific folder using
#' \code{\link{get_tools_settings_template}}.
#'

NULL





dir_of_path <- function(path) {
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





normalize_path <- function(path, double.slash = FALSE) {
  stopifnot(
    is.character(path)
  )
  assert_is_logical_nonNA_atom(double.slash)
  path <- normalizePath(path = path, winslash = "\\", mustWork = FALSE)
  path <- gsub("\\{2,}", "\\", path)

  if (double.slash) {
    path <- gsub("\\", "\\\\", path, fixed = TRUE)
  }

  is_dir <- dir.exists(path)
  path[is_dir] <- paste0(path, "\\")

  path

}





str_extract_all <- function(x, pattern, ...) {
  m <- gregexpr(pattern = pattern, text = x, ...)
  regmatches(x = x, m = m)
}
str_extract <- function(x, pattern, ...) {
  m <- regexpr(pattern = pattern, text = x, ...)
  regmatches(x = x, m = m)
}




file_ext <- function(file) {
  str_extract(file, pattern = "(?<=\\.)\\w{1,}$", perl = TRUE)
}





#' @title iarccrgtools: Using IARC CRG Tools via R
#' @name iarccrgtools
#' @docType package
#' @description
#' Contains functions to make using IARC CRG Tools easier with R,
#' including writing and reading IARC CRG Tools input/output files and
#' guided or automatic use of IARC CRG Tools itself.
#' @details
#'
#' Main attractions include
#' \code{\link{use_tools_automatically}} and
#' \code{\link{use_tools_interactively}} for calling IARC CRG Tools directly
#' from R and for helping with input/output from and to R.
#'
NULL






#' @md
#' @title Program Definitions
#' @description 
#' 
#' @format
#' A data.frame with these character string columns:
#' - `clean_name`: name of programme as understood by the functions in this 
#'   package
#' - `real_name`: name of programme in IARC CRG Tools menu
#' - `executable_name`: name of executable for corresponding programme
#' - `window_name`: name of executable's window in Windows
#' @family program_definition_data
"programs"

#' @md
#' @title Program Definitions
#' @description data.frame specifying output files for each
#' IARC CRG Tools program.
#' @format
#' A data.frame with these columns:
#' - `program_name`: character string column; name of the program
#' - `file_name_suffix`: character string column; suffix pasted to each output
#'   file name; in other words the output files are assumed to have these
#'   suffixes
#' - `is_table`: logical column; `TRUE` if the output file is a table,
#'   `FALSE` if it is non-tabular text (such as a log file)
#' @family program_definition_data
"program_output_files"

#' @md
#' @title Program Definitions
#' @description data.frame of specifications for columns used in various
#' functions
#' @format
#' A data.frame with character string columns
#' - `column_name` name of column
#' - `class` expected class of column when using functions in this package
#' - `info` short plain English explanation of column purpose and contents
#'
#' as well as a number of logical (TRUE/FALSE) columns. The names of these
#' logical columns follow the convention `"set_SETNAME"` where `SETNAME`
#' is one of the items returned by \code{\link{tools_program_colnameset_names}}.
#' Each such logical columns is `TRUE` when that column indicated in
#' `column_name` is included in that set of column names.
#'
#' @family program_definition_data
"column_specifications"




seconds_elapsed <- function(t) {
  stopifnot(
    inherits(t, "proc_time")
  )
  proc.time()["elapsed"] - t["elapsed"]
}




is_writable <- function(
  file.paths
) {


  vapply(file.paths, function(file_path) {
    assert_file_path(file_path)
    bat_lines <- c(
      "2>nul (",
      paste0("  >>", file_path, " (call )"),
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





wait_until_all_files_stop_growing <- function(
  file.paths,
  check.interval = 10, ## 30 sec
  max.wait.time = 60*30, ## 30 min
  initial.wait = 10L,
  wait.until.writable = FALSE,
  verbose = TRUE
) {
  lapply(file.paths, function(file_path) {
    assert_dir_path(dir_of_path(file_path))
  })
  stopifnot(
    length(check.interval) == 1,
    check.interval %% 1 == 0,
    check.interval > 0,
    length(max.wait.time) == 1,
    max.wait.time %% 1 == 0,
    max.wait.time > 0,
    max.wait.time > check.interval,
    length(initial.wait) == 1,
    initial.wait %% 1 == 0,
    initial.wait > 0
  )
  assert_is_logical_nonNA_atom(verbose)
  assert_is_logical_nonNA_atom(wait.until.writable)

  if (verbose) {
    message("* wait_until_all_files_stop_growing: waiting for ",
            initial.wait, " seconds before starting to inspect file sizes...")
  }
  Sys.sleep(initial.wait)

  tick <- 0L
  n_files <- length(file.paths)
  prev_file_sizes <- rep(-1L, n_files)
  file_sizes <- file.size(file.paths)
  file_sizes[is.na(file_sizes)] <- -1.0
  t <- proc.time()
  sec_elapsed <- 0L
  if (verbose) {
    message(
      "* wait_until_all_files_stop_growing: starting to wait for these ",
      "files:\n", paste0("   '", unname(file.paths), "'", collapse = "\n")
    )
  }

  file_grew <- rep(TRUE, length(file.paths))
  reached_max_time <- FALSE
  file_writable <- rep(FALSE, length(file.paths))
  while (any(file_grew | !file_writable) && !reached_max_time) {

    Sys.sleep(check.interval)
    sec_elapsed <- seconds_elapsed(t)
    prev_file_sizes <- file_sizes
    file_sizes <- file.size(file.paths)

    ## when file no longer found
    file_sizes[is.na(file_sizes)] <- -1.0

    file_grew <- prev_file_sizes < file_sizes
    reached_max_time <- sec_elapsed >= max.wait.time - check.interval
    file_writable <- if (wait.until.writable) is_writable(file.paths) else TRUE

    tick <- tick + 1L
    if (verbose) {
      msg <- paste0(
        "* wait_until_all_files_stop_growing: iteration ",tick, " done. ",
        "In total ", round(sec_elapsed), " seconds have elapsed."
      )
      if (any(file_grew)) {
        msg <- paste0(
          msg,
          "\n** These ",
          "files grew this iteration: \n",
          paste0("   '", file.paths[file_grew], "'", collapse = "\n")
        )
      } else {
        msg <- paste0(msg, "\n** No files grew since previous iteration. ")
      }
      if (wait.until.writable) {
        if (any(!file_writable)) {
          msg <- paste0(
            msg, "\n** These files are not yet writable:\n",
            paste0("   '",  file.paths[!file_writable], "'", collapse = "\n")
          )
        } else {
          msg <- paste0(msg, "\n** All files are now writable. ")
        }
      }

      message(msg)
    }

  }
  if (sec_elapsed > max.wait.time) {
    warning(
      "While waiting for files ", paste0("'", file.paths, "'", collapse = ", "),
      " to stop growing in disk space, ",
      "reached max.wait.time = ", max.wait.time, " before they all stopped ",
      "growing."
    )
  }

  if (verbose) {
    message("* wait_until_all_files_stop_growing: stopped waiting")
  }

  return(TRUE)
}





tools_program_definition_files <- function(
  colnameset.name
) {
  assert_tools_colnameset_name(colnameset.name)

  src_file_exts <- switch(
    colnameset.name,
    mandatory_icdo3_to_icd10 = "dfi",
    all_icdo3_to_icd10 = "dfi",
    all_iarc_check = "frm",
    all_iarc_multiple_primary = "mpr",
    raise_internal_error(
      "No source file location defined for colnameset.name = ",
      deparse(colnameset.name)
    )
  )
  src_files <- paste0("inst/", colnameset.name, ".", src_file_exts)
  src_files <- system.file(src_files, package = "iarccrgtools")
  src_files
}





group_indices <- function(x) {
  stopifnot(
    is.integer(x),
    x > 0,
    !is.na(x),
    identical(x, sort(x)),
    length(x) > 1
  )

  d <- diff(x)

  n_x <- length(x)

  grps <- rep(NA_integer_, n_x)
  grps[1] <- 1L
  current_grp <- 1L
  for (i in 2:n_x) {
    if (d[i-1L] > 1L) {
      current_grp <- current_grp + 1L
    }
    grps[i] <- current_grp

  }
  grps
}



run_executable <- function(exe_path) {
  assert_file_path(exe_path)
  
  dir <- dir_of_path(exe_path)
  
  bat_lines <- c(
    paste0("cd ", dir),
    basename(exe_path)
  )
  
  tf <- tempfile(pattern = "tmp_exe_call_", fileext = ".bat")
  
  on.exit({
    if (file.exists(tf)) file.remove(tf)
  })
  writeLines(bat_lines, tf)
  
  e <- environment()
  warn_fun <- function(w) {
    assign(x = "warn", value = w, envir = e)
  }
  warn <- list(message = "")
  suppressWarnings(withCallingHandlers(
    unused <- system2(
      command = tf,
      timeout = 1L,
      stdout = TRUE
    ),
    warning = warn_fun
  ))
  
  out <- FALSE
  if (grepl("timed out after 1s", warn[["message"]], fixed = TRUE)) {
    out <- TRUE
  } else {
    warning(simpleWarning(warn[["message"]], call = warn[["call"]]))
  }
  out
}



run_tools_executable <- function(exe.path = get_tools_exe_path()) {
  run_executable(exe.path)
}





#' @importFrom utils data
get_exported_dataset <- function(dataset.name) {
  stopifnot(
    length(dataset.name) == 1,
    is.character(dataset.name),
    !is.na(dataset.name)
  )

  expo_data_nms <- utils::data(package = "iarccrgtools")$results[, "Item"]
  if (!dataset.name %in% expo_data_nms) {
    raise_internal_error(
      "Requested exported dataset ",
      deparse(dataset.name), " is not one of ",
      deparse(expo_data_nms), ". "
    )
  }
  e <- new.env(parent = emptyenv())
  utils::data(list = dataset.name, envir = e)
  e[[dataset.name]]
}





get_internal_dataset <- function(dataset.name) {
  stopifnot(
    length(dataset.name) == 1,
    is.character(dataset.name),
    !is.na(dataset.name)
  )
  pkg_env <- as.environment("package:iarccrgtools")
  object_nms <- getNamespaceExports("iarccrgtools")

  dataset_nms <- object_nms[vapply(object_nms, function(object_nm) {
    is.data.frame(pkg_env[[object_nm]])
  }, logical(1))]

  if (!dataset.name %in% dataset_nms) {
    raise_internal_error(
      "Requested internal dataset ",
      deparse(dataset.name), " is not one of ",
      deparse(dataset_nms), ". "
    )
  }

  pkg_env[[dataset.name]]
}















