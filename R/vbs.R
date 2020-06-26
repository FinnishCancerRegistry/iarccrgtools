



#' @md
#' @title Coercion to `vbslines`
#' @description
#' Coerce
#' @param x any R object
#' @param ... additional arguments passed to methods
#' @export
as.vbslines <- function(x, ...) {
  UseMethod("as.vbslines")
}
#' @describeIn as.vbslines default method --- only raises error
#' @export
as.vbslines.default <- function(x, ...) {
  stop("No method defined for 'x' with classes: ",
       deparse(class(x)))
}
#' @describeIn as.vbslines coerces character strings to class \code{vbslines}
#' @export
as.vbslines.character <- function(x, ...) {
  y <- as.character(x)
  class(y) <- c("vbslines" , "character")
  y
}
#' @export
print.vbslines <- function(x, max.print = 50, ...) {
  n_lines <- length(x)
  stopifnot(
    length(max.print) == 1,
    max.print %% 1 == 0,
    max.print > 0
  )

  max.print <- min(max.print, n_lines)

  printable <- rep(TRUE, n_lines)

  if (n_lines > max.print) {
    first_10 <- 1:10
    last_10 <- seq(n_lines, n_lines-9, -1)
    printable[-c(first_10, last_10)] <- FALSE
  }

  cat("--- vbslines vector with", n_lines, "lines ---\n")
  row_num <- which(printable)
  row_num <- formatC(x = row_num, digits = nchar(n_lines), flag = " ")

  if (n_lines > max.print) {
    cat(paste0(row_num[1:10], ": ", x[1:10]), sep = "\n")
    n_hidden_lines <- n_lines-20L
    cat("---", n_hidden_lines, "lines not shown ---\n")
    cat(paste0(row_num[11:20], ": ", x[11:20]), sep = "\n")
  } else {
    cat(paste0(row_num, ": ", x), sep = "\n")
  }
  cat("--- vbslines vector end ---\n")

  invisible(NULL)
}
#' @export
`[.vbslines` <- function(x, ...) {
  y <- NextMethod()
  as.vbslines(y)
}
#' @export
c.vbslines <- function(...) {
  y <- NextMethod()
  as.vbslines(y)
}





call_vbsfile <- function(file.path) {
  assert_file_path(file.path, "file.path")
  stdout <- system2(
    command = "cscript.exe",
    args = paste0("\"", file.path, "\""),
    stdout = TRUE,
    stderr = TRUE
  )
  stdout
}





write_vbsfile <- function(lines, file, ...) {
  stopifnot(
    length(file) == 1,
    is.character(file),
    grepl("\\.vbs", file)
  )

  writeLines(lines, con = file, ...)

}





call_vbslines <- function(lines) {

  tf <- tempfile(fileext = ".vbs")
  writeLines(text = "", con = tf)
  tf <- normalize_path(tf)
  on.exit({
    if (file.exists(tf)) {
      file.remove(tf)
    }
  })
  write_vbsfile(lines, file = tf)
  call_vbsfile(tf)
}






vbslines_echo <- function(
  string
) {
  stopifnot(
    length(string) == 1,
    is.character(string)
  )
  as.vbslines(paste0('Wscript.Echo("', string,'")'))
}





vbslines_set_focus_to_window <- function(
  window.name = "IARC/IACR Cancer Registry Tools"
) {
  stopifnot(
    length(window.name) == 1,
    is.character(window.name)
  )

  lines <- c(
    "",
    'Set FocusShell = WScript.CreateObject("WScript.Shell")',
    paste0("FocusShell.AppActivate(\"", window.name, "\")"),
    ""
  )
  as.vbslines(lines)
}




#' @title Test .vbs Capability
#' @description
#' Simply returns TRUE/FALSE depending on whether you can execute
#' .vbs scripts. The reason may be due to your system not supporting them
#' or that you don't have sufficient permissions.
#' @export
can_call_vbs <- function() {

  stri <- "__%%__VERY_UNLIKELY_STRING_INDEED__%%__"
  stdout <- call_vbslines(vbslines_echo(string = stri))

  stri %in% stdout
}





vbscript_protect_path <- function(
  path
) {

  protect <- grepl("\\s", path) & !(grepl('$""', path) & grepl('""^', path))

  path[protect] <- paste0('""', path[protect], '""')

  return(path)
}





vbslines_call_tools <- function(
  exe.path = get_tools_exe_path()
) {
  assert_file_path(exe.path, path.arg.nm = "exe.path")
  exe.path <- normalize_path(exe.path)
  exe.path <- vbscript_protect_path(exe.path)
  lines <- c(
    '',
    'Set WshShell = WScript.CreateObject("WScript.Shell")',
    'If WScript.Arguments.Length = 0 Then',
    'Set ObjShell = CreateObject("Shell.Application")',
    'ObjShell.ShellExecute "wscript.exe" _',
    ', """" & WScript.ScriptFullName & """ RunAsAdministrator", , "runas", 1',
    'WScript.Quit',
    'End if',
    paste0('WshShell.Run "', exe.path,'", 9'),
    ''
  )
  as.vbslines(lines)
}





vbslines_get_filesize <- function(
  file.path
) {
  assert_file_path(file.path, "file.path")
  file.path <- normalize_path(file.path)
  lines <- c(
    '',
    'Set fso = CreateObject("Scripting.FileSystemObject")',
    paste0('Set fo = fso.GetFile("', file.path,'")'),
    'fosize = fo.Size',
    'Wscript.Echo(fosize)',
    ''
  )
  as.vbslines(lines)
}





vbslines_wait_until_file_stops_growing <- function(
  file.path,
  check.interval = 30L,
  max.time = 60L*60L*12L ## 12 hours
) {
  assert_write_file_path(file.path, path.arg.nm = "file.path")
  file.path <- normalize_path(file.path)
  stopifnot(
    length(check.interval) == 1,
    check.interval > 0,
    is.integer(check.interval),
    length(max.time) == 1,
    max.time > 0,
    is.integer(max.time),
    max.time > check.interval
  )

  if (grepl("\\s", file.path)) {
    file.path <- paste0('""', file.path, '""')
  }

  lines <- c(
    '',
    'Set fso = CreateObject("Scripting.FileSystemObject")',
    paste0('Set fo = fso.GetFile("', file.path, '")'),
    'oldsize = 0',
    'newsize = 1',
    paste0('timeallowed = ', max.time - check.interval),
    'timeelapsed = 0',
    paste0('intervaltime = ', 1000L*check.interval),
    'While newsize > oldsize AND timeelapsed < timeallowed',
    '   oldsize = newsize',
    paste0('   WScript.Sleep(intervaltime)'),
    '   newsize = fo.Size',
    '   timeelapsed = timeelapsed + intervaltime',
    'Wend',
    ''
  )

  as.vbslines(lines)
}





vbslines_exit_tools <- function() {
  lines <- c("%F", "X")
  lines <- paste0("WshShell.SendKeys(\"", lines, "\")")
  lines <- c(
    "",
    'Set WshShell = WScript.CreateObject("WScript.Shell")',
    lines,
    ""
  )
  as.vbslines(lines)
}





tool_expr_list <- function(
  tool.name,
  input.path,
  output.path,
  verbose = TRUE
) {
  assert_tool(tool.name)
  assert_write_file_path(input.path)
  input.path <- normalize_path(input.path, double.slash = TRUE)
  assert_write_file_path(output.path)
  output.path <- normalize_path(output.path, double.slash = TRUE)

  commands <- tool_commands(tool.name)

  special_strings <- list(

    CTRL = "^",
    ALT = "%",
    ENTER = "{ENTER}",
    SHIFT = "+",
    TAB = "{TAB}",
    `%%WRITE_INPUT_PATH%%` = input.path,
    `%%WRITE_OUTPUT_PATH%%` = output.path
  )

  lines <- toupper(commands)

  for (nm in names(special_strings)) {
    pat <- paste0("(\\Q", nm, c("", " + "), "\\E)", collapse = "|")
    lines <- gsub(
      pattern = pat,
      replacement = special_strings[[nm]],
      x = lines
    )
  }

  wait_for_files <- tool_output_file_paths(tool.name = tool.name)
  wait_for_files <- unname(wait_for_files)

  r_cmd_pool <- list(
    `%%WAIT_UNTIL_READY%%` = substitute(wait_until_all_files_stop_growing(
      file.paths = FILE_PATHS,
      verbose = VERBOSE
    ), list(FILE_PATHS = wait_for_files, VERBOSE = verbose))
  )
  wh_r_cmds <- lapply(names(r_cmd_pool), function(r_cmd_nm) {
    wh <- which(grepl(
      pattern = r_cmd_nm,
      x = lines,
      fixed = TRUE
    ))
    if (sum(wh) == 0) {
      wh <- NULL
    }
    wh
  })

  expr_list <- lapply(lines, function(line) {
    if (line %in% names(r_cmd_pool)) {
      return(r_cmd_pool[[line]])
    }
    line
  })

  wh_char <- which(vapply(expr_list, is.character, logical(1)))
  wh_char_grps <- group_indices(wh_char)
  grps <- seq_along(expr_list)
  grps[setdiff(grps, wh_char)] <- grps[setdiff(grps, wh_char)] + max(grps)
  grps[wh_char] <- wh_char_grps
  grps <- as.integer(factor(grps, levels = unique(grps)))

  expr_list <- lapply(unique(grps), function(u_grp) {
    wh_in_grp <- which(grps == u_grp)
    if (is.character(expr_list[[wh_in_grp[1]]])) {
      lines <- unlist(expr_list[wh_in_grp])
      lines <- c(
        'Set WshShell = WScript.CreateObject("WScript.Shell")',
        as.vbslines(paste0("WshShell.SendKeys(\"", lines, "\")"))
      )
      as.vbslines(lines)
    } else {
      expr_list[[wh_in_grp]]
    }
  })


  focus_tools <- substitute(call_vbslines(vbslines_set_focus_to_window(
    FOCUS_TO
  )), list(FOCUS_TO = "IARC/IACR Cancer Registry Tools"))
  tool_win_nm <- tool_window_name(tool.name)
  focus_popup <- substitute(call_vbslines(vbslines_set_focus_to_window(
    FOCUS_TO
  )), list(FOCUS_TO = tool_win_nm))

  expr_list <- c(
    list(
      quote(run_tools_executable()),
      focus_tools
    ),
    expr_list,
    list(
      focus_popup,
      quote(call_vbslines(vbslines_exit_tools()))
    )
  )
  expr_list

}





call_tool <- function(
  tool.name = tool.name,
  tool.exe.path = get_tool_exe_path(),
  working.dir = get_tools_working_dir(),
  wait.check.interval = 30L,
  wait.max.time = 60L * 60L,
  verbose = TRUE
) {
  assert_is_logical_nonNA_atom(verbose)
  input_path <- paste0(get_tools_working_dir(), "\\", tool.name,
                       "_input.txt")
  output_path <- paste0(get_tools_working_dir(), "\\", tool.name,
                        "_output.txt")

  expr_list <- tool_expr_list(
    tool.name = tool.name,
    input.path = input_path,
    output.path = output_path,
    verbose = verbose
  )

  error_to_warning <- function(e) {
    warning(simpleWarning(
      paste0("CONVERTED ERROR TO WARNING: ", e$message),
      call = e$call
    ))
  }
  stop("TODO: just call an exe with the settings file and press OK after waiting.")
  unused <- lapply(expr_list, function(expr) {
    if (verbose) {
      message("* call_tool: executing this: ")
    }
    if (is.language(expr)) {
      if (verbose) {
        expr_stri <- paste0("   ", deparse(expr), collapse = "\n")
        message(expr_stri)
      }
      tryCatch(
        eval(expr),
        error = error_to_warning
      )
    } else if (inherits(expr, "vbslines")) {
      if (verbose) {
        message(paste0("   ", expr, collapse = "\n"))
      }
      tryCatch(
        call_vbslines(expr),
        error = error_to_warning
      )
    } else {
      raise_internal_error("expr was not language nor vbslines object")
    }
  })

  TRUE
}









