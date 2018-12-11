




as.vbslines <- function(x, ...) {
  UseMethod("as.vbslines")
}
as.vbslines.default <- function(x, ...) {
  stop("No method defined for 'x' with classes: ",
       deparse(class(x)))
}
as.vbslines.character <- function(x, ...) {
  y <- as.character(x)
  class(y) <- c("vbslines" , "character")
  y
}
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
  as.vbslines(lines)
}





vbslines_tools_program_commands <- function(
  program.name,
  input.path,
  output.path
) {
  assert_tools_program(program.name)
  assert_write_file_path(input.path)
  assert_write_file_path(output.path)
  commands <- tools_program_commands(program.name)
  
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
  lines
  
  special_commands <- list(
    `%%WAIT_UNTIL_READY%%` = vbslines_wait_until_file_stops_growing(
      file.path = input.path
    )
  )
  
  lines <- paste0("WshShell.SendKeys(\"", lines, "\")")
  
  for (cmd_nm in names(special_commands)) {
    
    has_special <- grepl(
      pattern = cmd_nm,
      x = lines,
      fixed = TRUE
    )
    if (any(has_special)) {
      lines <- local({
        lines <- as.list(lines)
        lines[has_special] <- special_commands[cmd_nm]
        unlist(lines, use.names = FALSE)
      })
    }
  }
  
  lines <- c(vbslines_call_tools(), "", lines, "", vbslines_exit_tools())
  
  as.vbslines(lines)
}





vbslines_call_tools_program <- function(
  exe.path = get_tools_exe_path(),
  working.dir = get_tools_working_dir(),
  program.name = "iarc_check",
  wait.check.interval = 10L,
  wait.max.time = 60L*60L
) {
  
  
  vl_call_tools <- vbslines_call_tools(exe.path = exe.path)

  input_path <- paste0(get_tools_working_dir(), "\\", program.name, 
                       "_input.txt")
  output_path <- paste0(get_tools_working_dir(), "\\", program.name, 
                        "_output.txt")
  vl_commands <- vbslines_tools_program_commands(
    program.name = program.name,
    input.path = input_path,
    output.path = output_path
  )
  
  call_vbslines(vl_commands)

  TRUE
}









