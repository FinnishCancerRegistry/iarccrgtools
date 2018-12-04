




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
print.vbslines <- function(x, ...) {
  n_lines <- length(x)
  
  cat("--- vbslines vector with", n_lines, "lines ---\n")
  row_num <- seq_along(x)
  row_num <- formatC(x = row_num, digits = nchar(n_lines), flag = " ")
  
  cat(paste0(row_num, ": ", x), sep = "\n")
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
  tf <- normalizePath(tf)
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





can_call_vbs <- function() {

  stri <- "__%%__VERY_UNLIKELY_STRING_INDEED__%%__"
  stdout <- call_vbslines(vbslines_echo(string = stri))

  stri %in% stdout
}





vbslines_call_tools <- function(
  exe.path = get_path_to_exe()
) {
  assert_file_path(exe.path, path.arg.nm = "exe.path")
  exe.path <- normalizePath(exe.path)
  lines <- c(
    '',
    'Set WshShell = WScript.CreateObject("WScript.Shell")',
    paste0('WshShell.Run """', exe.path,'""", 9'),
    ''
  )
  as.vbslines(lines)
}



vbslines_get_filesize <- function(
  file.path
) {
  assert_file_path(file.path, "file.path")
  file.path <- normalizePath(file.path)
  lines <- c(
    '',
    'Set fso = CreateObject("Scripting.FileSystemObject")',
    paste0('Set fo = fso.GetFile("', file.path,'")'),
    'fosize = fo.Size',
    ''
  )
  as.vbslines(lines)
}





vbslines_wait_until_file_stops_growing <- function(
  file.path,
  check.interval = 30L,
  max.time = 60L*60L*12L ## 12 hours
) {
  assert_file_path(file.path, path.arg.nm = "file.path")
  file.path <- normalizePath(file.path)
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
    'Wscript.Echo("file stopped growing. ended.")',
    ''
  )

  as.vbslines(lines)
}





vbslines_tools_program_keystrokes <- function(
  program.name
) {
  assert_tools_program(program.name)
  keystrokes <- tools_program_keystrokes(program.name)
  specials <- list(CTRL = "^", ALT = "%", ENTER = "{ENTER}",
                   SHIFT = "+")
  
  lines <- keystrokes
  
  for (special_name in names(specials)) {
    lines <- gsub(
      paste0(special_name, " + "),
      specials[[special_name]],
      lines
    )
  }
  
  lines <- tolower(lines)
  lines <- paste0("WshShell.SendKeys(\"", lines, "\")")
  as.vbslines(lines)
}





vbslines_call_tools_program <- function(
  exe.path = get_path_to_exe(),
  working.dir = get_working_dir(),
  program.name = "check",
  wait.check.interval = 10L,
  wait.max.time = 60L*60L
) {
  
  vl_call_tools <- vbslines_call_tools(exe.path = exe.path)
  
  vl_keystrokes <- vbslines_tools_program_keystrokes(
    program.name = program.name
  )
  
  tgt_file_name <- tools_program_output_file_name(program.name = program.name)
  tgt_file_path <- paste0(working.dir, tgt_file_name)
  
  if (!file.exists(tgt_file_path)) {
    writeLines("", con = tgt_file_path)
  }
  
  vl_wait_until_finished <- vbslines_wait_until_file_stops_growing(
    file.path = tgt_file_path, 
    check.interval = wait.check.interval,
    max.time = wait.max.time
  )
  
  lines <- c(
    vl_call_tools,
    vl_keystrokes,
    vl_wait_until_finished,
    'wscript.shell.sendkeys("{ENTER}")'
  )
  as.vbslines(lines)
}









