




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
  paste0('Wscript.Echo("', string,'")')
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
  lines
}




vbslines_wait_until_file_stops_growing <- function(
  file.path,
  check.interval = 30L,
  max.time = 60L*60L*12L ## 12 hours
) {
  stop(
    "Not working when called --- got this error: ",
    "(4, 1) Microsoft VBScript runtime error: Object required: '[number: 0]'"
  )
  assert_file_path(file.path, path.arg.nm = "file.path")
  file.path <- normalizePath(file.path)
  stopifnot(
    length(check.interval) == 1,
    check.interval > 0,
    is.integer(check.interval),
    length(max.time) == 1,
    max.time > 0,
    is.integer(max.time)
  )

  lines <- c(
    '',
    'dim oldsize, newsize, filesys, fileobj',
    'Set filesys = CreateObject("Scripting.FileSystemObject")',
    'Set oldsize = 0',
    'Set newsize = 1',
    'While oldsize < newsize',
    '   Set oldsize = newsize',
    '   Wscript.Echo("checking file size")',
    paste0('   WScript.Sleep ', 1000L*check.interval),
    paste0('   Set fileobj = filesys.GetFile("""', file.path, '""")'),
    paste0('   Set newsize = fileobj.Size'),
    'Wend',
    'Wscript.Echo("no change, ended")',
    ''
  )

  lines
}










