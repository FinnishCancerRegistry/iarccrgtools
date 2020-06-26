



format_fwf <- function(x, width) {
  stopifnot(
    length(width) == 1L,
    width > 0L,
    width %% 1L == 0L,
    width >= nchar(x)
  )
  UseMethod("format_fwf")
}
format_fwf.integer <- function(x, width) {
  formatC(x = x, width = width, flag = "0")
}
format_fwf.numeric <- function(x, width) {
  formatC(x = x, width = width, flag = "0")
}
format_fwf.character <- function(x, width) {
  formatC(x = x, width = width, flag = " ")
}
format_fwf.Date <- function(x, width) {
  format_fwf(as.character(x), width)
}



#' @importFrom data.table setDT setnames fwrite
write_fwf <- function(x, path, widths = NULL, ...) {
  assert_dataframe(x)
  assert_write_file_path(path)
  stopifnot(
    is.null(widths) || (is.numeric(widths) && length(widths) == length(x))
  )
  
  min_widths <- vapply(x, function(col) max(nchar(col)), integer(1L))
  if (is.null(widths)) {
    widths <- min_widths
  }
  if (any(widths < min_widths)) {
    stop("some supplied widths were smaller than max(nchar(x$col)) in the ",
         "corresponding column")
  }
  
  char_x <- data.table::setDT(lapply(seq_along(widths), function(j) {
    format_fwf(x[[j]], width = widths[j])
  }))
  data.table::setnames(char_x, names(char_x), names(x))
  data.table::fwrite(char_x, file = path, ...)
}




read_fwf <- function(path, ...) {
  data.table::fread(file = path, ...)
}


