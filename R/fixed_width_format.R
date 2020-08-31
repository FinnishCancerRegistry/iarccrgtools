

#' @title Fixed Width Format
#' @description
#' Read and write fixed-width format files.
#' @param x `[various]` (mandatory, no default)
#' 
#' - `write_fwf`: `data.frame` to write to disk
#' - `format_fwf`: an R object to format
#' @name fwf
NULL

#' @rdname fwf
#' @param width `[integer]` (mandatory, no default)
#' width of field to be written
#' @export
format_fwf <- function(x, width) {
  stopifnot(
    length(width) == 1L,
    width >= 0L,
    width %% 1L == 0L,
    width >= max(nchar(x))
  )
  UseMethod("format_fwf")
}
#' @rdname fwf
#' @export
format_fwf.integer <- function(x, width) {
  formatC(x = x, width = width, flag = "0")
}
#' @rdname fwf
#' @export
format_fwf.numeric <- function(x, width) {
  formatC(x = x, width = width, flag = "0")
}
#' @rdname fwf
#' @export
format_fwf.character <- function(x, width) {
  formatC(x = x, width = width, flag = " ")
}
#' @rdname fwf
#' @export
format_fwf.Date <- function(x, width) {
  format_fwf(as.character(x), width)
}



#' @rdname fwf
#' @export
#' @importFrom data.table setDT setnames fwrite
#' @param path `[character]` (mandatory, no default)
#' 
#' - `write_fwf`: path to write to
#' - `read_fwf`: path to read from
#' @param widths `[NULL, integer]` (optional, default `NULL`)
#' 
#' - `NULL`: the width of each field will be `max(nchar(x[[i]]))` for all 
#'   `i in 1:ncol(x)`
#' - `integer`: use these widths; must be of length `ncol(x)`
#' @param ...
#' 
#' additional arguments passed to
#' - `write_fwf`: [data.table::fwrite]
#' - `read_fwf`: [data.table::fread]
write_fwf <- function(x, path, widths = NULL, ...) {
  assert_dataframe(x)
  assert_write_file_path(path)
  
  min_widths <- vapply(x, function(col) max(nchar(col)), integer(1L))
  if (is.null(widths)) {
    widths <- min_widths
  } else {
    stopifnot(
      is.numeric(widths),
      length(widths) == length(x),
      widths >= min_widths
    )
  }
  
  fwf_x <- data.table::setDT(lapply(seq_along(widths), function(j) {
    format_fwf(x[[j]], width = widths[j])
  }))
  data.table::setnames(fwf_x, names(fwf_x), names(x))
  data.table::fwrite(fwf_x, file = path, ...)
}




#' @rdname fwf
#' @export
read_fwf <- function(path, ...) {
  data.table::fread(file = path, ...)
}


