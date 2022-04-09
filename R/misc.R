




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






str_extract <- function(x, pattern, ...) {
  m <- regexpr(pattern = pattern, text = x, ...)
  regmatches(x = x, m = m)
}






seconds_elapsed <- function(t) {
  stopifnot(
    inherits(t, "proc_time")
  )
  proc.time()["elapsed"] - t["elapsed"]
}


























