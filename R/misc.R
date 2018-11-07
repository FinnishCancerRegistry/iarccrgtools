




ask_yes_no <- function(
  query
) {
  stopifnot(
    length(query) == 1,
    is.character(query)
  )

  message(query)

  answer <- ""
  while (!answer %in% c("y", "yes", "n", "no", "c", "cancel")) {
    answer <- readline(prompt = ": ")
    answer <- tolower(answer)
  }

  switch(answer,
         y = TRUE,
         yes = TRUE,
         n = FALSE,
         no = FALSE,
         c = stop("Cancelled.", call. = FALSE),
         cancel = stop("Cancelled.", call. = FALSE))
}





ask_to_proceed <- function(
  query
) {
  if (!ask_yes_no(query)) {
    stop("Cancelled.")
  }
}














