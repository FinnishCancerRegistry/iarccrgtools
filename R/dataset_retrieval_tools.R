
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
  expr <- paste0("iarccrgtools:::", dataset.name)
  result <- tryCatch(
    eval(parse(text = expr)),
    error = function(e) e
  )
  if (inherits(result, "try-error")) {
    raise_internal_error(
      "Requested internal dataset ",
      deparse(dataset.name), " is not found. "
    )
  }
  return(result[])
}
