
#' @title Cache Metadata
#' @description
#' Tools to access and handle cache metadata. These are advanced /
#' under-the-hood tools which you almost certainly should not use for anything.
#' `[iarccrgtools::cache_clean_hash]` and
#' `[iarccrgtools::cache_clean_all]` may be the exceptions.
#' @name cache_metadata
NULL

#' @rdname cache_metadata
#' @export
#' @section Functions:
#' `[iarccrgtools::cache_metadata_file_path]` returns a string, the name of
#' to the cache metadata file.
cache_metadata_file_name <- function() {
  "iarccrgtools_cache_info.csv"
}

#' @rdname cache_metadata
#' @export
#' @section Functions:
#' `[iarccrgtools::cache_metadata_file_path]` returns a string, the path
#' to the cache metadata.
cache_metadata_file_path <- function() {
  dir_path <- iarccrgtools::get_tools_work_dir()
  file_path <- paste0(dir_path, "/", cache_metadata_file_name())
  filesystem_path_normalise(file_path)
}

#' @rdname cache_metadata
#' @export
#' @section Functions:
#' `[iarccrgtools::cache_metadata_read]` reads cache metadata from the path
#' given by
#' `[iarccrgtools::cache_metadata_file_path]`. Returns a `data.table` with
#' zero rows if no file found.
cache_metadata_read <- function() {
  file_path <- cache_metadata_file_path()
  default_out <- data.table::data.table(
    dir_path = character(0L),
    input_file_path = character(0L),
    cache_metadata_time = Sys.time()[0],
    hash = character(0L)
  )
  if (file.exists(file_path)) {
    out <- data.table::fread(file_path)
    if (nrow(out) == 0L) {
      out <- default_out
    }
  } else {
    out <- default_out
  }
  return(out)
}

#' @rdname cache_metadata
#' @export
#' @param metadata `[data.frame]` (no default)
#'
#' Metadata.
#' @section Functions:
#' `[iarccrgtools::cache_metadata_write]` writes `metadata` to the path given by
#' `[iarccrgtools::cache_metadata_file_path]`.
cache_metadata_write <- function(metadata) {
  stopifnot(
    is.data.frame(metadata)
  )
  data.table::fwrite(metadata, file = cache_metadata_file_path())
}

#' @rdname cache_metadata
#' @export
#' @param hash `[character]` (no default)
#'
#' Hash of an input dataset.
#' @param working.dir `[character]` (no default)
#'
#' Path to directory to include in metadata.
#' @param input.file.path `[character]` (no default)
#'
#' Path to input file (which must exist) to include in metadata.
#' @section Functions:
#' `[iarccrgtools::cache_metadata_append_or_replace]` appends data to
#' the cache metadata, or replaces data that already exists for the
#' same input file path, if any.
cache_metadata_append_or_replace <- function(
  hash,
  working.dir,
  input.file.path
) {
  stopifnot(
    is.character(hash),
    length(hash) == 1L,
    !is.na(hash),

    is.character(working.dir),
    length(working.dir) == 1L,
    !is.na(working.dir),
    dir.exists(working.dir),

    is.character(input.file.path),
    length(input.file.path) == 1L,
    !is.na(input.file.path),
    file.exists(input.file.path)
  )
  cache_metadata <- cache_metadata_read()
  cache_metadata <- rbind(
    cache_metadata,
    data.table::data.table(
      dir_path = working.dir,
      input_file_path = input.file.path,
      cache_metadata_time = Sys.time(),
      hash = hash
    )
  )
  data.table::setkeyv(
    cache_metadata,
    c("dir_path", "input_file_path", "cache_metadata_time")
  )
  cache_metadata <- cache_metadata[
    !duplicated(cache_metadata, by = "input_file_path", fromLast = TRUE),
  ]
  cache_metadata_write(cache_metadata)
}

#' @rdname cache_metadata
#' @export
#' @section Functions:
#' `[iarccrgtools::cache_metadata_refresh]` removes any non-existing
#' directories from the cache metadata.
cache_metadata_refresh <- function() {
  cache_metadata <- cache_metadata_read()
  cache_metadata <- cache_metadata[dir.exists(cache_metadata[["dir_path"]]), ]
  cache_metadata_write(cache_metadata)
}

#' @rdname cache_metadata
#' @export
#' @section Functions:
#' `[iarccrgtools::cache_hash_random]` outputs a random hash.
cache_hash_random <- function() {
  cache_hash(runif(n = 1L))
}
#' @rdname cache_metadata
#' @export
#' @section Functions:
#' `[iarccrgtools::cache_hash]` outputs a hash for the input `data.frame`.
#' @param df `[data.frame]` (no default)
#' `data.frame` for which to compute hash.
cache_hash <- function(df) {
  stopifnot(
    is.data.frame(df)
  )
  digest::digest(df, algo = "sha256")
}

#' @rdname cache_metadata
#' @export
#' @section Functions:
#' `[iarccrgtools::cache_clean_hash]` removes all cache dirs for the given hash.
#' Also cleans up the cache metadata.
cache_clean_hash <- function(hash) {
  meta <- cache_metadata_read()
  has_hash <- meta[["hash"]] == hash
  dir_paths <- meta[["dir_path"]][has_hash]
  dir_paths <- dir_paths[dir.exists(dir_paths)]
  if (length(dir_paths) > 0) {
    unlink(dir_paths, recursive = TRUE, force = TRUE)
  }
  meta <- meta[!has_hash, ]
  cache_metadata_write(meta)
}

#' @rdname cache_metadata
#' @export
#' @section Functions:
#' `[iarccrgtools::cache_clean_all]` removes all cache dirs.
#' Also cleans up the cache metadata.
cache_clean_all <- function() {
  meta <- cache_metadata_read()
  dir_paths <- meta[["dir_path"]]
  dir_paths <- dir_paths[dir.exists(dir_paths)]
  if (length(dir_paths) > 0) {
    unlink(dir_paths, recursive = TRUE, force = TRUE)
  }
  meta <- meta[0L, ]
  cache_metadata_write(meta)
}



