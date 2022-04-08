

cache_metadata_file_name <- function() {
  "iarccrgtools_cache_info.csv"
}

cache_metadata_file_path <- function() {
  dir_path <- iarccrgtools::get_tools_work_dir()
  file_path <- paste0(dir_path, "/", cache_metadata_file_name())
  normalize_path(file_path)
}

cache_metadata_read <- function() {
  file_path <- cache_metadata_file_path()
  if (file.exists(file_path)) {
    data.table::fread(file_path)
  } else {
    data.table::data.table(
      dir_path = character(0L),
      input_file_path = character(0L),
      cache_metadata_time = Sys.time()[0],
      hash = character(0L)
    )
  }
}

cache_metadata_write <- function(x) {
  data.table::fwrite(x, file = cache_metadata_file_path())
}

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

cache_metadata_refresh <- function() {
  cache_metadata <- cache_metadata_read()
  cache_metadata <- cache_metadata[dir.exists(cache_metadata[["dir_path"]]), ]
  cache_metadata_write(cache_metadata)
}

cache_hash_random <- function() {
  cache_hash(runif(n = 1L))
}
cache_hash <- function(x) {
  digest::digest(x, algo = "sha256")
}

cache_clean_hash <- function(hash) {
  meta <- cache_metadata_read()
  has_hash <- meta[["hash"]] == hash
  dir_path <- meta[["dir_path"]][has_hash]
  if (length(dir_path) == 1 && dir.exists(dir_path)) {
    unlink(dir_path, recursive = TRUE, force = TRUE)
  }
  meta <- meta[!has_hash, ]
  cache_metadata_write(meta)
}

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



