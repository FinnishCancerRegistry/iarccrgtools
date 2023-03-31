
testthat::test_that("iarccrgtools:::filesystem_file_path_extension works", {
  testthat::expect_equal(
    iarccrgtools:::filesystem_file_path_extension("file.txt"),
    "txt"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_file_path_extension("file.txt.gz"),
    "gz"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_file_path_extension("file.txt.gz.tar"),
    "tar"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_file_path_extension("file.txt.gz.tar.bz2"),
    "bz2"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_file_path_extension("file.txt.gz.tar.bz2.7z"),
    "7z"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_file_path_extension("file.txt.gz.tar.bz2.7z.zip"),
    "zip"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_file_path_extension("file.txt.gz.tar.bz2.7z.zip.xz"),
    "xz"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_file_path_extension("file.txt.gz.tar.bz2.7z.zip.xz.tar.gz"),
    "gz"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_file_path_extension("file.txt.gz.tar.bz2.7z.zip.xz.tar.gz.zip"),
    "zip"
  )
})

testthat::test_that("filesystem_dir_of_path works", {
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("file.txt"),
    "."
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("file.txt.gz"),
    "."
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("file.txt.gz.tar"),
    "."
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("file.txt.gz.tar.bz2"),
    "."
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("file.txt.gz.tar.bz2.7z"),
    "."
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("file.txt.gz.tar.bz2.7z.zip"),
    "."
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("file.txt.gz.tar.bz2.7z.zip.xz"),
    "."
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("dir/file.txt"),
    "dir"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("dir/file.txt.gz"),
    "dir"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("dir/file.txt.gz.tar"),
    "dir"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("dir/file.txt.gz.tar.bz2"),
    "dir"
  )
  
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("some/dir/file.txt.gz.tar"),
    "some/dir"
  )
  testthat::expect_equal(
    iarccrgtools:::filesystem_dir_of_path("some/dir/file.txt.gz.tar.bz2"),
    "some/dir"
  )
})

testthat::test_that("iarccrgtools:::filesystem_path_normalise works", {
  testthat::expect_equal(
    iarccrgtools:::filesystem_path_normalise("\\\\solaris/drive/dir/"),
    "\\\\solaris\\drive\\dir\\"
  )

  testthat::expect_equal(
    iarccrgtools:::filesystem_path_normalise("dir"),
    paste0(iarccrgtools:::filesystem_path_normalise(getwd()), "dir")
  )

  testthat::expect_equal(
    iarccrgtools:::filesystem_path_normalise("dir/"),
    paste0(iarccrgtools:::filesystem_path_normalise(getwd()), "dir\\")
  )

  testthat::expect_equal(
    iarccrgtools:::filesystem_path_normalise("./dir/"),
    paste0(iarccrgtools:::filesystem_path_normalise(getwd()), "dir\\")
  )

  testthat::expect_equal(
    iarccrgtools:::filesystem_path_normalise("C:/some/dir/"),
    "C:\\some\\dir\\"
  )

  testthat::expect_equal(
    iarccrgtools:::filesystem_path_normalise("C:/some/dir/file.txt"),
    "C:\\some\\dir\\file.txt"
  )


})
