testthat::test_that("is_vectorDB correctly identifies vectorDB objects", {
  db <- create_collection()
  testthat::expect_true(is_vectorDB(db))

  # Altering the class should cause the function to return FALSE
  class(db) <- "data.frame"
  testthat::expect_false(is_vectorDB(db))

  # Altering the structure should cause the function to return FALSE
  db <- create_collection()
  db$extra <- data.table::data.table()
  testthat::expect_false(is_vectorDB(db))

  # Removing the id column should cause the function to return FALSE
  db <- create_collection()
  db$metadata[, id := NULL]
  testthat::expect_false(is_vectorDB(db))
})
