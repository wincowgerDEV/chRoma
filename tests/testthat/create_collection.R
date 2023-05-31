context("Test create_collection function")

test_that("create_collection creates a valid vectorDB", {
  db <- create_collection()

  # Test that the output is a vectorDB
  expect_true(is_vectorDB(db))

  # Test that the database is initially empty
  expect_equal(nrow(db$vectors), 0)
  expect_equal(nrow(db$metadata), 0)
})
