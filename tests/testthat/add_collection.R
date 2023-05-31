test_that("add_collection function works correctly", {
  # Create a new collection
  db <- create_collection()

  # Add new vectors and metadata to the collection
  db <- add_collection(db,
                       vectors = list(c(1.2, 2.3, 4.5), c(6.7, 8.2, 9.2)),
                       metadatas = list(list(text = "This is a document", file = "source1"),
                                        list(text = "This is another document", file = "source2"))
  )

  # Check that the function has added vectors and metadata to the collection correctly
  expect_equal(ncol(db$vectors), 2)
  expect_equal(nrow(db$metadata), 2)

  # Check that the function raises an error if the number of vectors does not match the number of metadata entries
  expect_error(add_collection(db,
                              vectors = list(c(1.2, 2.3, 4.5)),
                              metadatas = list(list(text = "This is a document", file = "source1"),
                                               list(text = "This is another document", file = "source2"))
  ),
  "All inputs and outputs should be of the same length."
  )
})
