testthat::test_that("query_collection works correctly", {
  db <- create_collection()
  db <- add_collection(db, vectors = list(c(1.2, 2.3, 4.5), c(6.7, 8.2, 9.2)),
                       metadata = list(list(text = "This is a document", file = "source1"), list(text = "This is another document", file = "source2")))

  testthat::expect_error(query_collection(db, query_embeddings = list("text")))
  testthat::expect_error(query_collection(db, top_n = -1), "top_n must be a positive integer.")
  testthat::expect_error(query_collection(db, type = "invalid_type"), "type must be 'cosine' or 'dotproduct'.")

  filtered_db <- query_collection(db, filter = "file == 'source1'")
  testthat::expect_equal(nrow(filtered_db$metadata), 1)
  testthat::expect_equal(ncol(filtered_db$vectors), 1)

  similarity <- query_collection(db, query_embeddings = c(1.2, 2.3, 4.5))
  testthat::expect_equal(dim(similarity), c(1, 2))

  similarity_top1 <- query_collection(db, query_embeddings = c(1.2, 2.3, 4.5), top_n = 1)
  testthat::expect_equal(nrow(similarity_top1), 1)
})
