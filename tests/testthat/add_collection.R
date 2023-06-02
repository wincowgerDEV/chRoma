test_that("add_collection function handles various input formats correctly", {

  vectors <- list(c = c(7, 8, 9), d = c(10, 11, 12))
  metadatas <- list(list(text = "text c", file = "source3"), list(text = "text d", file = "source4"))

  db_updated <- add_collection(create_collection(), vectors, metadatas)

  expect_equal(length(db_updated$vectors), 4)
  expect_equal(nrow(db_updated$metadata), 4)
  expect_true(all(c("a", "b", "c", "d") %in% db_updated$metadata$id))
  expect_true(all(c("a", "b", "c", "d") %in% colnames(db_updated$vectors)))

  vectors_dt <- data.table::rbindlist(vectors, use.names = FALSE)
  metadatas_dt <- data.table::rbindlist(metadatas, use.names = FALSE)

  db_updated <- add_collection(db, vectors_dt, metadatas_dt)

  expect_equal(length(db_updated$vectors), 4)
  expect_equal(nrow(db_updated$metadata), 4)
  expect_true(all(c("a", "b", "c", "d") %in% db_updated$metadata$id))
  expect_true(all(c("a", "b", "c", "d") %in% colnames(db_updated$vectors)))
})

test_that("add_collection function handles wrong input formats", {

  db <- create_vectorDB(list(a = c(1, 2, 3), b = c(4, 5, 6)),
                        list(id = c("a", "b"), text = c("text a", "text b")))

  vectors <- c(7, 8, 9, 10, 11, 12)
  metadatas <- list(list(text = "text c", file = "source3"), list(text = "text d", file = "source4"))

  expect_error(add_collection(db, vectors, metadatas), "Vectors must be either a data.table or a list")

  vectors <- list(c = c(7, 8, 9), d = c(10, 11, 12))
  metadatas <- c("text c", "text d")

  expect_error(add_collection(db, vectors, metadatas), "Metadatas must be either a data.table or a list")
})

test_that("add_collection function handles inconsistent vectors and metadatas lengths", {

  db <- create_vectorDB(list(a = c(1, 2, 3), b = c(4, 5, 6)),
                        list(id = c("a", "b"), text = c("text a", "text b")))

  vectors <- list(c = c(7, 8, 9), d = c(10, 11, 12), e = c(13, 14, 15))
  metadatas <- list(list(text = "text c", file = "source3"), list(text = "text d", file = "source4"))

  expect_error(add_collection(db, vectors, metadatas), "The number of metadata and vectors should be  the same.")

  vectors <- list(c = c(7, 8, 9))
  metadatas <- list(list(text = "text c", file = "source3"), list(text = "text d", file = "source4"))

  expect_error(add_collection(db, vectors, metadatas), "The number of metadata and vectors should be the same.")
})

test_that("add_collection function handles inconsistent vector lengths", {

  db <- create_vectorDB(list(a = c(1, 2, 3), b = c(4, 5, 6)),
                        list(id = c("a", "b"), text = c("text a", "text b")))

  vectors <- list(c = c(7, 8), d = c(10, 11, 12))
  metadatas <- list(list(text = "text c", file = "source3"), list(text = "text d", file = "source4"))

  expect_error(add_collection(db, vectors, metadatas), "All vectors should have the same length.")
})

test_that("add_collection function correctly fetches new vectors", {

  skip_on_cran()

  db <- create_vectorDB(list(a = c(1, 2, 3), b = c(4, 5, 6)),
                        list(id = c("a", "b"), text = c("text a", "text b")))

  metadatas <- list(list(text = "text c", file = "source3"), list(text = "text d", file = "source4"))

  db_updated <- add_collection(db, NULL, metadatas)

  expect_equal(length(db_updated$vectors), 4)
  expect_equal(nrow(db_updated$metadata), 4)
  expect_true(all(c("a", "b", "c", "d") %in% db_updated$metadata$id))
  expect_true(all(c("a", "b", "c", "d") %in% colnames(db_updated$vectors)))
})

