test_that("add_collection function handles various input formats correctly", {

  vectors <- list(c(7, 8, 9), c(10, 11, 12))
  metadatas <- list(list(text = "text c", file = "source3"), list(text = "text d", file = "source4"))

  db_updated <- add_collection(create_collection(), vectors, metadatas)

  expect_equal(length(db_updated$vectors), 2)
  expect_equal(nrow(db_updated$metadata), 2)
  expect_true(all(names(db_updated$vectors) %in% db_updated$metadata$id))

  vectors_dt <- data.table::as.data.table(vectors, use.names = FALSE)
  metadatas_dt <- data.table::rbindlist(metadatas, use.names = FALSE)

  db_updated <- add_collection(db_updated, vectors_dt, metadatas_dt)

  expect_equal(length(db_updated$vectors), 4)
  expect_equal(nrow(db_updated$metadata), 4)
  expect_true(all(names(db_updated$vectors) %in% db_updated$metadata$id))
})

test_that("add_collection function handles wrong input formats", {

  vectors <- c(7, 8, 9, 10, 11, 12)
  metadatas <- list(list(text = "text c", file = "source3"), list(text = "text d", file = "source4"))

  expect_error(add_collection(create_collection(), vectors, metadatas), "Vectors must be either a data.table or a list")

  vectors <- list(c = c(7, 8, 9), d = c(10, 11, 12))
  metadatas <- c("text c", "text d")

  expect_error(add_collection(create_collection(), vectors, metadatas), "Metadatas must be either a data.table or a list")
})

test_that("add_collection function handles inconsistent vectors and metadatas lengths", {

  vectors <- list(c = c(7, 8, 9), d = c(10, 11, 12), e = c(13, 14, 15))
  metadatas <- list(list(text = "text c", file = "source3"), list(text = "text d", file = "source4"))

  expect_error(add_collection(create_collection(), vectors, metadatas), "The number of metadata and vectors should be the same.")

  vectors <- list(c = c(7, 8, 9))
  metadatas <- list(list(text = "text c", file = "source3"), list(text = "text d", file = "source4"))

  expect_error(add_collection(create_collection(), vectors, metadatas), "The number of metadata and vectors should be the same.")
})

test_that("add_collection function handles inconsistent vector lengths", {

  vectors <- list(c = c(7, 8), d = c(10, 11, 12))
  metadatas <- list(list(text = "text c", file = "source3"), list(text = "text d", file = "source4"))

  expect_error(add_collection(create_collection(), vectors, metadatas), "All vectors should have the same length in the list.")
})


