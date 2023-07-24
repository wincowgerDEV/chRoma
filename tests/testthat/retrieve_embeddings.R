test_that("retrieve_vectors handles missing API key correctly", {
  # Remove the API key from the environment
  Sys.unsetenv("OPENAI_API_KEY")

  # Try to call the function and expect an error
  inputs <- c("This is a document", "This is another document")
  expect_error(retrieve_vectors(inputs), "API key is missing.")
})

test_that("retrieve_vectors handles incorrect input types correctly", {
  # Create a test API key and export it to the environment
  test_api_key <- "test_api_key"
  Sys.setenv(OPENAI_API_KEY = test_api_key)

  # Call the function with incorrect inputs
  incorrect_inputs <- list(c("This is a document", "This is another document"))
  expect_error(retrieve_vectors(incorrect_inputs), "Inputs must be a character vector of terms to get embeddings for.")

  incorrect_inputs <- data.frame(c("This is a document", "This is another document"))
  expect_error(retrieve_vectors(incorrect_inputs), "Inputs must be a character vector of terms to get embeddings for.")
})


test_that("check that an open ai api key works", {
  library(data.table)
  testthat::skip_on_cran()

  api_key <- readLines("openai.txt")

  words_to_retrieve <- c("test", "experiment", "elephant")

  vectors <- retrieve_vectors(words_to_retrieve, api_key = api_key)

  expect_true(is.data.table(vectors))
})

