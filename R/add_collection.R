#' Add new vectors and/or metadata to the collection
#'
#' This function retrieves new vectors from an external source using the provided API key
#' (if not already provided) and adds them, along with any metadata, to the specified vector database.
#' If vectors are not provided, the function uses the text field in the metadata to query an API
#' to retrieve the vectors. The id field in the metadata is created as a reproducible hash of the
#' embedding vector.
#'
#' @param db The vector database
#' @param vectors A list of vectors to add. If NULL, vectors are retrieved using the text field in the metadata.
#' @param metadatas A list of metadata corresponding to each vector
#' @return The updated vector database
#' @importFrom digest digest
#' @importFrom data.table data.table rbindlist setnames as.data.table :=
#' @export
#'
#' @examples
#' \dontrun{
#' db <- add_collection(create_collection(),
#'                  vectors = list(c(1.2, 2.3, 4.5),
#'                                    c(6.7, 8.2, 9.2),
#'                                    c(3.5, 6.7, 8.1),
#'                                    c(1.8, 7.2, 6.3),
#'                                    c(5.6, 7.2, 8.9)),
#'                  metadatas = list(list(text = "This is a document", file = "source1"),
#'                                   list(text = "This is another document", file = "source2"),
#'                                   list(text = "This is yet another document", file = "source3"),
#'                                   list(text = "This is a fourth document", file = "source4"),
#'                                   list(text = "This is the fifth document", file = "source5"))
#'                  )
#' }

add_collection <- function(db, vectors = NULL, metadatas, model = 'text-embedding-ada-002', url = "https://api.openai.com/v1/embeddings", api_key = Sys.getenv("OPENAI_API_KEY")) {
  if(!inherits(db, "vectorDB")) stop("db is not a vector database.")

  metadata_dt <- data.table::rbindlist(metadatas, use.names = FALSE)

  if (is.null(vectors)) {
    # Retrieve new vectors using the API and the text field in the metadata
    # Replace the following line with your actual retrieval function
    vectors <- retrieve_vectors(metadata_dt$text, model = model, url = url, api_key = api_key)
  }

  if(nrow(metadata_dt) != length(vectors)) stop("All inputs and outputs should be of the same length.")

  # Create a reproducible hash of the embedding vector to be used as the id
  ids <- vapply(vectors, function(x) digest::digest(x), FUN.VALUE = character(1))

  # Prepare data.tables
  vectors_dt <- data.table::as.data.table(vectors)
  data.table::setnames(vectors_dt, ids)

  metadata_dt[, id := ids]

  # Add to database
  db$vectors <- cbind(db$vectors, vectors_dt)
  db$metadata <- rbind(db$metadata, metadata_dt, fill = TRUE)

  return(db)
}
