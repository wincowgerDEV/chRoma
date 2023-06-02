#' Add new vectors and/or metadata to the collection
#'
#' This function retrieves new vectors from an external source using the provided API key
#' (if not already provided) and adds them, along with any metadata, to the specified vector database.
#' If vectors are not provided, the function uses the text field in the metadata to query an API
#' to retrieve the vectors. The id field in the metadata is created as a reproducible hash of the
#' embedding vector.
#'
#' @param db The vector database.
#' @param vectors A list of vectors to add. If NULL, vectors are retrieved using the text field in the metadata.
#' @param metadatas A list of metadata corresponding to each vector. This cannot be NULL or empty.
#' @param model The name of the model to use for retrieving vectors. Default is 'text-embedding-ada-002'.
#' @param url The URL of the API to use for retrieving vectors. Default is "https://api.openai.com/v1/embeddings".
#' @param api_key The API key for the vector retrieval API. This is required when vectors is NULL.
#' @return The updated vector database.
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
#' #Example combining two databases.
#' db2 <- add_collection(create_collection(),
#'                       vectors = db$vectors,
#'                       metadatas = db$metadatas)
#' }
add_collection <- function(db, vectors = NULL, metadatas, model = 'text-embedding-ada-002', url = "https://api.openai.com/v1/embeddings", api_key = Sys.getenv("OPENAI_API_KEY")) {
  if(!inherits(db, "vectorDB")) stop("db is not a vector database.")

  # Check if metadatas is NULL or empty
  if(is.null(metadatas) || length(metadatas) == 0) stop("metadatas cannot be NULL or empty.")

  if(is.data.table(metadatas)){
    metadata_dt <- metadatas
  }
  else if(is.list(metadatas)){
    metadata_dt <- data.table::rbindlist(metadatas, use.names = FALSE)
  }
  else{
    stop("Metadatas must be either a data.table or a list")
  }

  if (is.null(vectors)) {
    # Check if API Key is NULL or empty
    if(is.null(api_key) || api_key == "") stop("API Key cannot be NULL or empty when vectors is NULL.")
    if(any(metadata_dt$text %in% db$metadata$text)) message("Some of the text you are requesting embeddings for already exists in the metadata of the database you are adding to.")
    # Retrieve new vectors using the API and the text field in the metadata
    vectors <- retrieve_vectors(metadata_dt$text, model = model, url = url, api_key = api_key)
  }

  if(is.data.table(vectors)){
    vectors_dt <- vectors
  }
  else if(is.list(vectors)){
    vectors_dt <- data.table::rbindlist(vectors, use.names = FALSE)
  }
  else{
    stop("Vectors must be either a data.table or a list")
  }

  if(nrow(db$vectors) != 0 & nrow(vectors_dt) != nrow(db$vectors)) stop("All vectors should have the same length.")

  if(nrow(metadata_dt) != length(vectors_dt)) stop("The number of metadata and vectors should be the same.")

  # Create a reproducible hash of the embedding vector to be used as the id
  ids <- vapply(vectors_dt, function(x) digest::digest(x), FUN.VALUE = character(1))

  # Prepare data.tables
  data.table::setnames(vectors_dt, ids)

  metadata_dt[, id := ids]

  # Add to database
  db$vectors <- cbind(db$vectors, vectors_dt)
  db$metadata <- rbind(db$metadata, metadata_dt, fill = TRUE)

  return(db)
}
