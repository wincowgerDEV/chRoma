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
#' @param metadata A list of metadata corresponding to each vector. This cannot be NULL or empty.
#' @param model The name of the model to use for retrieving vectors. Default is 'text-embedding-ada-002'.
#' @param url The URL of the API to use for retrieving vectors. Default is "https://api.openai.com/v1/embeddings".
#' @param api_key The API key for the vector retrieval API. This is required when vectors is NULL.
#' @param ignore_duplicates a logical value TRUE for ignore FALSE for don't ignore.
#' @return The updated vector database.
#' @importFrom digest digest
#' @importFrom data.table is.data.table data.table rbindlist setnames as.data.table :=
#' @importFrom utils head
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite fromJSON
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
#'                  metadata = list(list(text = "This is a document", file = "source1"),
#'                                   list(text = "This is another document", file = "source2"),
#'                                   list(text = "This is yet another document", file = "source3"),
#'                                   list(text = "This is a fourth document", file = "source4"),
#'                                   list(text = "This is the fifth document", file = "source5"))
#'                  )
#' #Example combining two databases.
#' db2 <- add_collection(create_collection(),
#'                       vectors = db$vectors,
#'                       metadata = db$metadata)
#' }

add_collection <- function(db = create_collection(), vectors = NULL, metadata, model = 'text-embedding-ada-002', url = "https://api.openai.com/v1/embeddings", api_key = Sys.getenv("OPENAI_API_KEY"), ignore_duplicates = TRUE) {
  if(!inherits(db, "vectorDB")) stop("db is not a vector database.")

  # Check if metadata is NULL or empty
  if(is.null(metadata) || length(metadata) == 0) stop("metadata cannot be NULL or empty.")

  if(data.table::is.data.table(metadata)){
    metadata_dt <- metadata
  }
  else if(is.data.frame(metadata)){
    metadata_dt <- data.table::as.data.table(metadata)
  }
  else if(is.list(metadata)){
    metadata_dt <- data.table::rbindlist(metadata, use.names = FALSE)
  }
  else{
    stop("metadata must be either a data.table, data.frame, or a list")
  }

  if(!is.null(vectors)){
    if(data.table::is.data.table(vectors)){
    vectors_dt <- vectors
  }
  else if(is.list(vectors)){
    if(length(unique(sapply(vectors, length))) != 1) stop("All vectors should have the same length in the list.")
    vectors_dt <- data.table::as.data.table(vectors, use.names = FALSE)
  }
  else{
    stop("Vectors must be either a data.table or a list")
  }
  }

  if(any(metadata_dt$text %in% db$metadata$text)){
    if(ignore_duplicates){
      logical_dups <- !metadata_dt$text %in% db$metadata$text
      if(any(!logical_dups)){
        metadata_dt <- metadata_dt[logical_dups,]
        message("Some of the text you are requesting embeddings for already exists in the metadata of the database you are adding to. Ignoring duplicates.")
        if(!is.null(vectors_dt)){
          vectors_dt <- vectors_dt[, .SD, .SDcols = logical_dups]
        }
      }
    }
    else{
      message("Duplicating entries.Some of the text you are requesting embeddings for already exists in the metadata of the database you are adding to.")
    }
  }

  if (is.null(vectors)) {
    # Check if API Key is NULL or empty
    print(vectors)
    if(is.null(api_key) || api_key == "") stop("API Key cannot be NULL or empty when vectors is NULL.")
    # Retrieve new vectors using the API and the text field in the metadata
    vectors_dt <- retrieve_vectors(metadata_dt$text, model = model, url = url, api_key = api_key)
    print(nrow(metadata_dt))
    print(nrow(vectors_dt))

  }

  if((nrow(vectors_dt) != nrow(db$vectors)) & (nrow(db$vectors) != 0)) stop("All vectors should have the same length as the vectorDB you are adding to or you must be adding to an empty vectorDB.")
  #if(nrow(metadata_dt) != length(vectors_dt)) stop("The number of metadata and vectors should be the same.")

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

