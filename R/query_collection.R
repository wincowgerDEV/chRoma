#' Query a vector database
#'
#' This function filters a vector database based on provided metadata constraints and/or computes
#' similarity of the remaining vectors to a provided embedding, a text string, or another database.
#'
#' @param db The vector database
#' @param filter An expression string that represents the filter to be applied on metadata
#' @param query_embeddings A vector or other vector database to compute similarity against
#' @param top_n Integer, indicating the number of top matches to return
#' @param type The function to use to compute the similarities between the vectors
#' @return A matrix of similarity scores or a data.table of top matches
#' @importFrom data.table data.table setnames :=
#' @importFrom utils head
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' db <- add_collection(create_collection(),
#'                  metadatas = list(list(text = "This is a document", file = "source1"),
#'                                   list(text = "This is another document", file = "source2"),
#'                                   list(text = "This is yet another document", file = "source3"),
#'                                   list(text = "This is a fourth document", file = "source4"),
#'                                   list(text = "This is the fifth document", file = "source5"))
#'                  )
#' similarity_internal <- query_collection(db,
#'                                query_embeddings = db,
#'                                top_n = 2)
#'
#' similarity <- query_collection(db,
#'                                filter = "file == 'source1'",
#'                                query_embeddings = c(1.2, 2.3, 4.5),
#'                                top_n = 5)
#' }
query_collection <- function(db, filter = NULL, query_embeddings = NULL, top_n = NULL, type = "cosine") {
  if(!is_vectorDB(db)) stop("db is not a vector database.")

  if(!(is.null(top_n) || (round(top_n) == top_n && top_n > 0))) stop("top_n must be a positive integer.")

  if(!type %in% c("cosine", "dotproduct")) stop("type must be 'cosine' or 'dotproduct'.")

  # Filter the database based on metadata
  if (!is.null(filter)) {
    db$metadata <- db$metadata[eval(parse(text=filter)), ]
    db$vectors <- db$vectors[, db$metadata$id, with = FALSE]
  }

  # Compute similarity
  if (!is.null(query_embeddings)) {
    if(is.character(query_embeddings)){
      query_embeddings <- as.matrix(retrieve_vectors(query_embeddings))
      colnames(query_embeddings) <- "identified"
    }
    else if(is.numeric(query_embeddings)){
      query_embeddings <- matrix(query_embeddings, ncol = 1)
      colnames(query_embeddings) <- "identified"
    }
    else if(is_vectorDB(query_embeddings)){
      query_embeddings <- as.matrix(query_embeddings$vectors)
    }
    else {
      stop("query_embeddings must be character, numeric, or vectorDB.")
    }

    if(type == "cosine"){
      #similarity <- text2vec::sim2(t(query_embeddings), t(as.matrix(db$vectors)), method = "cosine", norm = "l2")

      query_matrix <- t(query_embeddings)

      db_matrix <- db_vectors

      similarity <- cosine(query_matrix, db_matrix)

      }

    if(type == "dotproduct"){
      similarity <- crossprod(query_embeddings, as.matrix(db$vectors))
    }

    if (!is.null(top_n)) {
      # Reshape similarity matrix and convert to data.table
      similarity_dt <- data.table::data.table(query_id = rownames(similarity),
                                              db_id = rep(colnames(similarity),
                                                          each = nrow(similarity)),
                                              similarity = c(similarity))

      # Order and filter top_n matches for each query
      similarity_dt <- similarity_dt[order(-similarity), head(.SD, top_n), by = query_id]

      return(similarity_dt)
    }

    return(similarity)
  }

  return(db)
}

