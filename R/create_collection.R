#' Create a new vector database
#'
#' This function creates a new in-memory vector database.
#'
#' @return A new vector database
#' @importFrom data.table data.table
#' @importFrom utils head
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' db <- create_collection()
#' }
create_collection <- function() {
  # Create the database
  db <- list(vectors = data.table::data.table(),
             metadata = data.table::data.table(id = character(),
                                               text = character(),
                                               file = character())
             )
  class(db) <- 'vectorDB'
  return(db)
}

