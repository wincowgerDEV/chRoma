#' Check if an object is a vectorDB
#'
#' This function checks if an object is a vectorDB by checking its class, structure, and contents.
#'
#' @param x The object to check
#' @return TRUE if the object is a vectorDB, FALSE otherwise
#' @importFrom data.table is.data.table
#' @export
#'
#' @examples
#' \dontrun{
#' is_vectorDB(db)
#' }
is_vectorDB <- function(x) {
  if (!inherits(x, "vectorDB")) return(FALSE)

  # Check if the object has the expected structure
  if (!is.list(x)) return(FALSE)
  if (length(x) != 2) return(FALSE)
  if (!all(names(x) %in% c("vectors", "metadata"))) return(FALSE)

  # Check the type of each component
  if (!all(sapply(x, data.table::is.data.table))) return(FALSE)

  # Check if metadata has an "id" column
  if (!"id" %in% names(x$metadata)) return(FALSE)

  return(TRUE)
}
