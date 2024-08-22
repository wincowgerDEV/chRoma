#' Retrieve embeddings from OpenAI
#'
#' This function retrieves embeddings for a list of text inputs using OpenAI's API.
#'
#' @param inputs A list of text inputs
#' @param model The OpenAI embedding model to use
#' @param url The OpenAI api url to use
#' @param api_key The OpenAI api key to use
#' @return A list of numeric vectors
#' @importFrom jsonlite fromJSON
#' @importFrom data.table as.data.table
#' @export
#'
#' @examples
#' \dontrun{
#' embeddings <- retrieve_vectors(c("This is a document", "This is another document"))
#' }

retrieve_vectors <- function(inputs, model = 'text-embedding-ada-002',
                 url = "https://api.openai.com/v1/embeddings",
                 api_key = Sys.getenv("OPENAI_API_KEY")) {


  if(!is.character(inputs)) stop("Inputs must be a character vector of terms to get embeddings for.")

  if(is.null(api_key) || api_key == "") stop("API key is missing. Please set the OPENAI_API_KEY environment variable.")


  # Construct the command to run the Node.js script with arguments
  cmd <- sprintf("node inst/api_call.js '%s' '%s' '%s' '%s'", inputs, model, url, api_key)

  # Run the command
  system(cmd)

  # Load the output from the JSON file
  output <- tryCatch({
    jsonlite::fromJSON("inst/data.json")
  }, error = function(e) {
    message("Failed to read JSON: ", e$message)
    NULL
  })

  # Output the result
  data <- output$data

  # output embedding vectors as data frame
  vectors <- unlist(data$embedding) |>
    data.table::as.data.table()

  return(vectors)
}

