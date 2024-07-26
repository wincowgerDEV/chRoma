#' Retrieve embeddings from OpenAI
#'
#' This function retrieves embeddings for a list of text inputs using OpenAI's API.
#'
#' @param inputs A list of text inputs
#' @param model The OpenAI embedding model to use
#' @param url The OpenAI api url to use
#' @param api_key The OpenAI api key to use
#' @return A list of numeric vectors
#' @importFrom httr POST content add_headers
#' @importFrom data.table as.data.table
#' @export
#'
#' @examples
#' \dontrun{
#' embeddings <- retrieve_vectors(c("This is a document", "This is another document"))
#' }
retrieve_vectors <- function(inputs, model = 'text-embedding-ada-002', url = "https://api.openai.com/v1/embeddings", api_key = Sys.getenv("OPENAI_API_KEY")) {
  if(!is.character(inputs)) stop("Inputs must be a character vector of terms to get embeddings for.")

  if(is.null(api_key) || api_key == "") stop("API key is missing. Please set the OPENAI_API_KEY environment variable.")

    parameter_list = list(input = inputs, model = model)

    body <- toJSON(parameter_list)

    request_base <- tryCatch({
      # send http post request to url
    #   httr::POST(url = url,
    #              body = parameter_list,
    #              # adds an authorized header which is bearer(token validation) and api_key
    #              httr::add_headers(Authorization = paste("Bearer", api_key)),
    #              encode = "json")
    # },

     con <- url(description = url, headers = c("Authorization" = paste("Bearer",
                                                                 api_key),
                                         "Content-Type" = "application/json"
                                         ))
     # write the body to the connection
     write(body, con)

     # read the response
     response <- readLines(con)

     # Close the connection
     # close(con)

     #return the response
     response
    },
    error = function(e) {
      stop("Error in HTTP request: ", e$message)
    })

    if(request_base$status_code != 200) stop("API request failed with status code: ", request_base$status_code)

    output_base <- httr::content(request_base)

    vectors <- lapply(output_base$data, function(x){unlist(x$embedding)}) |>
      data.table::as.data.table()

  return(vectors)
}
