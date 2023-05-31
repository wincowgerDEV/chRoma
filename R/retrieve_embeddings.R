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

  vectors <- lapply(inputs, function(input){
    parameter_list = list(input = input, model = model)

    request_base = httr::POST(url = url,
                              body = parameter_list,
                              httr::add_headers(Authorization = paste("Bearer", api_key)),
                              encode = "json")

    output_base = httr::content(request_base)
    if(request_base$status_code == 200){
      embedding_raw = unlist(output_base$data[[1]]$embedding)
    }
    else{
      embedding_raw = rep(-88, times = 1536)
    }
  }) |> data.table::as.data.table()

  return(vectors)
}
