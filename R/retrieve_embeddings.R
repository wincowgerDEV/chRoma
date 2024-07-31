#' Retrieve embeddings from OpenAI
#'
#' This function retrieves embeddings for a list of text inputs using OpenAI's API.
#'
#' @param inputs A list of text inputs
#' @param model The OpenAI embedding model to use
#' @param url The OpenAI api url to use
#' @param api_key The OpenAI api key to use
#' @return A list of numeric vectors
#' @importFrom data.table as.data.table
#' @importFrom utils head
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \dontrun{
#' embeddings <- retrieve_vectors(c("This is a document", "This is another document"))
#' }
# retrieve_vectors <- function(inputs, model = 'text-embedding-ada-002', url = "https://api.openai.com/v1/embeddings", api_key = Sys.getenv("OPENAI_API_KEY")) {
#   if(!is.character(inputs)) stop("Inputs must be a character vector of terms to get embeddings for.")
#
#   if(is.null(api_key) || api_key == "") stop("API key is missing. Please set the OPENAI_API_KEY environment variable.")
#
#   # Convert the URL to host and path
#   parsed_url <- urltools::url_parse(url)
#   host <- parsed_url$domain
#   path <- parsed_url$path
#
#   # Prepare the data
#   parameter_list <- list(input=inputs, model=model)
#   json_data <- jsonlite::toJSON(parameter_list)
#
#   # Create authorization header
#   auth_header <- paste("Bearer", api_key)
#
#   # Create a connection
#   con <- socketConnection(host, port = 443, blocking = TRUE, open = "r+b", server = FALSE, ssl = TRUE)
#
#   # Write the request
#     parameter_list = list(input = inputs, model = model)
#     # Convert inputs to JSON
#     body <- toJSON(parameter_list)
#
#     request_base <- tryCatch({
#       # send http post request to url
#     #   httr::POST(url = url,
#     #              body = parameter_list,
#     #              # adds an authorized header which is bearer(token validation) and api_key
#     #              httr::add_headers(Authorization = paste("Bearer", api_key)),
#     #              encode = "json")
#     # },
#
#      con <- url(description = url, headers = c("Authorization" = paste("Bearer",
#                                                                  api_key),
#                                          "Content-Type" = "application/json"
#                                          ))
#      # write the body to the connection
#      write(body, con)
#
#      # read the response
#      response <- readLines(con)
#
#      # Close the connection
#      # close(con)
#
#      #return the response
#      response
#     },
#     error = function(e) {
#       stop("Error in HTTP request: ", e$message)
#     })
#
#     if(request_base$status_code != 200) stop("API request failed with status code: ", request_base$status_code)
#
#     output_base <- httr::content(request_base)
#
#     vectors <- lapply(output_base$data, function(x){unlist(x$embedding)}) |>
#       data.table::as.data.table()
#
#   return(vectors)
# }

# inputs: vector of character strings for which you want to get embeddings


# retrieve_vectors <- function(inputs, model = 'text-embedding-ada-002', url = "https://api.openai.com/v1/embeddings", api_key = Sys.getenv("OPENAI_API_KEY")) {
#   # Validate input is a character
#   if(!is.character(inputs)) stop("Inputs must be a character vector of terms to get embeddings for.")
#
#   if(is.null(api_key) || api_key == "") stop("API key is missing. Please set the OPENAI_API_KEY environment variable.")
#
#   # Create parameter request
#   parameter_list = list(input = inputs, model = model)
#
#   # Making the API request
#   request_base <- tryCatch({
#     # send a POST request --> sending data to the server
#     httr::POST(url = url,
#                # data being sent in the request
#                body = parameter_list,
#                # header required for authentication with the OpenAI API
#                httr::add_headers(Authorization = paste("Bearer", api_key)),
#                # JSON format
#                encode = "json")
#   }, error = function(e) {
#     stop("Error in HTTP request: ", e$message)
#   })
#
#   if(request_base$status_code != 200) stop("API request failed with status code: ", request_base$status_code)
#
#   # Extracts the response and parses the JSON response into an R list
#   output_base <- httr::content(request_base)
#
#   vectors <- lapply(output_base$data, function(x){unlist(x$embedding)}) |>
#     data.table::as.data.table()
#
#   return(vectors)
# }

# retrieve_vectors <- function(inputs, model = 'text-embedding-ada-002', url = "https://api.openai.com/v1/embeddings", api_key = Sys.getenv("OPENAI_API_KEY")) {
#   if(!is.character(inputs)) stop("Inputs must be a character vector of terms to get embeddings for.")
#
#   if(is.null(api_key) || api_key == "") stop("API key is missing. Please set the OPENAI_API_KEY environment variable.")
#
#   parameter_list = list(input = inputs, model = model)
#
#   body <- toJSON(parameter_list)
#
#   request_base <- tryCatch({
#     # send http post request to url
#     #   httr::POST(url = url,
#     #              body = parameter_list,
#     #              # adds an authorized header which is bearer(token validation) and api_key
#     #              httr::add_headers(Authorization = paste("Bearer", api_key)),
#     #              encode = "json")
#     # },
#
#     con <- url(description = url, headers = c("Authorization" = paste("Bearer",
#                                                                       api_key),
#                                               "Content-Type" = "application/json"
#     ))
#     # write the body to the connection
#     write(body, con)
#
#     # read the response
#     response <- readLines(con)
#
#     # Close the connection
#     # close(con)
#
#     #return the response
#     response
#   },
#   error = function(e) {
#     stop("Error in HTTP request beep boop: ", e$message)
#   })
#
#   if(request_base$status_code != 200) stop("API request failed with status code: ", request_base$status_code)
#
#   output_base <- httr::content(request_base)
#
#   vectors <- lapply(output_base$data, function(x){unlist(x$embedding)}) |>
#     data.table::as.data.table()
#
#   return(vectors)
# }

retrieve_vectors <- function(inputs, model = 'text-embedding-ada-002', url = "https://api.openai.com/v1/embeddings", api_key = Sys.getenv("OPENAI_API_KEY")) {
  if (!is.character(inputs)) stop("Inputs must be a character vector of terms to get embeddings for.")
  if (is.null(api_key) || api_key == "") stop("API key is missing. Please set the OPENAI_API_KEY environment variable.")

  # Prepare the request body
  body <- sprintf('{"input": %s, "model": "%s"}',
                  jsonlite::toJSON(inputs, auto_unbox = TRUE),
                  model)

  # Send request and get response
  response <- tryCatch({
    # Create the connection
    conn <- url(url, open = "r+b")
    on.exit(close(conn))

    # Send the request
    writeLines(paste0(
      "POST /v1/embeddings HTTP/1.1\r\n",
      "Host: api.openai.com\r\n",
      "Content-Type: application/json\r\n",
      "Authorization: Bearer ", api_key, "\r\n",
      "Content-Length: ", nchar(body), "\r\n",
      "\r\n",
      body
    ), conn)

    # Read the response
    response_lines <- readLines(conn)

    # Check status code
    status_line <- strsplit(response_lines[1], " ")[[1]]
    status_code <- as.integer(status_line[2])

    if (status_code != 200) {
      stop("API request failed with status code: ", status_code, "\nResponse: ", paste(response_lines, collapse="\n"))
    }

    # Extract the JSON part of the response
    json_start <- which(response_lines == "")[2] + 1
    paste(response_lines[json_start:length(response_lines)], collapse = "")

  }, error = function(e) {
    stop("Error in HTTP request: ", e$message)
  })

  # Parse the JSON response
  output_base <- tryCatch({
    jsonlite::fromJSON(response)
  }, error = function(e) {
    stop("Error parsing JSON response: ", e$message, "\nResponse received: ", response)
  })

  # Extract the embeddings
  vectors <- do.call(rbind, lapply(output_base$data, function(x) x$embedding))

  return(as.data.frame(vectors))
}
