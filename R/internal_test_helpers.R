#' @title Skip Tests if Ollama Server is Unavailable
#' @description Helper function for unit tests that skips the test if the local Ollama server is not running.
#' @details The function sends a request to the Ollama server at `http://localhost:11434/api/tags`.
#' If the server is not running or the request fails, the test is skipped with an appropriate message.
#'
#' @return testthat skip object or NULL
#'
#' @noRd
#' @keywords internal
skip_test_if_no_ollama <- function() {

  # Define the request URL for the local Ollama server
  # List models that are available
  url <- "http://localhost:11434/api/tags"

  req <- httr2::request(url)

  # Try performing the request and catch any errors
  ollama_running <- tryCatch(
    {
      # Perform a GET request
      httr2::req_perform(req)
      TRUE # If the request succeeds, the server is running
    },
    error = function(e) {
      FALSE # If there's an error, assume the server is not running
    }
  )

  if (!ollama_running) {
    return(skip("skip test: Ollama not available"))
  }

  return(invisible(NULL))
}
