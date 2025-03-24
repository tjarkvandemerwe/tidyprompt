# Quitting when total token count is exceeded (Google Gemini API example)
\dontrun{
  "How are you?" |>
    # Forcing multi-response via initial error, for demonstration purposes
    answer_as_integer(add_instruction_to_prompt = FALSE) |>
    # Validation function to check total token count
    prompt_wrap(validation_fn = function(response, llm_provider, http_list) {
      total_tokens <- purrr::map_dbl(
        http_list$responses,
        ~ .x$body |>
          rawToChar() |>
          jsonlite::fromJSON() |>
          purrr::pluck("usageMetadata", "totalTokenCount")
      ) |> sum()
      if (total_tokens > 50) {
        warning("Token count exceeded; preventing further interactions")
        # Using llm_break_soft() to prevent further interactions
        return(llm_break_soft(response))
      }
    }) |>
    send_prompt(llm_provider_google_gemini(), return_mode = "full")
}
