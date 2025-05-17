#' Extraction function to extract final response from 'FINISH[...]'
#'
#' This is a helper function which assists in extracting the final response from
#' a large language model after it has been instructed to give this within
#' 'FINISH[...]' (as is done by [answer_by_chain_of_thought()] and
#' [answer_by_react()]). If something cannot be extracted from within the brackets,
#' it falls back on attempting to extract everything after the final occurence
#' of a 'FINISH' variant.
#'
#' @param llm_response Response of a large language model
#' @param lenience Whether lenience should be applied when extracting;
#' when TRUE, extraction fallback method will be to extract anything after
#' the last occurence of a variation on 'FINISH'
#'
#' @return Extracted final answer
#'
#' @noRd
#' @keywords internal
extraction_fn_finish <- function(llm_response, lenience = TRUE) {
  error_feedback <- llm_feedback(
    glue::glue(
      "Error, could not parse your final answer.\n",
      "Please type: 'FINISH[<put here your final answer to the original prompt>]'"
    )
  )

  # Check if FINISH is mentioned in the response.
  if (!stringr::str_detect(llm_response, "(?si)FINISH")) return(error_feedback)

  # First attempt: Extract text between FINISH[...]
  extracted_response <- stringr::str_extract(
    llm_response,
    "(?si)(?<=FINISH\\[).+?(?=\\])"
  )

  # If extraction fails, try alternative method
  if (is.na(extracted_response) & lenience) {
    # Use regex to match 'FINISH' variants and capture text between brackets/braces or spaces
    # start with capturing everything after finish:
    extracted_response <- stringr::str_extract(
      llm_response,
      "(?si)(?<=\\bFINISH).*"
    )

    # remove whitespaces
    extracted_response <- stringr::str_trim(extracted_response)

    # if text starts with punctuation ':, =, ...' and optional space remove it
    extracted_response <- stringr::str_replace(
      extracted_response,
      "^[[:punct:]]+\\s*",
      ""
    )

    # if text begins and ends with any form of braces '[{(' remove them
    has_start_brace <- stringr::str_detect(extracted_response, "^[\\[\\{\\(]")
    has_end_brace <- stringr::str_detect(extracted_response, "[\\]\\}\\)]$")

    if (has_start_brace && has_end_brace) {
      # remove braces
      extracted_response <- stringr::str_replace_all(
        extracted_response,
        "(^[\\[\\{\\(])|([\\]\\}\\)]$)",
        ""
      )
    }
  }

  # Final check for successful extraction
  if (
    is.na(extracted_response) ||
      tolower(trimws(extracted_response)) %in% c("answer", "final answer") ||
      extracted_response |> stringr::str_trim() == ""
  ) {
    return(error_feedback)
  }

  return(extracted_response)
}

#' Extract and parse JSONs from a string (LLM response)
#'
#' This function extracts JSON blocks from a string and parses them using `jsonlite::fromJSON()`.
#' This can be used to extract all JSONs from LLM responses, immediately converting
#' them to R objects.
#'
#' @param llm_response A character string
#'
#' @return A list of parsed JSON objects
#'
#' @family text_helpers
#'
#' @noRd
#' @keywords internal
extraction_fn_json <- function(llm_response) {
  positions <- gregexpr("[{}]", llm_response)[[1]]
  matches <- regmatches(llm_response, gregexpr("[{}]", llm_response))[[1]]

  stack <- c()
  blocks <- list()
  block_start <- c()

  for (i in seq_along(positions)) {
    char <- matches[i]
    pos <- positions[i]

    if (is.na(char)) next

    if (char == "{") {
      if (length(stack) == 0) {
        # Start of a new block
        block_start <- c(block_start, pos)
      }
      stack <- c(stack, char)
    } else if (char == "}") {
      if (length(stack) == 0) {
        # Unmatched closing brace
        next
      }
      stack <- stack[-length(stack)]
      if (length(stack) == 0) {
        # End of a block
        start_pos <- block_start[1]
        end_pos <- pos
        block_text <- substr(llm_response, start_pos, end_pos)
        blocks <- c(blocks, block_text)
        block_start <- block_start[-1]
      }
    }
  }

  # Now parse the blocks
  parsed_jsons <- lapply(blocks, function(json_candidate) {
    tryCatch(
      {
        jsonlite::fromJSON(json_candidate, simplifyDataFrame = FALSE)
      },
      error = function(e) NULL
    )
  })

  # Remove NULL entries
  parsed_jsons <- parsed_jsons[!sapply(parsed_jsons, is.null)]
  return(parsed_jsons)
}
