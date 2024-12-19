#' @title PersistentChat R6 class
#' @name persistent_chat-class
#'
#' @description
#' A class for managing a persistent chat with a large language model (LLM).
#'
#' While 'tidyprompt' is primariy focused on automatic interactions with
#' LLMs through [send_prompt()] using a [tidyprompt-class] object with
#' [prompt_wrap()], this class may be useful for having a manual conversation
#' with an LLM. (It may specifically be used to continue a chat history which was
#' returned by [send_prompt()] with `return_mode = "full"`.)
#'
#' @example inst/examples/persistent_chat.R
#'
#' @seealso [llm_provider-class] [chat_history()]
NULL

#' @rdname persistent_chat-class
#' @export
`persistent_chat-class` <- R6::R6Class(
  "PersistentChat",
  public = list(

    #' @field chat_history A [chat_history()] object
    chat_history = NULL,
    #' @field llm_provider A [llm_provider-class] object
    llm_provider = NULL,

    #' @description Initialize the PersistentChat object
    #'
    #' @param llm_provider A [llm_provider-class] object
    #' @param chat_history (optional) A [chat_history()] object
    #'
    #' @return The initialized PersistentChat object
    initialize = function(
      llm_provider, chat_history = NULL
    ) {
      if (
        is.list(chat_history)
        && !is.data.frame(chat_history)
        && "chat_history" %in% names(chat_history)
      ) {
        chat_history <- chat_history$chat_history |> chat_history()
      }

      if (!is.null(chat_history)) {
        chat_history <- chat_history(chat_history)
        self$chat_history <- chat_history
      }

      if (!inherits(llm_provider, "LlmProvider")) {
        stop("The 'llm_provider' argument must be of class 'LlmProvider'")
      }

      self$llm_provider <- llm_provider
      self
    },

    #' @description Add a message to the chat history and get a response from the LLM
    #'
    #' @param msg Message to add to the chat history
    #' @param role Role of the message
    #' @param verbose Whether to print the interaction to the console
    #'
    #' @return The response from the LLM
    chat = function(
      msg,
      role = "user",
      verbose = TRUE
    ) {
      self$chat_history <- add_msg_to_chat_history(
        self$chat_history,
        msg,
        role = role
      )
      stopifnot(
        is.character(msg), length(msg) == 1, is.logical(verbose)
      )

      llm_provider <- self$llm_provider$clone()
      llm_provider$verbose <- verbose

      response <- llm_provider$complete_chat(self$chat_history)
      self$chat_history <- response$completed

      if (verbose)
        return(invisible(response))

      return(response)
    },

    #' @description Reset the chat history
    #'
    #' @return NULL
    reset_chat_history = function() {
      self$chat_history <- NULL
    }
  )
)
