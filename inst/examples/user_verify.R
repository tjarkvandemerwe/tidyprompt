\dontrun{
  "Tell me a fun fact about yourself!" |>
    user_verify() |>
    send_prompt()
  # --- Sending request to LLM provider (gpt-4o-mini): ---
  # Tell me a fun fact about yourself!
  # --- Receiving response from LLM provider: ---
  # I don't have personal experiences or feelings, but a fun fact about me is that
  # I can generate text in multiple languages! From English to Spanish, French, and
  # more, I'm here to help with diverse linguistic needs.
  #
  # ── Evaluation of tidyprompt resulted in:
  # [1] "I don't have personal experiences or feelings, but a fun fact about me is
  # that I can generate text in multiple languages! From English to Spanish, French,
  # and more, I'm here to help with diverse linguistic needs."
  #
  # ── Accept or decline
  # ℹ If satisfied, type nothing
  # ℹ If not satisfied, type feedback to the LLM
  # Type: Needs to be funnier!
  # --- Sending request to LLM provider (gpt-4o-mini): ---
  # Needs to be funnier!
  # --- Receiving response from LLM provider: ---
  # Alright, how about this: I once tried to tell a joke, but my punchline got lost
  # in translation! Now, I just stick to delivering “byte-sized” humor!
  #
  # ── Evaluation of tidyprompt resulted in:
  # [1] "Alright, how about this: I once tried to tell a joke, but my punchline got
  # lost in translation! Now, I just stick to delivering “byte-sized” humor!"
  #
  # ── Accept or decline
  # ℹ If satisfied, type nothing
  # ℹ If not satisfied, type feedback to the LLM
  # Type:
  # ✔ Result accepted
  # [1] "Alright, how about this: I once tried to tell a joke, but my punchline got
  # lost in translation! Now, I just stick to delivering “byte-sized” humor!"
}

