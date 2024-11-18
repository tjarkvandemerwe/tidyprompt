\dontrun{
  "What is 5 + 5?" |>
    answer_by_react() |>
    answer_as_integer() |>
    send_prompt(llm_provider_ollama())
  # --- Sending request to LLM provider (llama3.1:8b): ---
  #   You are given a user's prompt.
  #   To answer the user's prompt, you need to think step by step,
  #   take an action if needed, and then return a final answer.
  #
  #   ----- START OF USER'S PROMPT -----
  #   What is 5 + 5?
  #
  #   You must answer with only an integer (use no other characters).
  #   ----- END OF USER'S PROMPT -----
  #
  #   Use the following structure:
  #     Thought: <describe your thought process>
  #     Action: <if needed, describe the action you take (e.g., look up information)>
  #     Observation: <describe the result or observation from the action>
  #   (Repeat Thought -> Action -> Observation as necessary)
  #
  #   When you are done, you must type:
  #   FINISH[<put here your final answer to the user's prompt>]
  #
  #   Ensure your final answer aligns with your reasoning and observations.
  # --- Receiving response from LLM provider: ---
  #   Thought: The problem is asking for the sum of two numbers, 5 and 5.
  #
  #   Action: None needed, as this is a simple arithmetic operation that can be performed mentally.
  #
  #   Observation: I can easily add these two numbers together in my mind to get the result.
  #
  #   Thought: To find the sum, I will simply add the two numbers together: 5 + 5 = ?
  #
  #   Action: Perform the addition.
  #
  #   Observation: The result of adding 5 and 5 is 10.
  #
  #   FINISH[10]
  # [1] 10
}
