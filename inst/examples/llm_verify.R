\dontrun{
  "What is 'Enschede'?!" |>
    answer_as_text(max_words = 50) |>
    llm_verify() |>
    send_prompt()
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # What is 'Enschede'?!
  #
  # You must provide a text response. The response must be at most 50 words.
  # --- Receiving response from LLM provider: ---
  # Enschede is a city in the eastern Netherlands, located in the province of Overijssel.
  # It has a population of around 160,000 people and is known for its textile industry heritage,
  # universities (including the University of Twente), and cultural attractions like
  # the Rietveld Schröder House, a UNESCO World Heritage Site.
  # --- Sending request to LLM provider (llama3.1:8b): ---
  # You are given a user's prompt.
  # To answer the user's prompt, you need to think step by step to arrive at a final answer.
  #
  # ----- START OF USER'S PROMPT -----
  # >>> An assistant was asked:
  #
  # What is 'Enschede'?!
  #
  # You must provide a text response. The response must be at most 50 words.
  #
  # >>> The assistant answered:
  #
  #   [1] "Enschede is a city in the eastern Netherlands, located in the province of Overijssel.
  #   It has a population of around 160,000 people and is known for its textile industry heritage,
  #   universities (including the University of Twente), and cultural attractions like
  #   the Rietveld Schröder House, a UNESCO World Heritage Site."
  #
  # >>> Please verify if the answer is satisfactory.
  #
  # You must answer with only TRUE or FALSE (use no other characters).
  # ----- END OF USER'S PROMPT -----
  #
  # What are the steps you would take to answer the user's prompt?
  # Describe your thought process in the following format:
  #   >> step 1: <step 1 description>
  #   >> step 2: <step 2 description>
  #   (etc.)
  #
  # When you are done, you must type:
  #   FINISH[<put here your final answer to the user's prompt>]
  #
  # Make sure your final answer follows the logical conclusion of your thought process.
  # --- Receiving response from LLM provider: ---
  # >> step 1: Read and understand the original user's prompt, which asks for a text
  # response with at most 50 words.
  # >> step 2: Read and compare the assistant's initial answer to the original user's
  # prompt to see if it meets the specified requirements.
  # >> step 3: Determine if the assistant's answer is within the 50-word limit by counting
  # the number of words in the response.
  # >> step 4: Verify that the content of the assistant's answer provides a concise
  # and relevant description of what "Enschede" is, considering its context and purpose.
  #
  # Based on these steps, I conclude:
  #
  # FINISH[TRUE]
  # [1] "Enschede is a city in the eastern Netherlands, located in the province of Overijssel.
  # It has a population of around 160,000 people and is known for its textile industry heritage,
  # universities (including the University of Twente), and cultural attractions like the
  # Rietveld Schröder House, a UNESCO World Heritage Site."
}
