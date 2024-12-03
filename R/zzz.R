# Suppresses RCMD-check warning about 'self' being undefined in 'complete_chat'
#   functions (e.g., in llm_provider_openai()) that will be assigned the environment
#   of the R6 object (RCMD-check does not realise this and thinks a variable is undefined)
utils::globalVariables(c("self"))
# Similarly suppressing warning about 'schema_instruction' which is passed
#   between environments in answer_as_json()
utils::globalVariables(c("schema_instruction"))
