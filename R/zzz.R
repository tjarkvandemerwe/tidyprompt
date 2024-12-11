# Suppress warning about undefined variables which are passed between environments
#   and are not actually undefined
utils::globalVariables(c("self", "schema_instruction", ".data"))
