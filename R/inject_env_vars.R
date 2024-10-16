#' Inject Environment Variables into a Function's Environment
#'
#' This is a helper function used internally within the package to inject values
#' from the environment into a function's environment. It captures all variables
#' from the specified environment (defaulting to the parent environment) and
#' binds them into the function's environment, so they can be accessed during
#' the function's execution.
#'
#' This is particularly useful when you want to use non-standard evaluation
#' tools like `glue::glue()` within the function, and ensure that variables from
#' the calling environment are accessible inside the function.
#'
#' @param f A function whose environment will have variables injected from the
#'   calling environment.
#' @param env The environment from which variables should be taken. Defaults to
#'   the parent frame.
#'
#' @return The modified function with values from the environment injected into
#'   its environment.
#' @keywords internal
.inject_env_vars <- function(f, env = parent.frame()) {
  # Capture all variables from the environment
  vars <- ls(env)

  # Create a named list of values from the environment
  var_values <- mget(vars, envir = env)

  # Create a new environment and assign the variables from var_values
  env_injected <- new.env(parent = environment(f))
  list2env(var_values, envir = env_injected)

  # Set the new environment to the function
  environment(f) <- env_injected

  return(f)
}
