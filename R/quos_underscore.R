#' Convert arguments whose names end with _ into quosures automatically
#'
#' The expressions provided for all arguments whose names end with `_` are
#' automatically converted into **quosure**s, and also assigned to a name
#' without the training `_`. The other arguments are evaluated in an usual way.
#'
#' @param ... The named arguments provided to be either converted into quosures
#' or evaluated.
#'
#' @return An object of class **quosures** is returned. It can be used directly
#' in tidyeval-aware contexts.
#' @export
#' @seealso [as.quosure], \code{\link{\%>_\%}}
#' @keywords utilities
#' @concept automatic quosures creation for non-standard evaluation
#' @examples
#' foo <- function(...)
#'   quos_underscore(...)
#' foo(x  = 1:10, # "Normal" argument
#'     y_ = 1:10, # Transformed into a quosure
#'     z_ = non_existing_name) # Expressions in quosures are not evaluated
quos_underscore <- function(...) {
  # Transform into closures only those items whose name ends with _
  # (and also assign them to names without the _)
  #
  # rlang does not export dots_capture() that we could use here, and list(...)
  # does evaluate all arguments in ... So, one (suboptimal) solution is to
  # convert all ... arguments into quosures using rlang::quos(), and then, to
  # evaluate those in the list whose name does not end with '_'. The penalty is
  # negligible and allows to reuse rlang::quos(), which is a larger benefit than
  # reimplementing (and maintaining!) a slightly different version here.
  dots <- quos(...)
  dots_names <- names(dots)
  l_names <- nchar(dots_names)
  last_char <- substr(dots_names, l_names, l_names)
  ends_with_underscore <- last_char == "_"

  env <- caller_env(2L)

  for (name in dots_names[ends_with_underscore]) {
    dots[[substr(name, 1L, nchar(name) - 1L)]] <- dots[[name]]
    dots[[name]] <- NULL
  }

  for (name in dots_names[!ends_with_underscore])
    dots[[name]] <- eval_tidy(dots[[name]], env = env)

  dots
}
