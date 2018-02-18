#' Flow pipeline operators and debugging function
#'
#' Pipe operators. The simple one with no forcing to **Flow** objects is `%>.%`.
#' `%>+%` forces convertion to **Flow** and autoamtically manage non-standard
#' evaluation through creation and unquoting of **quosure**s for named arguments
#' whose name ends with `_`.
#'
#' @param x Value or **Flow** object to pass to the pipeline.
#' @param expr Expression to evaluation in the pipeline.
#'
#' @export
#' @name pipe
#' @details With `%>.%`, the value must be explicitly indicated with a `.`
#' inside the expression. The expression is **not** modified, but the value is
#' first assigned into the calling environment as `.` (warning! possibly
#' replacing any existing value... do **not** use `.` to name other objects).
#' Also the expression is saved as `.call` in the calling environment so that
#' `debug_flow()` can retrieve are rerun it easily.
#'
#' In the case of **Flow** objects, it is also assigned in the calling
#' environment as `..`.
#'
#' For `%>+%`, the expression is reworked like this. First, `++` is interpreted
#' as "get from the **Flow** object, or inherited environment, and unquote
#' expression"; `..` is interpreted as "get from the **Flow** object without
#' inheritage and unquote expression", and finally, if the expression starts by
#' calling a regular function name, without specifying `.` as first argument, it
#' is added. The raw expression is saved as `.call_raw`, while the reworked call
#' is saved as `.call` for possible further inspection and debugging.
#'
#' Finally, for both `%>.%` and `%>+%`, if `x` is a **Flow** object, and `expr`
#' is `.`, then, the last value from the pipe is extracted from the **Flow**
#' object and returned. It is equivalent, thus, to `flow_obj$.value`.
#' @seealso [flow()], [quos_underscore()]
#' @keywords utilities
#' @concept pipeline operators and debugging
#' @examples
#' # TODO...
debug_flow <- function() {
  # TODO: take into account flow() environment and call reworking and report
  # these clearly to better understand what is done with the %>+% operator!
  env <- caller_env()
  pipe_data <- env[["."]]
  pipe_call <- env[[".call"]]

  if (is_null(pipe_data) || is.null(pipe_call))
    abort("no flow pipe context to debug")

  cat("Last expression run in the pipeline:\n")
  print(pipe_call)

  cat("\nwith . being:\n")
  str(pipe_data)

  cat("\nproducing:\n")
  eval(pipe_call, env)
}

#' @export
#' @rdname pipe
`%>.%` <- function(x, expr) {
  # TODO: should it be 100% compatible with wrapr %>.%, or flow() aware?
  # Our own pipe operator, which requires explicit indication of .
  # It is compatible with wrapr's `%>.%` operator, but slightly faster when x
  # is *not* a Flow object, otherwise, it is flow-aware, and behaves differently
  #
  # Now, `$` keeps the same behaviour as with proto objects, but `%>.%` now
  # assigns result of expr to .value inside .., in case of a Flow object
  expr2 <- substitute(expr)
  env <- caller_env()
  env[[".call"]] <- expr2

  if (is_flow(x)) {
    if (expr2 == ".")
      return(x[[".value"]])

    env[["."]] <- x[[".value"]]
    env[[".."]] <- x
    x[[".call"]] <- expr2
    x[[".value"]] <- expr

    x

  } else {# Not a Flow object
    env[["."]] <- x

    expr
  }
}

#' @export
#' @rdname pipe
`%>+%` <- function(x, expr) {
  # A more sophisticated pipe operator that can deal nicely with flow objects
  # and tidyverse non-standard evaluation as in rlang and tidyeval
  # Alternate version, allowing ++var as a synonym of ..$var and
  # ++var_ as UQ(..$var_)
  if (!is_flow(x))
    x <- flow(x)

  expr2 <- substitute(expr)
  if (expr2 == ".")
    return(x[[".value"]])

  env <- caller_env()
  env[["."]] <- x[[".value"]]
  env[[".."]] <- x
  env[[".call_raw"]] <- expr2

  # Need to surround ..$var_ with UQ(), and perform other "magics" arround NSE
  # TODO: do that on the parsed tree directly!
  expr2 <- deparse(expr2)
  expr2 <- gsub("\\+\\+", "..$", expr2)
  expr2 <- gsub(
    "(?<![._a-zA-Z0-9])(\\.\\.[._a-zA-Z0-9]+)(?![._a-zA-Z0-9])", "..$\\1",
    expr2, perl = TRUE)
  expr2 <- gsub(
    "(?<![._a-zA-Z0-9])(\\.\\.\\$[._a-zA-Z0-9]+_)(?![._a-zA-Z0-9])", "UQ(\\1)",
    expr2, perl = TRUE)
  # This is for the dot-allergic => allow to omit ., if expr starts with 'fun('
  # TODO: should I keep this, or should I impose to *always* indicate '.'?
  expr2 <- sub(
    "^([.a-zA-Z][._a-zA-Z0-9]*\\s*\\()\\s*([^.]|\\.\\s*[^,])", "\\1., \\2",
    expr2)
  expr2 <- parse(text = expr2)

  env[[".call"]] <- expr2
  x[[".call"]] <- expr2
  x[[".value"]] <- eval(expr2, envir = env)

  x
}
