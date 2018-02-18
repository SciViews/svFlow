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
