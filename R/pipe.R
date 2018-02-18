`%>.%` <- function(x, expr) {
  # TODO: should it be 100% compatible with wrapr %>.%, or flow() aware?
  # TODO: should we force building flow() objects?
  # Our own pipe operator, which requires explicit indication of .
  # It is compatible with wrapr's `%>.%` operator, but slightly faster
  # Now, `$` keeps the same behaviour as with proto objects, but `%>.%` now
  # assigns result of expr to .value inside ..
  if (!is_flow(x))
    x <- flow(x)

  expr2 <- substitute(expr)
  if (expr2 == ".")
    return(x[[".value"]])

  env <- parent.frame()
  env[["."]] <- x[[".value"]]
  env[[".."]] <- x
  env[[".call"]] <- expr2
  x[[".call"]] <- expr2
  x[[".value"]] <- expr
  x
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
