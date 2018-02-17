flow <- function(. = NULL, .value = NULL, ...) {
  # If . is a flow object, heritate from it, otherwise, create a new flow
  # object with this value as a starting point
  # Note that all our flow/proto objects are systematically rooted in .GlobalEnv
  # no mather from where they are created (for consistency)
  flow_class <- c('flow', 'proto', 'environment')
  if (!is_flow(.)) {
    root <- structure(proto(.GlobalEnv, envir = child_env(.GlobalEnv),
      .value = NULL, .call = NULL),
      class = flow_class)
    if (!missing(.value)) . <- .value
    structure(do_call(proto,
      c(list(root, envir = child_env(root), .value = .),
        quos_underscore(...))
      ),
      class = flow_class)

  } else {# New flow, inheriting .value and .call from our parent
    args_list <- list(., envir = child_env(.))
    if (!missing(.value))
      args_list$.value <- .value
    structure(do_call(proto,
      c(args_list, quos_underscore(...)),
      class = flow_class))
  }
}

is_flow <- function(x)
  x %is% 'flow'

is.flow <- is_flow

`$.flow` <- function(x, name) {
  # This is essentially the same as `$.proto()`, but it unquotes name. Also,
  # if you specify obj$..name, it looks at 'name' in obj WITHOUT inheritance
  # The proto object look for '..name' without inheritance. So, you have to
  # specify if the name will be looked for with or without inheritance before
  # use, which is something we consider odd
  n <- nchar(name)
  if (n > 2 && substr(name, 1, 2) == "..") {
    inherits <- FALSE
    name <- substr(name, 3, n)
  } else inherits <- TRUE

  res <- get(name, envir = x, inherits = inherits)

  res <- `!!`(res) # Make sure to unquote the content of name now

  if (!is.function(res))
    return(res)

  if (deparse(substitute(x)) %in% c('.that', '.super'))
    return(res)

  # Construct a protoMethod compatible with proto package
  structure(function(...) res(x, ...), class = 'protoMethod', method = res)
}

`$<-.flow` <- function(this, s, value) {
  # The difference with `$<-.proto` is that the flow version assigns a quosure
  # if name ends with '_'
  if (s == '.super')
    env_parent(this) <- value

  if (is_function(value))
    environment(value) <- this

  s <- as_chr(substitute(s))
  l <- nchar(s)

  if (substr(s, l, l) == "_") {
    this[[s]] <- as_quosure(value, env = caller_env())
  } else {
    this[[s]] <- value
  }
  this
}

print.flow <- function(x, ...) {
  # Similar to print.proto(), but with indicate it is a flow object
  if (exists('proto_print', envir = x, inherits = TRUE)) {
    x$proto_print(...)
  } else {
    cat("<flow object with $.value being>\n")
    print(x$.value)
    invisible(x)
  }
}

str.flow <- function(object, max.level = 1, nest.lev = 0,
  indent.str = paste(rep.int(" ", max(0, nest.lev + 1)), collapse = ".."), ...) {
  # Same as str.proto(), but indicate it is a flow object
  cat("flow", proto:::name.proto(object), "\n")
  Lines <- utils::capture.output(utils::str(
    as.list(object), max.level = max.level,
    nest.lev = nest.lev, ...
  ))[-1]
  for (s in Lines)
    cat(s, "\n")
  if (is.proto(parent.env(object))) {
    cat(indent.str, "parent: ", sep = "")
    utils::str(parent.env(object), nest.lev = nest.lev + 1, ...)
  }
}

`%>.%` <- function(x, expr) {
  # Our own pipe operator, which requires explicit indication of .
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
  expr2 <- sub(
    "^([.a-zA-Z][._a-zA-Z0-9]*\\s*\\()\\s*([^.]|\\.\\s*[^,])", "\\1., \\2",
    expr2)
  expr2 <- parse(text = expr2)

  env[[".call"]] <- expr2
  x[[".call"]] <- expr2
  x[[".value"]] <- eval(expr2, envir = env)
  x
}

# TODO: for the rest, I still have to work this out!!!
# Our pipeline is easily transformable into a function for reuse:
#flow_function <- function(. = NULL, .value = NULL, ...) {
#  # Create a flow function instead of a flow/proto object
#  # . if provided, is ignored! First argument is replaced by .data
#  function(.data, ...) {
#    # Create quosures with all variables ending with _
#    list2env(quos_underscore(...), envir = environment())
#    str(environment())
#  }
#}
