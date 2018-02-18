#' Create Flow objects to better organize pipelines in R
#'
#' **Flow** objects, as explicitly created by `flow()`, or implicitly by the
#' \code{\link{\%>+\%}} pipe operator are **proto** objects (class-less objects
#' with possible inheritance) that can be compbined nicely with pipelines using
#' the specialized flow pipe operators \code{\link{\%>.\%}} and
#' \code{\link{\%>+\%}} (or by using `$`). They allow to encapsulated temporary
#' variables related to the pipeline, and they automate the encapsulation of
#' non-standard evaluations automatically with minimal changes required by the
#' user (in comparison to the **rlang** tidy evalation mechanism).
#'
#' @param . If a **Flow** object is provided, herite from it, otherwise, create
#' a new **Flow** object heritating from `.GlobalEnv` with `.` as pipe value.
#' @param .value The pipe value to pass to the object (used in priority to `.`,
#' in case both are provided).
#' @param ... For `flow()`, named arguments of other objects to create inside the
#' **Flow**. If the name ends with `_`, then, the expression is automatically
#' captured inside a *$quosure** (see [quos_underscore()]). For `print()`,
#' further arguments passed to the delegated `print_proto()` function (if it
#' exists inside the **Flow** object), or to the `print()` method of the object
#' inside `.value`.
#' @param x An object (a **Flow** object, or anyting to test if it is a **Flow**
#' object in `is_flow()`).
#' @param name The name of the item to get from a **Flow** object. If `name`
#' starts with two dots (`..`), the item is searched in the **Flow** object
#' itself without inheritance (like for **proto** objects), but _the name is
#' stripped from its leading two dots first_! If the content is a **quosure**,
#' it is automatically unquoted, and for the assignation version, if name ends
#' with `_`, the extression is automatically converted into a **quosure**.
#' @param value The value or expression to assign to `name` inside the **Flow**
#' object.
#'
#' @details When a **Flow** object is created from scratch, it always inherits
#' from `.GlobalEnv`, no mather where the expression was executed (in fact, it
#' inherits from a root **proto** object itself inheriting from `.GlobalEnv`).
#' This is a design strategy to overcome some difficulties and limitations of
#' **proto** objects, see [proto()].
#' @export
#' @name flow
#' @seealso [str.Flow()], [quos_underscore()], \code{\link{\%>+\%}}
#' @keywords utilities
#' @concept class-less objects for better R pipelines
#' @examples
#' # TODO...
flow <- function(. = NULL, .value = NULL, ...) {
  # If . is a flow object, heritate from it, otherwise, create a new flow
  # object with this value as a starting point
  # Note that all our flow/proto objects are systematically rooted in .GlobalEnv
  # no mather from where they are created (for consistency)
  flow_class <- c('Flow', 'proto', 'environment')
  if (!is_flow(.)) {
    root <- structure(
      proto(
        .GlobalEnv, envir = child_env(.GlobalEnv),
        .value = NULL, .call = NULL),
      class = flow_class)

    if (!missing(.value)) . <- .value
    structure(
      do_call(proto,
        c(list(root, envir = child_env(root), .value = .),
          quos_underscore(...))
      ),
      class = flow_class)

  } else {# New Flow object, inheriting .value and .call from the parent
    args_list <- list(., envir = child_env(.))

    if (!missing(.value))
      args_list$.value <- .value

    structure(
      do_call(proto, c(args_list, quos_underscore(...)),
      class = flow_class))
  }
}

#' @export
#' @rdname flow
is_flow <- function(x)
  x %is% 'Flow'

#' @export
#' @rdname flow
is.flow <- is_flow

#' @export
#' @rdname flow
`$.Flow` <- function(x, name) {
  # This is essentially the same as `$.proto()`, but it unquotes name. Also,
  # if you specify obj$..name, it looks at 'name' in obj WITHOUT inheritance
  # The proto object look for '..name' without inheritance. So, you have to
  # specify if the name will be looked for with or without inheritance when the
  # name is defined, not when the object is used, which is something odd!
  n <- nchar(name)
  if (n > 2 && substr(name, 1, 2) == "..") {
    inherits <- FALSE
    name <- substr(name, 3, n)
  } else inherits <- TRUE

  res <- get(name, envir = x, inherits = inherits)

  res <- `!!`(res) # Make sure to unquote the content of 'name' now

  if (!is.function(res))
    return(res)

  if (deparse(substitute(x)) %in% c('.that', '.super'))
    return(res)

  # Construct a protoMethod compatible with proto package
  structure(function(...) res(x, ...), class = 'protoMethod', method = res)
}

#' @export
#' @rdname flow
`$<-.Flow` <- function(x, name, value) {
  # The difference with `$<-.proto` is that the flow version assigns a quosure
  # automatically if name ends with '_'
  if (name == '.super')
    parent.env(x) <- value

  if (is_function(value))
    environment(value) <- x

  name <- as_chr(substitute(name))
  l <- nchar(name)

  if (substr(name, l, l) == "_") {
    x[[name]] <- as_quosure(value, env = caller_env())
  } else {
    x[[name]] <- value
  }

  x
}

#' @export
#' @rdname flow
print.Flow <- function(x, ...) {
  # Similar to print.proto(), but indicate it is a Flow object
  if (exists('proto_print', envir = x, inherits = TRUE)) {
    x$proto_print(...)
  } else {
    cat("<Flow object with $.value being>\n")
    print(x$.value, ...)
    invisible(x)
  }
}

#' Compactly display the content of a Flow object
#'
#' Print short informative strings about the **Flow** object and all it
#' containts, plus possibly, inheritage information.
#'
#' @param object A **Flow** object.
#' @param max.level The maximum nesting level to use for displaying nested
#' structures.
#' @param nest.lev Used internally for pretty printing nested objects (you
#' probably don't want to change default value).
#' @param indent.str Idem.
#' @param ... Further arguments passed to `str()` methods of **Flow** items.
#'
#' @export
#' @seealso [flow()]
#' @keywords utilities
#' @concept compactly inform about an object
#' @examples
#' # TODO...
str.Flow <- function(object, max.level = 1, nest.lev = 0,
indent.str = paste(rep.int(" ", max(0, nest.lev + 1)), collapse = ".."), ...) {
  # Same as str.proto(), but indicate it is a Flow object
  cat("Flow", .name_flow(object), "\n")

  lines <- capture_output(
    str(
      as.list(object),
      max.level = max.level,
      nest.lev  = nest.lev,
      ...)
    )[-1]

  for (line in lines)
    cat(line, "\n")

  if (is.proto(parent.env(object))) {
    cat(indent.str, "parent: ", sep = "")

    str(parent.env(object), nest.lev = nest.lev + 1, ...)
  }
}

# TODO: for the rest, I still have to work this out!!!
# Our pipeline is easily transformable into a function for reuse:
#flow_function <- function(. = NULL, .value = NULL, ...) {
#  # Create a flow function instead of a Flow/proto object
#  # . if provided, is ignored! First argument is replaced by .data
#  function(.data, ...) {
#    # Create quosures with all variables ending with _
#    list2env(quos_underscore(...), envir = environment())
#    str(environment())
#  }
#}
