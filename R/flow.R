#' Create Flow objects to better organize pipelines in R
#'
#' **Flow** objects, as explicitly created by `flow()`, or implicitly by the
#' \code{\link{\%>_\%}} pipe operator are **proto** objects (class-less objects
#' with possible inheritance) that can be combined nicely with pipelines using
#' the specialized flow pipe operator \code{\link{\%>_\%}} (or by using `$`).
#' They allow for encapsulating satellite objects/variables related to the
#' pipeline, and they deal with non-standard evaluations using the tidyeval
#' mechanism automatically with minimal changes required by the user.
#'
#' `enflow()` creates a **Flow** object in the head of a "flow pipeline" in the
#' context of a functional sequence, that is a function that converts an
#' _ad hoc_, single use pipeline into a function reuseable in a different
#' context. Satellite data become arguments of the function.
#'
#' @param . If a **Flow** object is provided, heritate from it, otherwise,
#' create a new **Flow** object heritating from `.GlobalEnv` with `.` as pipe
#' value.
#' @param .value The pipe value to pass to the object (used instead of `.`,
#' in case both are provided).
#' @param ... For `flow()`, named arguments of other objects to create inside
#' the **Flow** object. If the name ends with `_`, then, the expression is
#' automatically captured inside a *quosure** (see [quos_underscore()]).
#' For `print()`, further arguments passed to the delegated `object_print()`
#' function (if it exists inside the **Flow** object), or to the `print()`
#' method of the object inside `.value`.
#' @param x An object (a **Flow** object, or anyting to test if it is a **Flow**
#' object in `is_flow()`).
#' @param name The name of the item to get from a **Flow** object. If `name`
#' starts with two dots (`..`), the item is searched in the **Flow** object
#' itself without inheritance, but the name is stripped of its leading two dots
#' first! If the content is a **quosure**, it is automatically unquoted, and for
#' the assignation version, if name ends with `_`, the expression is
#' automatically converted into a **quosure**.
#' @param value The value or expression to assign to `name` inside the **Flow**
#' object.
#'
#' @details When a **Flow** object is created from scratch, it always inherits
#' from `.GlobalEnv`, no mather where the expression was executed (in fact, it
#' inherits from an empty root **Flow** object itself inheriting from
#' `.GlobalEnv`). This is a deliberate design choice to overcome some
#' difficulties and limitations of **proto** objects, see [proto()].
#' `enflow()` creates a **Flow** object and populates it automatically with all
#' the objects that are present in `env=` (by default, the calling environment).
#' It is primarily intended to be used inside a function, as first instruction
#' of a "flow pipeline". Hence, it collects all function arguments inside that
#' pipeline in a most convenient way.
#' @export
#' @name flow
#' @seealso [str.Flow], [quos_underscore], \code{\link{\%>_\%}}
#' @keywords utilities
#' @concept class-less objects for better R pipelines
#' @examples
#' library(flow)
#' library(dplyr)
#' data(iris)
#'
#' foo <- function(data, x_ = Sepal.Length, y_ = log_SL,
#' fun_ = mean, na_rm = TRUE)
#'   enflow(data) %>_%
#'   mutate(., y_ = log(x_)) %>_%
#'   summarise(., fun_ = fun_(y_,
#'     na.rm = na_rm_)) %>_% .
#'
#' foo(iris)
#'
#' foo(iris, x_ = Petal.Width)
#'
#' foo(iris, x_ = Petal.Width, fun_ = median)
#' # Unfortunately, this does not work, due to limitations of tidyeval's :=
#' #foo(iris, x_ = Petal.Width, fun_ = stats::median)
#'
#' foo2 <- function(., x_ = Sepal.Length, y_ = log_SL, na_rm = TRUE)
#'   enflow(.)
#'
#' foo2
#' foo2(1:10) -> foo_obj
#' ls(foo_obj)
flow <- function(. = NULL, .value = NULL, ...) {
  # If . is a flow object, heritate from it, otherwise, create a new flow
  # object with this value as a starting point
  # Note that all our flow/proto objects are systematically rooted in .GlobalEnv
  # no mather from where they are created (for consistency, and to make them
  # more easy to save and reload).
  flow_class <- c('Flow', 'proto', 'environment')
  if (!is_flow(.)) {
    root <- structure(
      proto(
        .GlobalEnv, envir = child_env2(.GlobalEnv),
        .value = NULL, .call = NULL, .name = "root"),
      class = flow_class)

    if (!missing(.value)) . <- .value
    structure(
      do_call(proto,
        c(list(root, envir = child_env2(root), .value = .),
          quos_underscore(...))
      ),
      class = flow_class)

  } else {# New Flow object, inheriting .value and .call from the parent
    args_list <- list(., envir = child_env2(.))

    if (!missing(.value))
      args_list$.value <- .value

    structure(
      do_call(proto, c(args_list, quos_underscore(...))),
      class = flow_class)
  }
}

#' @export
#' @rdname flow
#' @param env The environment to use for populating the **Flow** object. All
#' objects from this environment are injected into it, with the objects not
#' starting with a dot and ending with an underscore (`_`) automatically
#' converted into `quosures`. The object provided to `.value=` becomes the
#' default value of the `Flow` object, that is, the data transferred to the
#' pipeline.
#' @param objects A character string with the name of the objects from `env`
#' to import into the **Flow** object. If `env` is the calling environment (by
#' default), `.value` is the name of an object, and that name appears in
#' `objects` too, it is excluded from it to avoid importing it twice.
#' from that
enflow <- function(.value, env = caller_env(), objects = ls(env)) {
  # If env is the calling environment and .value is a name, then, the default
  # value of the Flow object is one of the objects in the imported environment.
  # So, do not import it twice and exclude it from objects.
  value_expr <- substitute(.value)
  if (identical(env, caller_env()) && is_name(value_expr) &&
    exists(as_chr(value_expr), envir = env, inherits = FALSE)) {
    keep <- objects != as_chr(value_expr)
    objects <- objects[keep]
  }

  # Create and populate the Flow object
  fl <- flow(.value)
  for (object in objects) {
    l <- nchar(object)
    if (substr(object, l, l) == "_") {
      # Usually not good to parse and evaluate this way, ... but object is
      # already a string, and it appears to be a robust and safe approach here
      expr <- parse(text = paste0("rlang::enquo(", object, ")"))
      env2 <- eval(caller_env(), envir = env)
      fl[[substr(object, 1, l - 1)]] <- eval(expr, envir = env2)
    } else {
      fl[[object]] <- get(object, envir = env, inherits = FALSE)
    }
  }
  fl
}

#' @export
#' @rdname flow
is.flow <- function(x)
  x %is% 'Flow'

#' @export
#' @rdname flow
is_flow <- is.flow

#' @export
#' @rdname flow
as.flow <- function(x, ...)
  UseMethod("as.flow")

#' @export
#' @rdname flow
as_flow <- as.flow

#' @export
as.flow.Flow <- function(x, ...)
  x

#' @export
as.flow.default <- function(x, ...) {
  # TODO: a proto object is not necessarily 'rooted' into .GlobalEnv, while
  # a Flow object is => impossible to solve this! Any idea?
  # A Flow object is really a special king of proto object.
  # So, first convert into proto
  x <- as.proto(x)
  class(x) <- c('Flow', 'proto', 'environment')

  # ..Name (proto) -> .name (Flow)
  if (exists("..Name", envir = x, inherits = FALSE)) {
    x[[".name"]] <- x[["..Name"]]
    x[["..Name"]] <- NULL
  }

  x
}

#' @export
as.proto.Flow <- function(x, ...) {
  # A Flow object really is the same as a proto object. So, only minor changes
  if (!x %is% "proto")
    abort("'x' must inherits from 'proto' class")
  # In Flow, name is in '.name', but it is in '..Name' for proto objects
  if (exists(".name", envir = x, inherits = FALSE)) {
    x[["..Name"]] <- x[[".name"]]
    x[[".name"]] <- NULL
  }

  class(x) <- c('proto', 'environment')
  x
}

#' @export
#' @rdname flow
`$.Flow` <- function(x, name) {
  # This is essentially the same as `$.proto()`, but it unquotes name. Also,
  # if you specify obj$..name, it looks at 'name' in obj WITHOUT inheritance
  # The proto object looks for '..name'. So, you have to specify if the name
  # will be looked for with or without inheritance when the name is defined, not
  # when the object is used, which is something odd!
  #
  # Also, '.' is used a synonym to '.value' to extract the "default" value
  if (name == ".")
    name <- ".value"

  n <- nchar(name)
  if (n > 2 && substr(name, 1, 2) == "..") {
    inherits <- FALSE
    name <- substr(name, 3, n)
  } else inherits <- TRUE

  res <- get(name, envir = x, inherits = inherits)
  res <- get_expr(res) # Make sure to unquote the content of 'name' now

  if (!is_function(res) || deparse(substitute(x)) %in% c('.that', '.super')) {
    res
  } else {
    # Construct a FlowMethod compatible with protoMethods in package proto
    structure(function(...) res(x, ...), class = "FlowMethod", method = res)
  }
}

#' @export
#' @rdname flow
`$<-.Flow` <- function(x, name, value) {
  # The difference with `$<-.proto` is that the flow version assigns a quosure
  # automatically if name ends with '_' (and . is synonym of .value)
  if (name == '.super')
    parent.env(x) <- value

  if (is_function(value))
    environment(value) <- x

  name <- as_chr(substitute(name))

  if (name == ".")
    name <- ".value"

  l <- nchar(name)
  if (substr(name, l, l) == "_") {
    x[[substr(name, 1, l - 1)]] <- as_quosure(value, env = caller_env())
  } else {
    x[[name]] <- value
  }

  x
}

#' @export
print.Flow <- function(x, ...) {
  # Similar to print.proto(), but indicate it is a Flow object
  if (exists('object_print', envir = x, inherits = TRUE)) {
    x$object_print(...)
  } else {
    cat("<Flow object with $.value being>\n")
    print(x$.value, ...)
    invisible(x)
  }
}

#' @export
print.FlowMethod <- function(x, ...) {
  # Similar to proto:::print.protoMethod(), but for Flow objects
  cat("<FlowMethod>\n")
  print(attr(x, "method"), ...)
}
