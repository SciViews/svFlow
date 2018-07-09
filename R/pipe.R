#' Flow pipeline operators and debugging function
#'
#' Pipe operators. \code{\link{\%>.\%}} is a very simple and efficient pipe
#' operator. \code{\link{\%>_\%}} is more complex. It forces convertion to
#' a **Flow** object inside a pipeline and automatically manage non-standard
#' evaluation through creation and unquoting of **quosure**s for named arguments
#' whose name ends with `_`.
#'
#' @param x Value or **Flow** object to pass to the pipeline.
#' @param expr Expression to evaluation in the pipeline.
#'
#' @export
#' @name pipe_operator
#' @details With \code{\%>.\%}, the value must be explicitly indicated with a
#' `.` inside the expression. The expression is **not** modified, but the value
#' is first assigned into the calling environment as `.` (warning! possibly
#' replacing any existing value... do **not** use `.` to name other objects).
#' Also the expression is saved as `.call` in the calling environment so that
#' `debug_flow()` can retrieve are rerun it easily. If a **Flow** object is used
#' with \code{\%>.\%}, the `.value` is extracted from it into `.` first (and thus
#' the **Flow** object is lost).
#'
#' In the case of \code{\%>_\%} the **Flow** object is passed or created, it is
#' also assigned in the calling environment as `..`. This can be used to refer
#' to **Flow** object content within the pipeline expressions (e.g., `..$var`).
#'
#' For \code{\%>_\%}, the expression is reworked in such a way that a suitable
#' lazyeval syntax is constructed for each variable whose name ends with `_`,
#' and that variable is explicitly searched starting from `..`. Thus, `x_` is
#' replaced by `!!..$x`. For such variables appearing at left of an `=` sign, it
#' is also replaced by `:=` to keep correct \R syntax (`var_ =` =>
#' `!!..$var :=`). This way, you just need to follow special variables by `_`,
#' both in the `flow()` function arguments (to create quosures), and to the
#' NSE expressions used inside the pipeline to get the job done! The raw
#' expression is saved as `.call_raw`, while the reworked call is saved as
#' `.call` for possible further inspection and debugging.
#'
#' Finally, for \code{\%>_\%}, if `expr` is `.`, then, the last value from the
#' pipe is extracted from the **Flow** object and returned. It is equivalent,
#' thus, to `flow_obj$.value`.
#'
#' You can mix \code{\%>.\%} and \code{\%>_\%} within the same pipeline. In case
#' you use \code{\%>.\%} with a flow pipeline, it "unflows" it, extracting
#' `.value` from the **Flow** object and further feeding it to the pipeline.
#' @seealso [flow], [quos_underscore]
#' @keywords utilities
#' @concept pipeline operators and debugging
#' @examples
#' # A simple pipeline with %>.% (explicit position of '.' required)
#' library(flow)
#' library(dplyr)
#' data(iris)
#' iris2 <- iris %>.%
#'   mutate(., log_SL = log(Sepal.Length)) %>.%
#'   filter(., Species == "setosa")
#'
#' # The %>.% operator is much faster than magrittr's %>%
#' # (although this has no noticeable impact in most situations when the
#' # pipeline in used in an ad hoc way, outside of loops or other constructs
#' # that call it a larger number of times)
`%>.%` <- function(x, expr) {
  # A simple pipe operator, which requires explicit indication of .
  # It is compatible with wrapr %.>% alias %>.%, except for Flow objects
  # where it extracts x$.value into . first.
  env <- caller_env()
  if (is_flow(x)) {
    env[["."]] <- x$.value
  } else {
    env[["."]] <- x
  }
  env[[".call"]] <- substitute(expr)
  expr
}

# Rework a (nse) expression to operate properly with the tidyeval mechanism in
# the context of a flow pipeline using %>_%
.tidy_flow_expr <- function(expr) {
  # TODO: rework expressions on the parsed tree directly!
  # For now, we stick with regular expressions substitution on the deparsed
  # expression: easier to implement for now. Work on the parsed tree left for
  # when that function will be fully field-tested!
  expr <- deparse(expr)
  # Whenever `var_ =` appears, replace with `!!..$var :=`
  expr <- gsub(
    "(?<![._a-zA-Z0-9])([._a-zA-Z0-9]+)_([ \t]*)=(?!=)",
    "!!..$\\1\\2:=", expr, perl = TRUE)
  # Whenever `var_(` appears (special case for functions), replace with
  # `rlang::eval_tidy(var)(`
  expr <- gsub(
    "(?<![._a-zA-Z0-9])([._a-zA-Z0-9]+)_([ \t]*)\\(",
    "rlang::eval_tidy(..$\\1)\\2(", expr, perl = TRUE)
  # Whenever var_ appears, replace by !!..$var
  expr <- gsub(
    "(?<![._a-zA-Z0-9])([._a-zA-Z0-9]+)_(?![._a-zA-Z0-9])",
    "!!..$\\1", expr, perl = TRUE)
  parse(text = expr)
}

#' @export
#' @rdname pipe_operator
`%>_%` <- function(x, expr) {
  # A more sophisticated pipe operator that can deal nicely with flow objects
  # and tidyverse non-standard evaluation as in rlang (tidyeval)
  if (!is_flow(x))
    x <- flow(x)

  # Special case to return the value out of the Flow object
  expr <- substitute(expr)
  if (expr == ".")
    return(x[[".value"]])

  env <- caller_env()
  env[["."]] <- x[[".value"]]
  env[[".."]] <- x
  on.exit(env[[".call_raw"]] <- expr)

  # Rework the expression to be tidyeval-compatible in the most transparent way
  expr2 <- .tidy_flow_expr(expr)

  on.exit({
    env[[".call"]] <- expr2
    x[[".call"]] <- expr2
  }, add = TRUE)
  x[[".value"]] <- eval(expr2, envir = env)

  x
}

#' @export
#' @rdname pipe_operator
debug_flow <- function() {
  # TODO: cleanup of the calling stack on error!
  # TODO: take into account flow() environment and call reworking and report
  # these clearly to help understand what is done with the %>_% operator!
  env <- caller_env()
  pipe_data <- env[["."]]
  pipe_call <- env[[".call"]]

  if (is_null(pipe_data) || is_null(pipe_call))
    abort("no flow pipe context to debug")

  cat("Last expression run in the pipeline:\n")
  print(pipe_call)

  cat("\nwith . being:\n")
  str(pipe_data)

  cat("\nproducing:\n")
  eval(pipe_call, env)
}
