#' Compactly display the content of a Flow object
#'
#' Print short informative strings about the **Flow** object and all it
#' containts, plus possibly, inheritance information.
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
#' @seealso [flow]
#' @keywords utilities
#' @concept compactly inform about an object
#' @examples
#' # A Flow object
#' data(iris)
#' fl <- flow(iris, x = 1:10, var_ = Sepal.Length)
#' fl # Shows the .value contained into fl
#' str(fl) # Provides compact information about satellite data contained in fl
str.Flow <- function(object, max.level = 1, nest.lev = 0,
indent.str = paste(rep.int(" ", max(0, nest.lev + 1)),
collapse = ".."), ...) {
  # Similar to str.proto(), but indicate it is a Flow object
  cat("Flow", .name_flow(object), "\n")

  lines <- capture_output(
    str(as.list(object), max.level = max.level, nest.lev  = nest.lev, ...)
  )[-1]
  for (line in lines)
    cat(line, "\n")

  p_env <- env_parent((object))
  if (is_proto(p_env)) {
    cat(indent.str, "parent: ", sep = "")
    str(p_env, nest.lev = nest.lev + 1, ...)
  }
}
