#' Pass first argument as dot to run code in second argument for pipe operators that do not natively support dot-replacement scheme (base R pipe operator)
#'
#' @param x Object to pass to `expr` as dot (`.`).
#' @param expr Expression to execute, containing `.` as a placeholder.
#'
#' @return The result from executing `expr` in the parent environment.
#' @details The function has a side-effect to assign `x` as `.` and unevaluated `expr` as `.call` in the calling environment. Therefore, make sure you do not use `.` or `.call` there for something else. In case `expr` fails in the middle of a series of chained pipes, you can inspect `.` and `.call` or possibly rerun a modified version of the instruction that failed on it for easier debugging purpose.
#' @export
#' @examples
#' # The function is really supposed to be use in a pipe instruction
#' # This example only runs on R >= 4.1
#' \dontrun{
#' # lm has data = as second argument, which does not fit well with the pipe |>
#' # In R 4.1, one should write:
#' iris |> \(.)(lm(data = ., Sepal.Length ~ Petal.Length + Species))()
#' # which is not very elegant ! With ._() it is more concise and straighforward
#' iris |> ._(lm(data = ., Sepal.Length ~ Petal.Length + Species))
#' }
._ <- function(x, expr) {
  env <- caller_env()
  env[["."]] <- x
  env[[".call"]] <- substitute(expr)
  expr
}
