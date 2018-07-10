#' Data analysis workflow and alternate pipeline operator
#'
#' Data (work)flow analysis using proto objects and pipe operator that
#' integrates non-standard evaluation and the tidyeval mechanism in a most
#' transparent way.
#'
#' @section Important functions:
#'
#' - \code{\link{\%>.\%}} and \code{\link{\%>_\%}} are two alternate pipe
#'   operators designed to supplement magrittr's \%>\% operator in the
#'   tidyverse and elsewhere. They are provided for good reasons.
#'   \code{\link{\%>.\%}} requires explicit indication of the position of `.`
#'   in the pipeline expression **all the time**. The expression is not
#'   modified. As a consequence, it can never surprise you with an unexpected
#'   behaviour, and all valid R expressions are useable in the pipeline. Another
#'   consequence: it is very fast. \code{\link{\%>_\%}} works with **FLow**
#'   objects that allow for encapsulation of satellite objects (data or
#'   functions) within the pipeline. It is self-contained. The pileine can be
#'   interrupted and restarted at any time. It also allows for a class-less
#'   object-oriented approach with single inheritance (could be useful to test
#'   easily differents scenarios on the same pipeline and to prototype objects
#'   that are "pipe-aware"). It also manages the tidyeval mechanism for
#'   non-standard expressions in the most transparent way: the only "rule" to
#'   remember is to suffix the name of variables that needs special treatment
#'   with an undercore (`_`) and the pipe operator manages the rest for you.
#'
#' - [debug_flow()] provides a convenient way to debug problematic pipelines
#'   build with our own pipe operators \code{\link{\%>.\%}} and
#'   \code{\link{\%>_\%}} in a comfortable way. Everything from the step that
#'   raised a error is available: the piped data, the expression to be
#'   evaluated, and possibly, the last state of the **Flow** object. Everything
#'   can be inspected, modified, and the expression can be rerun as if you were
#'   still right in the middle of the pipeline evaluation.
#'
#'- [flow()] constructs a `Flow` object that is pipe-aware and tidyeval-aware.
#'  This opens new horizons in your analysis workflow. You start building a
#'  simple _ad hoc_ pipeline, then you can include satellite data or functions
#'  right inside it, perhaps also test different scenarios by using the object
#'  inheritance features of **Flow** (common parts are shared among the diferent
#'  scenarios, thus reducing the memory foortprint). While your pipeline matures
#'  you gradually and naturally move towards either a functional sequence or a
#'  dedicated object. The functional sequence pathway consists in building a
#'  reuseable function to recycle you pipeline in a different context. The
#'  object pathway is not fully developped yet in the present version. But in
#'  the future, the object-oriented nature of **Flow** will also be leveraged,
#'  so that you could automatically translate your "flow pipeline" into an S3 or
#'  R6 object with satellite data becoming object attributes, and satellite
#'  functions becoming methods. The pipeline itself would then become the
#'  default method for that object. Of course, both functions and objects
#'  derived from a "flow pipeline" will be directly compatible with the tidyeval
#'  mechanism, as they will be most tidyverse-friendly as possible per
#'  construction.
#'
#' - [str.Flow()] compactly displays the content of a **Flow** object.
#'
#' - [as_quosure()], and unary `+` and `-` operators combined with **formula**
#'   objects provide an alternate way to create **quosure**s.
#'
#' - [quos_underscore()] automatically converts arguments whose name ends with
#'   `_` into **quosure**s, and this mechanism is used by our flow pipe
#'   operator to implement the tidyeval mechanism most transparently inside
#'   "flow pipelines".
#' @docType package
#' @name flow-package
#'
#' @import proto
#' @importFrom rlang abort warn caller_env empty_env env_parent f_env f_env<-
#'   f_lhs f_rhs is_function is_symbolic is_true new_quosure quos enquo !!
#'   eval_tidy get_expr quo_get_env quo_get_expr
#' @importFrom utils capture.output str
NULL


# Non-exported functions --------------------------------------------------

#' Transform a formula into a quosure
#'
#' Current implementation of **quosure**s is done by subclassing **formula**s in
#' **rlang** 0.2. However, this may change in the future. The
#' `.as_quosure.formula()` method is the workhorse function that does the
#' conversion in `-~expr`, and could be adapted in future versions of **rlang**,
#' if they use something else than **formua**s to implement **quosure**s.
#'
#' @param x A one-sided formula.
#' @param env The environment to associate with the expression inside the
#' **quosure** (by default, it is the environment here the code is run). This
#' could be `NULL`, and then, the environment currently assoiated with the
#' **formula** is used (fastest way).
#' @return A **quosure**
#' @noRd
#' @examples
#' .as_quosure(~a_name)
#' # should be equivalent to:
#' -~a_name
.as_quosure_formula <- function(x, env = caller_env()) {
  # A quosure does not have lhs, so, drop it if present (second term)
  lhs <- f_lhs(x)
  if (!is_null(lhs))
    x <- x[-2]

  # A quosure always has an environment, so, fix it if not there
  if (!is_null(env) && is_null(f_env(x)))
    f_env(x) <- env

  class(x) <- c('quosure', 'formula')
  x
}

#' Get the name of a Flow object
#'
#' Adapted from proto:::name.proto(), which is not exported. Here the name of
#' the object is `.name`, instead of `..Name` for the **proto** object.
#'
#' @param x An environment (but most probably, a **Flow** object), or the name
#' of such an object.
#' @param env The environment to look for `x`, in case it is a character string.
#' @return A character string with the name of the object.
#' @noRd
#' @examples
#' fl1 <- flow(letters)
#' flow:::.name_flow(fl1)
#'
#' fl2 <- flow(1:10, .name = "My Flow object")
#' flow:::.name_flow("fl2")
.name_flow <- function(x, env = caller_env()) {
  stop_if_not(is_env(x) || (is_chr(x) && is_env(x <- get(x, env))))

  if (exists('.name', x, inherits = FALSE)) {
    x[['.name']]
  } else {
    l <- unlist(eapply(env, identical, x))
    if (any(l)) {
      names(l[l])[1]
    } else {
      gsub("^.* |>$", "",
        capture_output(
          print.default(x)
        )[[1]]
      )
    }
  }
}

#' Test if an object belongs to one or more classes
#'
#' Is 'x' inheriting from one of given classes? Essentially similar to
#' `inherits(x, "class")` but more expressive.
#'
#' @param x Any object.
#' @param what A character string with one or more classes to check.
#' @return `TRUE` or `FALSE` depending on the result of the test
#' @noRd
#' @examples
#' "a string" %is% "character" # TRUE
#' "a string" %is% "logical"   # FALSE
#' 1:10 %is% c("integer", "numeric") # TRUE, because it is integer
`%is%` <- function(x, what) # This is more expressive!
  inherits(x, what)

# A faster child_env() than rlang::child_env(), but that does not convert
# .parent and ignores ...
# See ?rlang::child_env for further help
child_env2 <- function(.parent, ...)
  new.env(parent = .parent)

# rlang proposes invoke() in place of do.call(), but it is 20x slower!
# So, just to stick with snake_case name convention...
# For hel, see ?do.call
do_call <- function(what, ...)
  do.call(what, ...)

# Further base/utils functions rename for consistent snake_case notation...
# For help, see respective functions
is_chr <- is.character
is_env <- is.environment
stop_if_not <- stopifnot
capture_output <- capture.output
is_name <- is.name
is_proto <- is.proto
is_null <- is.null # rlang::is_null is much slower. So, until it is optimized...

# The as_character() and as_string() in rlang are difficult to understand fo me.
# Here we simply want a function that (tries) to convert anything into
# character, as as.character() does. Since we may end up with something slightly
# different that base::as.character(), we anticipate this change here...
as_chr <- as.character

# rlang uses ctxt_frame() and call_frame() in place of base::parent.frame() but
# these appear complex for simple use. Hence call_frame(2)$env is the same as
# parent.frame()... But there is caller_env() as shortcut for the same purpose!

# rlang uses env_has() and env_get() in place of exists() and get(), but with
# the environment as first argument (and also cannot specify mode). It can
# extract environments from objects like formulas or quosures, but then, they
# are more than 10x slower than exists() or get() (and get0()). So, for now, I
# stick with exists()/get() in my code... to be rechecked later on.

# Not used for now, but may be useful in the future
#f_drop_lhs <- function(x) {
#  lhs <- f_lhs(x)
#  if (is_null(lhs)) {
#    x
#  } else {# Drop lhs
#    x[-2]
#  }
#}
