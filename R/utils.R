#' Transform a formula into a quosure
#'
#' Current implementation of **quosure**s is done by subclassing **formula**s in
#' {rlang} 0.2. However, this may change in the future. The
#' `.as_quosure.formula()` method is the workhorse function that does the
#' conversion in `-~expr`, and could be adapted in future versions of **rlang**,
#' if they use something else than **formula**s to implement **quosure**s.
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
    x <- x[-2L]

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
  if (is_chr(x))
    x <- get(x, env)

  if (!is_env(x))
    abort("'x' must be an environment, or a string with its name")

  if (exists('.name', x, inherits = FALSE)) {
    x[['.name']]
  } else {
    l <- unlist(eapply(env, identical, x))
    if (any(l)) {
      names(l[l])[1L]
    } else {
      gsub("^.* |>$", "",
        capture_output(
          print.default(x)
        )[[1L]]
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
capture_output <- capture.output
is_name <- is.name
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
