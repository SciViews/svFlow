#' Data analysis workflow and alternate pipeline operator
#'
#' Data (work)flow analysis using proto objects and pipe operator that
#' integrates non-standard evaluation and the lazyeval mechanism.
#'
#' @section Important functions:
#'
#'- [flow()] constructs a `Flow` object.
#'
#' - [as_quosure()], and unary `+` and `-` operators combined with **formula**
#'   objects provide an alternate way to manipulate **quosure**s.
#'
#' - [quos_underscore()] automatically convert arguments whose name ends with
#'   `_` into **quosure**s, and this mechanism is used by our pipe operators.
#'
#' - [debug_flow()] provides a convenient way to debug problematic pipelines
#'   build with our own pipe operators \code{\link{\%>.\%}} and
#'   \code{\link{\%>+\%}}.
#' @docType package
#' @name flow-package
#'
#' @import proto
#' @importFrom rlang abort warn caller_env empty_env f_env f_env<- f_lhs f_rhs
#'   is_symbolic is_true new_quosure quos !!
#' @importFrom utils capture.output str
NULL


# Non-exported functions --------------------------------------------------

`%is%` <- function(x, what) # This is more expressive!
  inherits(x, what)

is_null <- is.null # rlang::is_null is much slower!

child_env <- function(.parent, ...) {
  # A faster child_env() than rlang::child_env(), but that does not convert
  # .parent and ignores ...
  new.env(parent = .parent)
}

# rlang proposes invoke() in place of do.call(), but it is 100x slower! So:
do_call <- function(what, ...)
  do.call(what, ...)

# rlang::env_parent(env, n = 1) is supposed to replace parent.env(), but it is
# 25x time slower, and we don't need to specify something else than n = 1 here.
# So, we redefine it simply for speed as:
env_parent <- function(env)
  parent.env(env)

# rlang uses ctxt_frame() and call_frame() in place of base::parent.frame() but
# it appears more complex for simple use. Hence call_frame(2)$env is the same as
# parent.frame()... But there is caller_env() as shortcut for the same purpose!

# Again, rlang::is_function is 10x slower than base::is.function(), so:
is_function <- is.function

# The as_character() and as_string() in rlang are difficult to understand. Here
# we simply want a function that (tries) to convert anything into character, as
# as.character() does. So, it is called as_chr()
as_chr <- as.character

# rlang uses env_has() and env_get() in place of exists() and get(), but with
# the environment as first argument (and also cannot specify mode). It can
# extract environments from objects like formulas or quosures, but then, they
# are more than 10x slower than exists() or get() (and get0()). So, for now, I
# stick with exists()/get() in my code...

# Further base/utils functions rename for consistent snake_case notation...
is_chr <- is.character
is_env <- is.environment
stop_if_not <- stopifnot
capture_output <- capture.output

#f_drop_lhs <- function(x) {
#  lhs <- f_lhs(x)
#  if (is_null(lhs)) {
#    x
#  } else {# Drop lhs
#    x[-2]
#  }
#}

.as_quosure_formula <- function(x, env = caller_env()) {
  # A quosure does not have lhs, so, drop if if present (second term)
  lhs <- f_lhs(x)
  if (!is_null(lhs))
    x <- x[-2]

  # A quosure always has an environment, so, fix it if not there
  if (!is_null(env) && is_null(f_env(x)))
    f_env(x) <- env

  class(x) <- c('quosure', 'formula')
  x
}

# name.proto() is not exported from the proto package. So, this is a copy here
.name_flow <- function(., envir = parent.frame()) {
  stop_if_not(is_env(.) || (is_chr(.) && is_env(get(., envir))))

  if (is_env(.)) {
    if (exists('..Name', ., inherits = FALSE)) {
      .[['..Name']]
    } else {
      l <- unlist(eapply(envir, identical, .))
      if (any(l)) {
        names(l[l])[1]
      } else {
        gsub("^.* |>$", "",
          capture_output(
            print.default(.)
          )[[1]]
        )
      }
    }

  } else {
    e <- get(., envir)
    if (exists('..Name', e, inherits = FALSE)) {
      e[['..Name']]
    } else {
      .
    }
  }
}
