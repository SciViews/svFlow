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
#'   objects provide an alternate way to create **quosure**s.
#'
#' - [quos_underscore()] automatically convert arguments whose name ends with
#'   `_` into **quosure**s, and this mechanism is used by our pipe operators.
#'
#' - [debug_flow()] provides a convenient way to debug problematic pipelines
#'   build with our own pipe operators \code{\link{\%>.\%}} and
#'   \code{\link{\%>_\%}}.
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

`%is%` <- function(x, what) # This is more expressive!
  inherits(x, what)

is_null <- is.null # rlang::is_null is much slower. So, until it is optimized...

# A faster child_env() than rlang::child_env(), but that does not convert
# .parent and ignores ...
child_env2 <- function(.parent, ...)
  new.env(parent = .parent)

# rlang proposes invoke() in place of do.call(), but it is 20x slower!
# So, just to stick with snake_case name convention...
do_call <- function(what, ...)
  do.call(what, ...)

# rlang uses ctxt_frame() and call_frame() in place of base::parent.frame() but
# these appear complex for simple use. Hence call_frame(2)$env is the same as
# parent.frame()... But there is caller_env() as shortcut for the same purpose!

# The as_character() and as_string() in rlang are difficult to understand fo me.
# Here we simply want a function that (tries) to convert anything into
# character, as as.character() does. Since we may end up with something slightly
# different that base::as.character(), we anticipate this change here...
as_chr <- as.character

# rlang uses env_has() and env_get() in place of exists() and get(), but with
# the environment as first argument (and also cannot specify mode). It can
# extract environments from objects like formulas or quosures, but then, they
# are more than 10x slower than exists() or get() (and get0()). So, for now, I
# stick with exists()/get() in my code... to be rechecked later on.

# Further base/utils functions rename for consistent snake_case notation...
is_chr <- is.character
is_env <- is.environment
stop_if_not <- stopifnot
capture_output <- capture.output
is_name <- is.name
is_proto <- is.proto

# Not used for now, but may be useful in the future
#f_drop_lhs <- function(x) {
#  lhs <- f_lhs(x)
#  if (is_null(lhs)) {
#    x
#  } else {# Drop lhs
#    x[-2]
#  }
#}

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

# Adapted from name.proto()
.name_flow <- function(., envir = caller_env()) {
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
