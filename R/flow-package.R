#' Data analysis workflow and alternate pipeline operator
#'
#' Data (work)flow analysis using proto objects and pipe operator that
#' integrates non-standard evaluation and the lazyeval mechanism.
#'
#' @section Important functions:
#'
#'- [flow()] constructs a `flow` object.
#'
#' @docType package
#' @name flow-package
#'
#' @import proto
#' @importFrom rlang abort warn caller_env empty_env f_env `f_env<-` f_lhs
#'   is_symbolic is_true, new_quosure quos `!!`
#' @importFrom utils capture.output str
NULL

# These function are not exported
`%is%` <- inherits # This is more expressive!

is_null <- is.null # rlang::is_null is much slower!

child_env <- function(.parent, ...) {
  # A faster child_env() than rlang::child_env(), but that does not convert
  # .parent and ignores ...
  new.env(parent = .parent)
}

# rlang proposes invoke() in place of do.call(), but it is 100x slower! So:
do_call <- do.call

# rlang::env_parent(env, n = 1) is supposed to replace parent.env(), but it is
# 25x time slower, and we don't need to specify something else than n = 1 here.
# So, we redefine it simply for speed as:
env_parent <- parent.env

# rlang uses ctxt_frame() and call_frame() in place of base::parent.frame() but
# it appears more complex for simple use. Hence call_frame(2)$env is the same as
# parent.frame()... So, I keep parent;frame() for now!

# Again, rlang::is_function is 10x slower than base::is.function(), so:
is_function <- is.function

# The as_character() and as_string() in rlang are difficult to understand. Here
# we simply want a function that (tries) to convert anything into character, as
# as.character() does. So, it is called as_chr()
as_chr <- as.character

# Further base/utils functions rename for consistent snake_case notation...
is_chr <- is.character
is_env <- is.environment
stop_if_not <- stopifnot
capture_output <- capture.output

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
