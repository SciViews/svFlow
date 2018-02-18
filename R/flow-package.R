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

# Again, rlang::is_function is 10x slower than base::is.function(), so:
is_function <- is.function

# The as_character() and as_string() in rlang are difficult to understand. Here
# we simply want a function that (tries) to convert anything into character, as
# as.character() does. So, it is called as_chr()
as_chr <- as.character

# name.proto() is not exported from the proto package. So, this is a copy here
.name_flow <- function(., envir = parent.frame()) {
  stopifnot(is.environment(.) || (is.character(.) &&
    is.environment(get(., envir))))

  if (is.environment(.)) {
    if (exists("..Name", ., inherits = FALSE)) {
      .$..Name
    } else {
      L <- unlist(eapply(envir, identical, .))
      if (any(L))
        names(L[L])[1]
      else gsub("^.* |>$", "", capture.output(print.default(.))[[1]])
    }

  } else {
    e <- get(., envir)
    if (exists("..Name", e, inherits = FALSE)) {
      e$..Name
    } else {
      .
    }
  }
}
