#' Create and manipulate quosures easily
#'
#' Quosures are defined in **rlang** package as part of the tidy evaluation of
#' non-standard evaluations (see [quo()]). Here, we provide an alternate
#' mechanism using `-~expr` as a synonym of `quo(expr)`, but faster. Also,
#' `UQ(quo_obj)` or `!!quo_obj` in **rlang** is just here `+quo_obj`.
#'
#' @param x An expression
#' @param env An environment specified for lexical scoping of the quosure.
#' @param e1 Unary operator member, or first member of a binary operator.
#' @param e2 Second member of a binary operator (not used here, except for `^`).
#' @param ... Further arguments passed to the `print()` method (not used yet).
#'
#' @details `-` is defined as an unary minus operator for **formula** objects
#' (which is *not* defined in base R, hence, not supposed to be used otherwise).
#' Thus, `-~expr` jsut converts a formula build using the base `~expr`
#' instruction into a quosure. `as_quosure()` does the same, when expression is
#' provided directly.
#'
#' Similarly, the unary `+` operator is defined for **quosure** in order to
#' easily "reverse" the mechanism of equosing an expression with a logical
#' complimentary operator. It does the same as `!!` in **rlang**, but it has
#' higher syntax precedence than `!`, and is thus less susceptible to require
#' parentheses (only `^` for exponentiation, indexing/subsetting operators like
#' `$` or `[`, and namespace operators `::` and `:::` have higher precedence).
#' @export
#' @name quosure
#' @seealso [quos_underscore()], \code{\link{\%>_\%}}
#' @keywords utilities
#' @concept expression encapsulation for non-standard evaluation
#' @examples
#' # TODO...
as_quosure <- function(x, env = caller_env()) {
  if (is_quosure(x)) {
    x

  } else if (is_bare_formula(x)) {
    .as_quosure_formula(x, env)

  } else if (is_symbolic(x)) {
    new_quosure(x, env)

  } else {
    new_quosure(x, empty_env())
  }
}

#' @export
#' @rdname quosure
as.quosure <- as_quosure

#' @export
#' @rdname quosure
is_quosure <- function(x)
  x %is% 'quosure'

#' @export
#' @rdname quosure
is.quosure <- is_quosure

# Do not export this, because it mask a function of the same name in purrr
is_formula <- function(x)
  x %is% 'formula'

#' @export
#' @rdname quosure
is.formula <- is_formula

#' @export
#' @rdname quosure
is_bare_formula <- function(x)
  is_true(class(x) == 'formula')

#' @export
#' @rdname quosure
is.bare_formula <- is_bare_formula

#' @export
#' @rdname quosure
`-.formula` <- function(e1, e2) {
  # Same as as.quosure(), but allows for a more compact notation -~expr
  # Warning: if the formula was subclassed, it is still converted. This is
  # different than as.quosure()!
  if (missing(e2)) {
    # Convert as quosure, but do not fix environment (assume it is correct!)
    # This allows to gain some speed for a case that is never match in -~expr!
    .as_quosure_formula(e1, NULL)
  } else {
    abort("binary - is not allowed for formulas")
  }
}

#' @export
#' @rdname quosure
`+.formula` <- function(e1, e2) {
  # Both unquote and eval if it is a quosure, so +~ can be the opposite to ~-
  if (missing(e2)) {
    eval_tidy(eval(get_expr(e1)))
    #if (!is_quosure(quo))
    #  abort("unary +~ only applies to quosures")
    #expr <- f_rhs(quo)
    ##if (is.name(expr))
    ##  expr <- call('(', expr)
    #eval(expr, envir = attr(quo, ".Environment"), enclos = env)
    #eval_tidy(quo)
  } else {
    abort("binary + is not allowed for formulas")
  }
}

#' @export
#' @rdname quosure
`^.quosure` <- function(e1, e2)
  eval_tidy(e1)^e2
  #eval((e1), envir = f_env(e1), enclos = caller_env(2))^e2

#' @export
#' @rdname quosure
`+.quosure` <- function(e1, e2) {
  if (missing(e2)) {
    expr <- quo_get_expr(e1)
    # Weird things happen with names, when setting attributes to them!
    # So, we transform them into other objects: 'name' becomes '(name)'
    if (is.name(expr))
      expr <- call('(', expr)
    unquo <- structure(expr, .Environment = quo_get_env(e1))
    class(unquo) <- c("unquoted", class(unquo))
    unquo
  } else {
    abort("binary + is not allowed for quosures")
  }
}

#' @export
#' @rdname quosure
`+.unquoted` <- function(e1, e2) {
  if (missing(e2)) {
    #eval(e1, envir = attr(e1, ".Environment"), enclos = caller_env(2))
    eval_tidy(e1)
  } else {
    abort("binary + is not allowed for unquoted objects")
  }
}

#' @export
#' @rdname quosure
print.unquoted <- function(x, ...) {
  cat(deparse(x), "\n")
  print(attr(x, ".Environment"))
  invisible(x)
}

# Convert names_ to quosures ----------------------------------------------

#' Convert names ending with _ into quosures automatically
#'
#' All names that end with _ are automatically converted into quosures, and also
#' assigned to the name without training `_`. The other arguments are evaluated.
#'
#' @param ... The named arguments provided to be either converted into quosures
#' or evaluated.
#'
#' @export
#' @seealso [as_quosure()], \code{\link{\%>_\%}}
#' @keywords utilities
#' @concept automatic quosures creation for non-standard evaluation
#' @examples
#' # TODO...
quos_underscore <- function(...) {
  # Transform into closures only those items whose name ends with _
  # (and also assign them to names without the _)
  #
  # rlang does not export dots_capture() that we could use here, and list(...)
  # does evaluate all arguments in ... So, one (suboptimal) solution is to
  # convert all ... arguments into quosures using rlang::quos(), and then, to
  # evaluate those in the list whose name does not end with '_'
  # TODO: reimplement of course later with something more efficient!
  dots <- quos(...)
  dots_names <- names(dots)
  l_names <- nchar(dots_names)
  last_char <- substr(dots_names, l_names, l_names)
  env <- caller_env(2)
  for (name in dots_names[last_char != "_"])
    dots[[name]] <- eval_tidy(dots[[name]], env = env)
  for (name in dots_names[last_char == "_"]) {
    dots[[substring(name, 1, nchar(name) - 1)]] <- dots[[name]]
    dots[[name]] <- NULL
  }
  dots
}
