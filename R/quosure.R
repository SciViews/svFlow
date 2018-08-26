#' Create and manipulate quosures easily
#'
#' @description  Quosures are defined in **rlang** package as part of the tidy
#' evaluation of non-standard expressions (see [quo()]). Here, we provide an
#' alternate mechanism using `-~expr` as a synonym of `quo(expr)`. Also,
#' `+quo_obj` is equivalent to `!!quo_obj` in **rlang**, and ++quo_obj both
#' unquotes and evaluates it in the right environment. Quosures are keystone
#' objects in the tidy evaluation mechanism. So, they deserve a special, clean
#' and concise syntax to create and manipulate them.
#'
#' The `as_xxx()` and `is_xxx()` further ease the manipulation of **quosure**s
#' or related objects.
#'
#' @param x An expression
#' @param env An environment specified for scoping of the quosure.
#' @param e1 Unary operator member, or first member of a binary operator.
#' @param e2 Second member of a binary operator (not used here, except for `^`).
#' @param ... Further arguments passed to the `print()` method (not used yet).
#'
#' @return These functions build or manipulated **quosure**s and return such
#' objects. `+quosure` creates an **unquoted** object. The `+` unary operator
#' applied to **unquoted** objects evaluate the expression contained in the
#' **quosure** in the right environment.
#'
#' @details `-` is defined as an unary minus operator for **formula** objects
#' (which is *not* defined in base R, hence, not supposed to be used otherwise).
#' Thus, `-~expr` just converts a formula build using the base `~expr`
#' instruction into a **quosure**. `as_quosure()` does the same, when the
#' expression is provided directly, and allows also to define the enclosing
#' environment (by default, it is the environment where the code is evaluated,
#' and it is also the case when using `-~expr`).
#'
#' Similarly, the unary `+` operator is defined for **quosure** in order to
#' easily "reverse" the mechanism of quoting an expression with a logical
#' complementary operator. It does something similar to `!!` in **rlang**, but
#' it can be used outside of tidy eval expressions. Since unary `+` has higher
#' syntax precedence than `!` in R, it is less susceptible to require
#' parentheses (only `^` for exponentiation, indexing/subsetting operators like
#' `$` or `[`, and namespace operators `::` and `:::` have higher precedence). A
#' specific `^` operator for quosures solves the precedence issue. `::` or `:::`
#' are very unlikely used in the context.
#'
#' `++quosure` is indeed a two-steps operation (`+(+quosure)`). It first
#' unquotes the quosure, returning an **unquoted** object. Then, the second `+`
#' evaluates the **unquoted** object. This allows for fine-graded manipulation
#' of **quosure**s: you can unquote at one place, and evaluate the **unquoted**
#' object elsewhere (and, of course, the contained expression is always
#' evaluated in the _right_ environment, despite all these manipulations).
#'
#' `!!` and just evaluates its argument and passes the result. It is only useful
#' inside a quasi-quoted argument, see \code{\link[rlang]{quasiquotation}}.
#' @export
#' @name quosure
#' @seealso [quos_underscore], \code{\link{\%>_\%}}
#' @keywords utilities
#' @concept expression encapsulation for non-standard evaluation
#' @examples
#' x <- 1:10
#' # Create a quosure (same as quo(x))
#' x_quo <- -~x
#' x_quo
#' # Unquote it (same as !!x, but usable everywhere)
#' +x_quo
#' # Unquote and evaluate the quosure
#' ++x_quo
#' # Syntax precedence issues (^ has higher precedence than unary +)
#' # is solved by redefining ^ for unquoted objects:
#' ++x_quo^2
#' # acts like if ++ had higher precedence than ^, thus like if it was
#' (++x_quo)^2
#'
#' # Assign the unquoted expression
#' x_unquo <- +x_quo
#' # ... and use x_unquo in a different context
#' foo <- function(x) +x
#' foo(x_unquo)
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
  # Both unquote and eval if it is a quosure, so +~ can be the opposite to -~
  if (missing(e2)) {
    eval_tidy(eval(get_expr(e1)))
  } else {
    abort("binary + is not allowed for formulas")
  }
}

#' @export
#' @rdname quosure
`^.quosure` <- function(e1, e2)
  eval_tidy(e1)^e2

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

#' @export
#' @rdname quosure
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
# It is used for the same purpose, but I don't understand why its code must
# be so complex (and slow!). So, I stick to my own version here
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
`!!` <- function(x) get_expr(x)

# This works for code, but Roxygen2 generates !!(x) <- value instead of
# `!!`(x) <- value, and then this is incorrectly analyzed by R CMD check!
# @export
# @rdname quosure
# @param value A value to assign to `x`.
#`!!<-` <- function(x, value) value
