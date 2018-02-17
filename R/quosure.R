is_quosure <- function(x)
  x %is% 'quosure'

is.quosure <- is_quosure

is_formula <- function(x)
  x %is% 'formula'

is.formula <- is_formula

is_bare_formula <- function(x)
  is_true(class(x) == 'formula')

is.bare_formula <- is_bare_formula

f_drop_lhs <- function(x)
  if (!is_null(f_lhs(x))) x[-2]

.as_quosure_formula <- function(x, env = caller_env()) {
  # A quosure does not have lhs, so, drop if if present (second term)
  if (!is_null(f_lhs(x)))
    x <- x[-2]
  # A quosure always has an environment, so, fix it if not there
  if (!is_null(env) && is_null(f_env(x)))
    f_env(x) <- env
  class(x) <- c('quosure', 'formula')
  x
}

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

as.quosure <- as_quosure


`-.formula` <- function(e1, e2) {
  # Same as as.quosure(), but allows a more compact notation -~expr
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

`+.quosure` <- function(e1, e2) {
  # + -~expr is like UQE(quo(expr)) or UQ(quo(expr)) inside %>+% pipe
  # expressions thanks to some black magic in this operator
  if (missing(e2)) {
    f_rhs(e1)
  } else {
    abort("binary + is not allowed for quosures")
  }
}

quos_underscore <- function(...) {
  # Transform into closures only those items whose name ends with _
  #
  # rlang does not export dots_capture() that we could use here, and list(...)
  # does evaluate all arguments in ... So, one (suboptimal) solution is to
  # convert all ... arguments into quosures, and then, to evaluate those in the
  # list whose name does not end with '_'
  # TODO: reimplement later with something more efficient!
  dots <- quos(...)
  dots_names <- names(dots)
  l_names <- nchar(dots_names)
  last_char <- substr(dots_names, l_names, l_names)
  env <- caller_env(2)
  for (name in dots_names[last_char != "_"])
    dots[[name]] <- eval(`!!`(dots[[name]]), envir = env)
  dots
}
