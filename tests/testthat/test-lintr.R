test_that("Package style is correct", {
  skip_if(Sys.getenv("_R_CHECK_PACKAGE_NAME_") != "", "Run in R CMD check")
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_on_ci()
  skip_on_covr()

  lintr::expect_lint_free(linters = list(

    ## Common mistakes

    # Check assignment <- is always used
    assignment =  lintr::assignment_linter,
    # Check explicit integers, e.g., 1L
    implicit_integer = lintr::implicit_integer_linter,
    # No T or F for TRUE or FALSE
    T_and_F_symbol = lintr::T_and_F_symbol_linter,
    # no c() or c("a") => just "a"
    unneeded_concatenation = lintr::unneeded_concatenation_linter,
    # no x == NA # nolint
    equals_na = lintr::equals_na_linter,
    # Avoid 1:length(x), 1:NROW(x), etc. (problem when length(x) == 0)
    seq = lintr::seq_linter,
    # Warn to use [[ instead of $ (but allowed in SciViews::R) =>
    #   extraction_operator = lintr::extraction_operator_linter,
    # No, in SciViews::R, single quote strings have special meanings =>
    #  single_quotes = lintr::single_quotes_linter,

    ## Object names

    # Many false positives =>
    #   object_name = lintr::object_name_linter(styles = "snake_case"),
    # Deprecated, use object_name instead =>
    #   camel_case = lintr::camel_case_linter,
    object_length = lintr::object_length_linter(length = 30L),

    ## Spaces, whitespaces, tabs, ...

    no_tab = lintr::no_tab_linter,
    semicolon_terminator = lintr::semicolon_terminator_linter(semicolon =
      c("compound", "trailing")),
    spaces_left_parentheses = lintr::spaces_left_parentheses_linter,
    function_left_parentheses = lintr::function_left_parentheses_linter,
    paren_brace = lintr::paren_brace_linter,
    commas = lintr::commas_linter,
    spaces_inside = lintr::spaces_inside_linter,
    infix_spaces = lintr::infix_spaces_linter,
    trailing_whitespace = lintr::trailing_whitespace_linter,
    trailing_blank_lines = lintr::trailing_blank_lines_linter,

    ## General organization of the code

    # One pipe operation per line (or everything on a single line)
    pipe_continuation = lintr::pipe_continuation_linter,
    # Open curly brace never on its own line and followed by a newline
    open_curly = lintr::open_curly_linter(allow_single_line = FALSE),
    # Closed curly braces on their own line (unless with else)
    closed_curly = lintr::closed_curly_linter(allow_single_line = FALSE),
    # Code complexity measure
    cyclocomp = lintr::cyclocomp_linter(complexity_limit = 25L),
    # Line length > 80 characters
    line_length = lintr::line_length_linter(80L),

    ## Code usage

    # Check function usage is OK using checkUsage()
    object_usage = lintr::object_usage_linter,
    # Blacklist functions
    undesirable_function = lintr::undesirable_function_linter(
      fun = lintr::default_undesirable_functions),
    # Blacklist operators
    undesirable_operator = lintr::undesirable_operator_linter(
      op = lintr::default_undesirable_operators),

    ## Comments

    # Code comments and TODO/FIXME comments are accepted in SciViews::R
    #commented = lintr::commented_code_linter,
    #todo = lintr::todo_comment_linter(todo = c("todo", "fixme")),

    ## File paths

    absolute_path = lintr::absolute_path_linter(lax = TRUE),
    nonportable_path = lintr::nonportable_path_linter(lax = TRUE)
  ))
})
