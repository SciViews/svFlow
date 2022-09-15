# svFlow To Do list

-   Complete examples for `?flow` and `?pipe_operator`.

-   More tests.

-   Refine the repo `README.md` file: explanation of what it does + examples.

-   Transparent lazyeval mechanism for ... (equivalent of `quos(...)`) + `!!()` outside of lazyeval + name\_ = also outside, or with something else than a name

-   Ambiguity of name\_ in assignation (name\_\_ for both "assign to flow and make quosure"?)

-   Use same root for all **Flow** objects.

-   Functions to work inside the flow, `pass()` or `skip()`, ... in the pipeline.

-   `pipe_debug()` =\> true debugging, possibly with arg

-   `pipe_show()` or `pipe_stepin()`, or ...?

-   `as_s3()` to convert a **Flow** object into S3?

-   "branches", + merge, etc.

-   Integrate with {targets}?

-   A specific function to get parent. Now, we use `fl$parent.env()`.

-   Add `expr=` as argument to **Flow** object. May be add possibility to redefine environment for methods? `flow(.)` vs `flow(.value)` when passing a **Flow** object.

-   Complete the tutorial vignettes.
