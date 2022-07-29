#' Data analysis workflow and alternate pipeline operator
#'
#' Data (work)flow analysis using proto objects (see [proto()]) and a pipe
#' operator that integrates non-standard evaluation and the tidyeval mechanism
#' in a most transparent way.
#'
#' @section Important functions:
#'
#' - \code{\link{\%>.\%}} and \code{\link{\%>_\%}} are two alternate pipe
#'   operators designed to supplement magrittr's \%>\% operator in the
#'   tidyverse and elsewhere. They are provided for good reasons.
#'   \code{\link{\%>.\%}} requires explicit indication of the position of `.`
#'   in the pipeline expression **all the time**. The expression is not
#'   modified. As a consequence, it can never surprise you with an unexpected
#'   behavior, and all valid R expressions are usable in the pipeline. Another
#'   consequence: it is very fast. \code{\link{\%>_\%}} works with **Flow**
#'   objects that allow for encapsulation of satellite objects (data or
#'   functions) within the pipeline. It is self-contained. The pipeline can be
#'   interrupted and restarted at any time. It also allows for a class-less
#'   object-oriented approach with single inheritance (could be useful to test
#'   easily different scenarios on the same pipeline and to prototype objects
#'   that are "pipe-aware"). It also manages the tidyeval mechanism for
#'   non-standard expressions in the most transparent way: the only "rule" to
#'   remember is to suffix the name of variables that needs special treatment
#'   with an underscore (`_`) and the pipe operator manages the rest for you.
#'
#' - [debug_flow()] provides a convenient way to debug problematic pipelines
#'   build with our own pipe operators \code{\link{\%>.\%}} and
#'   \code{\link{\%>_\%}} in a comfortable way. Everything from the step that
#'   raised a error is available: the piped data, the expression to be
#'   evaluated, and possibly, the last state of the **Flow** object. Everything
#'   can be inspected, modified, and the expression can be rerun as if you were
#'   still right in the middle of the pipeline evaluation.
#'
#'- [flow()] constructs a `Flow` object that is pipe-aware and tidyeval-aware.
#'  This opens new horizons in your analysis workflow. You start building a
#'  simple _ad hoc_ pipeline, then you can include satellite data or functions
#'  right inside it, perhaps also test different scenarios by using the object
#'  inheritance features of **Flow** (common parts are shared among the
#'  different scenarios, thus reducing the memory footprint). While your
#'  pipeline matures you gradually and naturally move towards either a
#'  functional sequence or a dedicated object. The functional sequence pathway
#'  consists in building a reusable function to recycle you pipeline in a
#'  different context. The object pathway is not fully developed yet in the
#'  present version. But in the future, the object-oriented nature of **Flow**
#'  will also be leveraged, so that you could automatically translate your "flow
#'  pipeline" into an S3 or R6 object with satellite data becoming object
#'  attributes, and satellite functions becoming methods. The pipeline itself
#'  would then become the default method for that object. Of course, both
#'  functions and objects derived from a "flow pipeline" will be directly
#'  compatible with the tidyeval mechanism, as they will be most
#'  tidyverse-friendly as possible per construction.
#'
#' - [str.Flow()] compactly displays the content of a **Flow** object.
#'
#' - [as_quosure()], and unary `+` and `-` operators combined with **formula**
#'   objects provide an alternate way to create **quosure**s.
#'
#' - [quos_underscore()] automatically converts arguments whose name ends with
#'   `_` into **quosure**s, and this mechanism is used by our flow pipe
#'   operator to implement the tidyeval mechanism most transparently inside
#'   "flow pipelines".
#' @docType package
#' @name svFlow-package
#'
#' @importFrom proto proto as.proto
#' @importFrom rlang abort warn caller_env empty_env env_parent f_env f_env<-
#'   f_lhs f_rhs is_function is_symbolic is_true new_quosure quos enquo
#'   eval_tidy get_expr quo_get_env quo_get_expr
#' @importFrom utils capture.output str
#' @importFrom igraph graph_from_edgelist
#' @importFrom graphics plot
NULL
