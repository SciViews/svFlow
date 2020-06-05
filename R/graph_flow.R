#' Create a graph with Flow objects hierarchy
#'
#' A graph showing all **Flow** objects heritage is calculated, and displayed.
#'
#' @param env The environment to look for **Flow** objects. By default, it is
#' `.GlobalEnv`, and you should not change it, since all **Flow** objects are
#' derived from it by construction.
#' @param child_to_parent Do the arrows go from child to parent (by default), or
#' in the other direction?
#' @param plotit Do we plot the graph (by default)?
#' @param ... Further parameters passed to [plot.igraph()].
#' @return An **igraph** object (returned invisibly if `plotit = TRUE`.
#'
#' @export
#' @seealso [flow]
#' @keywords utilities
#' @concept display objects hierarchy
#' @examples
#' a <- flow()
#' b <- a$flow()
#' c <- b$flow()
#' d <- a$flow()
#' # Use of custom names
#' e <- flow(.name = "parent")
#' f <- e$flow(.name = "child")
#' graph_flow()
#'
#' # Arrows pointing from childs to parents, and do not plot it
#' g <- graph_flow(child_to_parent = FALSE, plotit = FALSE)
#' g
#' plot(g)
graph_flow <- function(env = .GlobalEnv, child_to_parent = TRUE,
plotit = TRUE, ...) {
  edges <- character(0L)

  add_edge <- function(edges, from, to)
    c(edges, from, to)

  is_node <- unlist(eapply(env, is_flow))
  nodes <- names(is_node[is_node])
  for (node in nodes) {
    if (isTRUE(child_to_parent)) {
      edges <- add_edge(edges,
        from  = .name_flow(node, env),
        to    = .name_flow(get(node, env)$parent.env(), env))
    } else {
      edges <- add_edge(edges,
        from = .name_flow(get(node, env)$parent.env(), env),
        to  = .name_flow(node, env))
    }
  }

  if (length(edges)) {
    g <- graph_from_edgelist(matrix(edges, ncol = 2L, byrow = TRUE))
  } else abort("No Flow objects found")

  if (isTRUE(plotit)) {
    plot(g, ...)
    invisible(g)
  } else g
}
