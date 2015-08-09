#' buildGraph
#' 
#' @import DiagrammeR
#' @import V8
#' @export
buildGraph <- function(movs, teams) {
  nodes <- create_nodes(nodes = teams$team, 
                        color = teams$secondary, 
                        fillcolor = teams$primary, 
                        penwidth = 10,
                        label = FALSE,
                        tooltip = teams$team, 
                        width = scale_1_10(teams$pts) / 5)
  
  edges <- create_edges(from = movs$winner, 
                        to = movs$loser, 
                        data = movs$pts)
  
  edges <- scale_edges(edges_df = edges,
                       to_scale = edges$data,
                       edge_attr = "penwidth",
                       range = c(1, 5))
  
  node_attrs <- c("style = filled", "shape = circle", 
                  "fontname = Helvetica")
  
  edge_attrs <- c("arrowhead = vee", 
                  "arrowsize = 1", 
                  "color = slategray")
  
  graph_attrs <- c("layout = circo",
                   "overlap = false",
                   "fixedsize = false",
                   "ranksep = 3",
                   "outputorder = edgesfirst")
  
  create_graph(nodes_df = nodes,
               edges_df = edges,
               graph_attrs = graph_attrs, 
               node_attrs = node_attrs,
               edge_attrs = edge_attrs, 
               graph_name = 'NFL 2014') %>%
    render_graph(output = 'SVG', width = 800, height = 800) %>%
    cat(file = 'graph.svg')
}
