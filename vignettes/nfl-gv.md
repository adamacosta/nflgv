---
title: "NFL 2014 GraphViz"
author: "Adam Acosta"
date: "2015-08-09"
output: 
  rmarkdown::html_vignette:
    fig_caption: yes
vignette: >
  %\VignetteIndexEntry{NFL 2014 GraphViz}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

## NFL 2014 Season with GraphViz and DiagrammeR

Hover over nodes to see teams and over edges to see games. Nodes 
are color-coded according to team colors. 

Nodes are scaled according to cumulative percentage margin for the 
whole season, that is, margin of victory as a percentage of total 
points scored in each game, a measure intended to correct for the 
bias toward offense in pure margin of victory. 

Edges are scaled according to the percent margin of victory for 
each game. 



<!--html_preserve-->
<object type='image/svg+xml' data='graph.svg' 
        height=800px width=800px></object>
<!--/html_preserve-->

Colors from [NFL Team Colors Codes](http://teamcolorcodes.com/category/nfl-team-color-codes/)

Season data from [Pro Football Reference](http://pro-football-reference.com)

## Code


```r
#' loadData
#' 
#' @importFrom dplyr transmute
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom magrittr %>%
#' @export
loadData <- function() {
  pointFile <- system.file('extdata', 'nfl2014.csv', 
                           package = 'nflgv')
  colorFile <- system.file('extdata', 'nfl_team_colors.csv', 
                           package = 'nflgv')
  
  nfl2014 <- read.csv(pointFile, stringsAsFactors = FALSE) %>%
    transmute(week = Week, winner = Winner, loser = Loser, 
              # percent_margin prevents favoring high scores
              pts = percent_margin(PtsW, PtsL))
  
  colors <- read.csv(colorFile, stringsAsFactors = FALSE)
  
  movs <- aggregate(pts ~ winner + loser, 
                    data = select(nfl2014, winner, loser, pts),
                    mean)
  
  teams <- aggregate(pts ~ winner, data = movs, sum) %>%
    rename(team = winner) %>%
    left_join(colors)
  
  return(list(movs = movs, teams = teams))
}

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

library(nflgv)
args <- loadData()
buildGraph(args$movs, args$teams)
```
