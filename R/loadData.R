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