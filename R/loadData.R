#' loadData
#' 
#' @importFrom dplyr transmute
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @export
loadData <- function() {
  filename <- system.file('extdata', 'nfl2014.csv', 
                          package = 'nflgv')
  nfl2014 <- read.csv(filename) %>%
    transmute(week = Week, winner = Winner, loser = Loser, 
              pts = percent_margin(PtsW, PtsL))
  
  movs <- aggregate(pts ~ winner + loser, 
                    data = select(nfl2014, winner, loser, pts),
                    mean)
  
  teams <- aggregate(pts ~ winner, data = movs, sum)
  
  return(list(movs = movs, teams = teams))
}