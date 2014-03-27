getLineups <- function(team){#returns list of lineups for a team
 
  lineups13 <- read.csv('lineups13.csv')
  td <- data.frame(lineups13[lineups13$Team == team, ])
  
  
  
  as.numeric(td$Minutes)
  td$Number <- NULL
  td$Off <- NULL
  td$Def <- NULL
  td$W <- NULL
  td$L <- NULL
  td$WinPerc <- NULL
  colnames(td) = c("Team", "Unit", "Minutes", "Offensive Rating", "Defensive Rating", "Net Rating", "+/-")
  td[order(as.numeric(td$"Minutes")),]
  
} 