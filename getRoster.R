getRoster <- function(team){#returns list of player names that are included in the corresponding team's list of lineups
  lineups13 <- read.csv('lineups13.csv')
  rosters13 <- read.csv('rosters13.csv')
  lineups <- lineups13[lineups13$Team == team, ]
  roster <- rosters13[rosters13$Team == team, ]
  size <- 1
  valid <- 0
  count <- 1
  for(i in 1:length(roster$Player)){
    valid <- 0
    player <- roster$Player[i]
    for(j in 1:length(lineups$Unit)){
      if(grepl(player, lineups$Unit[j])){#check if player name is in the lineup list
        valid <- 1
      }
    }
    if(valid == 1){
      if(size == 1){
        result <- data.frame(roster[roster$Player == player, ])
        size = size + 1
      }
      else {
        result[size, ] <- roster[roster$Player == player, ]
        size = size + 1
      }
      count = count + 1
    }
  }

  result
}