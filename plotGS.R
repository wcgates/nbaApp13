plotGS <- function(team, player){#generates plot of date vs. player score
  df <- gameTable(team, player)
  
  date <- df$Date 
  gs <- df$"Game Score"
  
  meanGS <- mean(gs)
  varGS <- var(gs)
  
  df$Color[df$"Game Score">=meanGS]="red"
  df$Color[df$"Game Score"< meanGS]="blue"
  
  
  
  plot(date, gs, col=df$Color, xlab = "Date", ylab="Game Score", main=paste("Game Scores for", player, "    variance: ", varGS))

}
