plotFP <- function(team, min){#generates plot of player vs. floor percentage
  df <- PlayerOffEff(team, min)
  player <- df$Player[c(1:length(df$Player))]
  fp <- df$"Floor Percentage"[c(1:length(df$"Floor Percentage"))]
  
  fp <- as.numeric(fp)
  player <- as.character(player)
  
  data <- data.frame(player, fp)
  #ows <- reorder(ows, player)
  data <- data[order(data$fp,decreasing = TRUE),]
  #barplot(ows, names = player, xlab="Player", ylab = "Offensive Win Shares", main = "Offensive Production")
  barchart(player ~ fp, data = data, xlab = "Player", ylab = "Floor Percentage", main = "Offensive Production")
  
  
}