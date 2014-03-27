plotOWS <- function(team, min){#plots player name vs. offensive win shares
  df <- PlayerOffEff(team, min)
  player <- df$Player[c(1:length(df$Player))]
  ows <- df$"Offensive Win Shares"[c(1:length(df$"Offensive Win Shares"))]

  ows <- as.numeric(ows)
  player <- as.character(player)
  
  data <- data.frame(player, ows)
  #ows <- reorder(ows, player)
  data <- data[order(data$ows,decreasing = TRUE),]
  #barplot(ows, names = player, xlab="Player", ylab = "Offensive Win Shares", main = "Offensive Production")
  barchart(player ~ ows, data = data, xlab = "Offensive Win Shares", ylab = "Player", main = "Offensive Production")


}