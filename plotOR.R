plotOR <- function(team, min){#generates plot of player vs. offensive rating
  df <- PlayerOffEff(team, min)
  player <- df$Player[c(1:length(df$Player))]
  or <- df$"Offensive Rating"[c(1:length(df$"Offensive Rating"))]
  
  or <- as.numeric(or)
  player <- as.character(player)
  
  data <- data.frame(player, or)
  #ows <- reorder(ows, player)
  data <- data[order(data$or,decreasing = TRUE),]
  #barplot(ows, names = player, xlab="Player", ylab = "Offensive Win Shares", main = "Offensive Production")
  barchart(player ~ or, data = data, xlab = "Player", ylab = "Offensive Rating", main = "Offensive Production")
  
  
}