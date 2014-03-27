indivImpact <- function(team, player){
  lineups13 <- read.csv('lineups13.csv')
  as.character(lineups13$Unit)
  td <- data.frame(lineups13[lineups13$Team == team, ])
  as.numeric(td$Min)
  as.numeric(td$Offensive.Rating)
  as.numeric(td$Defensive.Rating)
  as.numeric(td$Plus.Minus)
  size = 1
  sizewo = 1
  mins = 0
  with <- data.frame(u=character(0), m=numeric(0), or = numeric(0), dr = numeric(0), pm = numeric(0), stringsAsFactors = FALSE)
  without <- data.frame(u=character(0), m=numeric(0), or = numeric(0), dr = numeric(0), pm = numeric(0), stringsAsFactors = FALSE)
  for(i in 1:length(td$Unit)){
    unit <- paste('With', player)
    wounit <- paste('Without', player)
    
    if(grepl(player, td$Unit[i])){ 
      statline <- list(as.numeric(as.character(td$Min[i])), as.numeric(td$Offensive.Rating[i]), as.numeric(td$Defensive.Rating[i]), as.numeric(td$PlusMinus[i]))
      if(size==1){
        with <- data.frame(c(unit, statline), stringsAsFactors = FALSE)
        size = size + 1
      }
      else {
        with[size, ] <- c(unit, statline)
        size = size + 1
      }
    }
    else if(!grepl(player, td$Unit[i])){      
      statline <- list(as.numeric(as.character(td$Min[i])), as.numeric(td$Offensive.Rating[i]), as.numeric(td$Defensive.Rating[i]), as.numeric(td$PlusMinus[i]))
      if(sizewo==1){
        without <- data.frame(c(wounit, statline), stringsAsFactors = FALSE)
        sizewo = sizewo + 1
      }
      else {
        without[sizewo, ] <- c(wounit, statline)
        sizewo = sizewo + 1
      }
    }
    
    
  }
  
  without = without[complete.cases(without),]
  colnames(with) <- c("Unit", "Minutes", "Offensive Rating", "Defensive Rating", "+/-")  
  colnames(without) <- c("Unit", "Minutes", "Offensive Rating", "Defensive Rating", "+/-")  
  #if(F){
  means <- data.frame(unit, sum(with$Minutes), mean(with$"Offensive Rating"), mean(with$"Defensive Rating"), mean(with$"+/-"), stringsAsFactors = FALSE)
  means[2,] <- c(wounit, sum(without$Minutes), mean(without$"Offensive Rating"), mean(without$"Defensive Rating"), mean(without$"+/-"))
  colnames(means) <- c("Unit", "Total Minutes", "Offensive Rating", "Defensive Rating", "+/-")  
  #format(means,digits=2, nsmall=2)
  means = format(means,digits=2, nsmall=2)
  #diffMin <- as.numeric(means$"Total Minutes"[1])-as.numeric(means$"Total Minutes"[2])
  diffOR <- as.numeric(means$"Offensive Rating"[1]) - as.numeric(means$"Offensive Rating"[2])
  diffDR <- as.numeric(means$"Defensive Rating"[1]) - as.numeric(means$"Defensive Rating"[2])
  diffPM <- as.numeric(means$"+/-"[1]) - as.numeric(means$"+/-"[2])
  net <- diffOR - diffDR
  
  diff <- data.frame(player, means$"Total Minutes"[1], diffOR, diffDR, net)
  colnames(diff) <- c("Player", "Minutes", "Offensive Impact", "Defensive Impact", "Net Impact")
  diff
  
  
}