threeManUnits <- function(team, player1, player2, player3){#returns average stats for lineups with three specified players on the floor
  lineups13 <- read.csv('lineups13.csv')
  as.character(lineups13$Unit)
  td <- data.frame(lineups13[lineups13$Team == team, ])#narrows data down to team

  size = 1
  sizewo = 1
  mins = 0
  with <- data.frame(u=character(0), m=numeric(0), or = numeric(0), dr = numeric(0), pm = numeric(0), stringsAsFactors = FALSE)#data frame for lineups with those players
  without <- data.frame(u=character(0), m=numeric(0), or = numeric(0), dr = numeric(0), pm = numeric(0), stringsAsFactors = FALSE)
  for(i in 1:length(td$Unit)){
    unit <- paste(player1, '-' ,player2, '-', player3)
    wounit <- paste('Other Lineups')
    
    if(grepl(player1, td$Unit[i]) & grepl(player2, td$Unit[i]) & grepl(player3, td$Unit[i])){ #checks if lineup has all 3 players
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
    else {#otherwise add to without dataframe    
      
      statline <- list(as.numeric(as.character(td$Min[i])), as.numeric(td$Offensive.Rating[i]), as.numeric(td$Defensive.Rating[i]), as.numeric(td$PlusMinus[i]))
      if(sizewo==1){
        without <- data.frame(c(unit, statline), stringsAsFactors = FALSE)
        sizewo = sizewo + 1
      }
      else {
        without[sizewo, ] <- c(unit, statline)
        sizewo = sizewo + 1
      }
    }
    
    #}
  }
  
  
  colnames(with) <- c("Unit", "Minutes", "Offensive Rating", "Defensive Rating", "+/-")  
  colnames(without) <- c("Unit", "Minutes", "Offensive Rating", "Defensive Rating", "+/-")  
  #if(F){
  means <- data.frame(unit, sum(with$Minutes), mean(with$"Offensive Rating"), mean(with$"Defensive Rating"), mean(with$"+/-"), stringsAsFactors = FALSE)
  #means[2,] <- c(wounit, sum(without$Minutes), mean(without$"Offensive Rating"), mean(without$"Defensive Rating"), mean(without$"+/-"))
  colnames(means) <- c("Unit", "Total Minutes", "Offensive Rating", "Defensive Rating", "+/-")  
  #format(means,digits=2, nsmall=2)
  means = format(means,digits=2, nsmall=2)
  if(FALSE){#difference between with & without, not included
  diffOR <- as.numeric(means$"Offensive Rating"[1]) - as.numeric(means$"Offensive Rating"[2])
  diffDR <- as.numeric(means$"Defensive Rating"[1]) - as.numeric(means$"Defensive Rating"[2])
  diffPM <- as.numeric(means$"+/-"[1]) - as.numeric(means$"+/-"[2])
  net <- diffOR - diffDR
  
  diff <- data.frame(unit, means$"Total Minutes"[1], diffOR, diffDR, net)
  colnames(diff) <- c("Unit", "Minutes", "Offensive Impact", "Defensive Impact", "Net Impact")
  diff
  }
  means
}