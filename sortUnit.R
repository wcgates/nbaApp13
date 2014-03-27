sortUnit <- function(team, type){
  lineups13 <- read.csv('lineups13.csv')
  lineups <- data.frame(lineups13[lineups13$Team == team, ])
  roster <- getRoster(team)
  size = 1
  if(type=='ind'){
    for(i in 1:length(roster$Player)){
      
      player <- roster$Player[i]
      if(size==1){
        result <- data.frame(indivImpact(team, player))
        size = size + 1
      }
      else {
        result[size, ] <- indivImpact(team, player)
        size = size + 1
      } 
      #result$Unit[i] <- player
    }
  }
  
  else if(type=='two'){
    for(i in 1:length(roster$Player)){
      player1 <- roster$Player[i]
      for(j in 1:length(roster$Player)){
      player2 <- roster$Player[j]
        if(size == 1){
            result <- data.frame(twoManUnits(team, player1, player2))
            size = size + 1
        }
        else {
            result[size, ] <- twoManUnits(team, player1, player2)
            size = size + 1
        }
      }  
    }
      
  }
  
  else if(type=='three'){
    
  }
  else if(type=='five'){
    
  }
  
  
  #result$net <- result$"Offensive Rating" - result$"Defensive Rating"
  colnames(result) <- c("Player(s)", "Total Minutes", "Offensive Rating", "Defensive Rating", "Net Impact")
  result = format(result,digits=2, nsmall=2)
  result[order(as.numeric(result$"Total Minutes"), decreasing=TRUE), ]
  
}