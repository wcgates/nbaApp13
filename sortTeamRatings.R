sortTeamRatings <- function(){
  
  #teamStats <- read.csv('TeamOffense12-13.csv')
  td <- data.frame(teamStats)
    
  for(i in 1:length(td$Team)){
    
    fgapg <- (td[td$Team==td$Team[i], 'FGA'])/(td[td$Team==td$Team[i], 'G'])
    orpg <- td[td$Team==td$Team[i], 'ORB']/td[td$Team==td$Team[i],'G']
    topg <- td[td$Team==td$Team[i], 'TOV']/td[td$Team==td$Team[i],'G']
    ftapg <- td[td$Team==td$Team[i], 'FTA']/td[td$Team==td$Team[i],'G']
    ppg <- td[td$Team==td$Team[i], 'PTS']/td[td$Team==td$Team[i],'G']
    popg <- td[td$Team==td$Team[i], 'POPG']
    
    poss <- fgapg-orpg+topg+(0.4*ftapg)
    
    team <- td[td$Team==td$Team[i], 'Team']
    
    oeff <- 100*ppg/poss
    deff <- 100*popg/poss
    net <- oeff - deff
    statline <- list(team, oeff, deff, net)
    if(i==1){
      result <- data.frame(statline)
     
      colnames(result) <- c("Team", "Offensive Efficiency", "Defensive Efficiency", "Net Rating") 
 
    }
    else {
      result[i,] = statline   
      
    }
    
  }
  result = format(result,digits=2, nsmall=2)
  #result = result[result$"Minutes"<20,]
  result[order(as.numeric(result$"Net Rating"), decreasing=TRUE), ]
}
