gameTable <- function(team, player){#goes through every game a player has played in, puts stats in new data frame
  data <- read.csv('NBA v003.csv')
  rs_df <- data[data$playoff_ind == 0, ] #narrows data frame to regular season
  tT <- rs_df[rs_df$Team == team, ]#narrows to team
  td <- tT[tT$Player == player, ]#narrows to player
  
  
  
  for(i in 1:length(td$Game.ID)){#look at each individual game
   
    date <- td$GameDate[i]
    #teamStats <- findGame(team, td$Game.ID[i])
      
    name <- as.character(td$Player[i])
    PTS <- td$Points[i]
    FGM <- td$Field.Goals[i]
    FGA <- td$Field.Goal.Attempts[i]
    FTM <- td$Free.Throws[i]
    FTA <- td$Free.Throw.Attempts[i]
    MIN <- td$Minutes.Played[i]
    AST <- td$Assists[i]
    OR <- td$Offensive.Rebounds[i]
    TO <- td$Turnovers[i]
    REB <- td$Total.Rebounds[i]
    STL <- td$Steals[i]
    BLK <- td$Blocks[i]
    FTPerc <- td$Free.Throws[i]/td$Free.Throw.Attempts[i]
    FG3M <- td$X3.Point.Field.Goals[i]
    PF <- td$Personal.Fouls[i]
    PM <- td$Plus.Minus[i]
    
    gameScore <- PTS+(FGM*0.4)+(FGA*-0.7)+((FTA-FTM)*-0.4)+(OR*0.7)+((REB-OR)*0.3)+(STL*1.0)+(AST*0.7)+(BLK*0.7)+(PF*-0.4)+(TO*-1.0)#linear weighted formula for game score
    
    stat_line <- list(date, MIN, gameScore)#decides which stats to store
    
    if(i==1 ){
      result <- data.frame(c(name, stat_line),stringsAsFactors = FALSE)
    }
    else if (i>1){
    
      result[i,] = c(name, stat_line)   
    }
    
 
  
  }
  colnames(result) = c("Player", "Date", "Minutes","Game Score")
  result
  
}
