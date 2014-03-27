PlayerOffEff <- function(team, min){#returns table of players' minutes per game, offensive win shares, offensive rating, floor percentage through the '13 season
  data <- read.csv('NBA v003.csv')
  rs_teamReb <- read.csv('rsTeamReb.csv')#needed for the rebounding percentage stats
  rs_df <- data[data$playoff_ind == 0, ]#narrow data frame down to regular season
  td <- data.frame(rs_df[rs_df$Team == team, ])#narrow data frame down to rows with specified team
  teamRebDF <- data.frame(rs_teamReb[rs_teamReb$Team ==team,])#narrow data frame down to team
  td$Minutes = td$Seconds.Played/60
  
  #gather team stats
  TMMIN <- sum(td$Minutes)
  TMAST <- sum(td$Assists)
  TMFGM <- sum(td$Field.Goals)
  TMPTS <- sum(td$Points)
  TMFTM <- sum(td$Free.Throws)
  TMFTA <- sum(td$Free.Throw.Attempts)
  TMFGA <- sum(td$Field.Goal.Attempts)
  TMFGPerc <- sum(td$Field.Goals)/sum(td$Field.Goal.Attempts)
  TMFTPerc <- sum(td$Free.Throws)/sum(td$Free.Throw.Attempts)
  TMOR <- sum(td$Offensive.Rebounds)
  TMTO <- sum(td$Turnovers) 
  TMFG3M <- sum(td$X3.Point.FieldGoals)
  
  TMScPoss <- TMFGM + (1-(1-(TMFTPerc))^2)*TMFTA*0.4
  TMPlayPerc <- TMScPoss/(TMFGA+TMFTA*0.4+TMTO)
  TMPoss <- (TMFGA - TMOR + TMTO + 0.4*TMFTA)/teamOffAvg[teamOffAvg$Team == team, 'G']
  
  TMORPerc <- teamRebDF$OffensiveRebPerc
 
  TMORWeight <- ((1-TMORPerc)*TMPlayPerc)/((1-TMORPerc)*TMPlayPerc+TMORPerc*(1-TMPlayPerc))
  
  #get average league points per possession, points per game, and possessions
  AVGLGPPP <- mean(teamOffAvg$ORTG)/100
  AVGLGPPG <- mean(teamOffAvg$PTS.G)
  AVGLGPoss <- mean(teamOffAvg$POSS)
  
  for(i in 1:length(unique(td$Player))){#look at each player 
    GP <- sum(td[td$Player == unique(td$Player)[i], 'GP'])
    PTS <- sum(td[td$Player == unique(td$Player)[i], 'Points'])
    FGM <- sum(td[td$Player == unique(td$Player)[i], 'Field.Goals'])
    FGA <- sum(td[td$Player == unique(td$Player)[i], 'Field.Goal.Attempts'])
    FTM <- sum(td[td$Player == unique(td$Player)[i], 'Free.Throws'])
    FTA <- sum(td[td$Player == unique(td$Player)[i], 'Free.Throw.Attempts'])
    MIN <- sum(td[td$Player == unique(td$Player)[i], 'Minutes'])
    AST <- sum(td[td$Player == unique(td$Player)[i], 'Assists'])
    OR <- sum(td[td$Player == unique(td$Player)[i], 'Offensive.Rebounds'])
    TO <- sum(td[td$Player == unique(td$Player)[i], 'Turnovers'])
    REB <- sum(td[td$Player == unique(td$Player)[i], 'Total.Rebounds'])
    STL <- sum(td[td$Player == unique(td$Player)[i], 'Steals'])
    BLK <- sum(td[td$Player == unique(td$Player)[i], 'Blocks'])
    FTPerc <- sum(td[td$Player == unique(td$Player)[i], 'Free.Throws'])/sum(td[td$Player == unique(td$Player)[i], 'Free.Throw.Attempts'])  
    FG3M <- sum(td[td$Player == unique(td$Player)[i], 'X3.Point.Field.Goals'])
    PF <- sum(td[td$Player == unique(td$Player)[i], 'Personal.Fouls'])
    
    
    QAST <- ((MIN/(TMMIN/5))*(1.14*((TMAST-AST)/TMFGM)))+((((TMAST/TMMIN)*MIN*5-AST)/((TMFGM/TMMIN)*MIN*5-FGM))*(1-(MIN/(TMMIN/5))))#percentage of a player's shots that are assisted
    
    FGPart <- FGM*(1-0.5*((PTS-FTM)/(2*FGA))*QAST)#contribution to field goals
    ASTPart <- 0.5*(((TMPTS-TMFTM)-(PTS-FTM))/(2*(TMFGA-FGA)))*AST#contribution to assists
    FTPart <- (1-(1-(FTM/FTA))^2)*0.4*FTA#contribution to assists
    ORPart <- OR*TMORWeight*TMPlayPerc#contribution to offensive rebounds
    #EFG <- (FGM + 0.5*(FG3M))/FGA
    
    #WinScore <- (PTS+REB+STL+0.5*AST+0.5*BLK-FGA-TO-0.5*FTA-0.5*PF)/sum(td[td$Player == unique(td$Player)[i], 'Games'])
    
    MissFGPart <- (FGA-FGM)*(1-1.07*TMORPerc)#possessions that end due to a player's missed field goals
    MissFTPart <- ((1-(FTM/FTA))^2)*0.4*FTA#possessions that end due to a player's missed free throws
    
    scorPoss <- (FGPart+ASTPart+FTPart)*(1-(TMOR/TMScPoss)*TMORWeight*TMPlayPerc)+ORPart#scoring possessions
    Poss <- scorPoss + MissFGPart + MissFTPart+TO#individual's total possessions
    
    
    FGPart2 <- 2*(FGM+0.5*FG3M)*(1-0.5*((PTS-FTM)/(2*FGA))*QAST)#similar to FGPart, but accounts for 3-point shots made by the player
    ASTPart2 <- 2*((TMFGM - FGM + 0.5*(TMFG3M-FG3M))/(TMFGM-FGM))*0.5*(((TMPTS-TMFTM)-(PTS-FTM))/(2*(TMFGA-FGA)))*AST#similar to ASTPart, modified for how many points each assist creates
    FTPart2 = FTM
    ORPart2 <- OR*TMORWeight*TMPlayPerc*(TMPTS/(TMFGM+(1-(1-(TMFTM/TMFTA))^2)*0.4*TMFTA))  
    pointsProd <- (FGPart2+ASTPart2+FTM)*(1-(TMOR/TMScPoss)*TMORWeight*TMPlayPerc)+ORPart2#similar to scoring possessions, but accounts for number of points generated on each scoring possession
    ORtg <- 100*(pointsProd/Poss)#offensive rating
    margOff <- pointsProd - (0.92*AVGLGPPP) * Poss#marginal offense
    margPPW <- 0.32*(AVGLGPPG)*(TMPoss/AVGLGPoss)#marginal points per win
    offWinShares <- margOff/margPPW#offensive win shares
    mpg <- MIN/sum(td[td$Player == unique(td$Player)[i], 'Games'])#minutes per game
    #adjWinShares <- offWinShares/(MIN/sum(td[td$Player == unique(td$Player)[i], 'Games']))
    stat_line <- list(GP, mpg, offWinShares, offWinShares/GP, scorPoss/Poss, ORtg)
    name <- as.character(unique(td$Player)[i])
    
    valid <- !is.nan(scorPoss/Poss) & !is.nan(ORtg) & !is.nan(offWinShares)
    
    if(i==1 & GP > 10){

      result <- data.frame(c(name, stat_line),stringsAsFactors = FALSE)
      
      
    }
    else if (GP > 10){
      result[i,] = c(name, stat_line)   
    }
    
    
    
  }
  
  colnames(result) <- c("Player", "Games", "Minutes", "Offensive Win Shares", "OWS/Games Ratio", "Floor Percentage", "Offensive Rating")
  result = format(result,digits=2, nsmall=2)
  result = result[result$Minutes < min, ]
  #result = complete.cases(result)
  #result = result[result$"Minutes"<20,]
  result = result[result$Minutes > 10, ]
  result[order(result$"Offensive Win Shares", decreasing=TRUE), ]
}
