findGames <- function(team, gameID){
  rs_teamReb <- read.csv('rsTeamReb.csv')
  teamRebDF <- data.frame(rs_teamReb[rs_teamReb$Team ==team,])
  TMORPerc <- teamRebDF$OffensiveRebPerc
  
  teamOffAvg <- read.csv('TeamOffense12-13.csv')
  
  data <- read.csv('NBA v003.csv')
  rs_df <- data[data$playoff_ind == 0, ] 
  tT <- rs_df[rs_df$Game.ID == gameID, ]
  td <- tT[tT$Team == team,]
  
    TMMIN <- sum(td$Minutes)
    TMAST <- sum(td$Assists)
    TMFGM <- sum(td$Field.Goals)
    TMPTS <- sum(td$Points)
    TMFTM <- sum(td$Free.Throws)
    TMFTA <- sum(td$Free.Throw.Attempts)
    TMFGA <- sum(td$Field.Goal.Attempts)
    TMFGPerc <- sum(td$Field.Goals/td$Field.Goal.Attempts)
    TMFTPerc <- sum(td$Free.Throws/td$Free.Throw.Attempts)
    TMOR <- sum(td$Offensive.Rebounds)
    TMTO <- sum(td$Turnovers)
    TMFG3M <- sum(td$X3.Point.FieldGoals)
  
    TMScPoss <- TMFGM + (1-(1-(TMFTPerc))^2)*TMFTA*0.4
    TMPlayPerc <- TMScPoss/(TMFGA+TMFTA*0.4+TMTO)
    TMPoss <- (TMFGA - TMOR + TMTO + 0.4*TMFTA)/teamOffAvg[teamOffAvg$Team == team, 'G']
  
    
  
    TMORWeight <- ((1-TMORPerc)*TMPlayPerc)/((1-TMORPerc)*TMPlayPerc+TMORPerc*(1-TMPlayPerc))
  
    AVGLGPPP <- mean(teamOffAvg$ORTG)/100
    AVGLGPPG <- mean(teamOffAvg$PTS.G)
    AVGLGPoss <- mean(teamOffAvg$POSS)

    stat_line <- list(TMMIN, TMAST, TMFGM, TMPTS, TMFTM, TMFTA,TMFGA, TMFGPerc, TMFTPerc, TMOR, TMTO, TMFG3M, TMScPoss, TMPlayPerc, TMPoss, TMORPerc, TMORWeight, AVGLGPPP, AVGLGPPG, AVGLGPoss)
  
    
      result <- data.frame(c(team, gameID, stat_line))
    
  
  colnames(result) = c("Team", "Game", "TMMIN", "TMAST", "TMFGM", "TMPTS", "TMFTM", "TMFTA","TMFGA", "TMFGPerc", "TMFTPerc", "TMOR", "TMTO", "TMFG3M", "TMScPoss", "TMPlayPerc", "TMPoss", "TMORPerc", "TMORWeight", "AVGLGPPP", "AVGLGPPG", "AVGLGPoss")
  result
}