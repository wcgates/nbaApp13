getTeamAvg <- function(team){#returns average offensive and defensive ratings
  tmAverage <- sortTeamRatings()[sortTeamRatings()$Team==team,]
  name <- team
  stat_line <- list(tmAverage$"Offensive Efficiency", tmAverage$"Defensive Efficiency", tmAverage$"Net Rating")
  result = data.frame(c(name, stat_line))
  colnames(result) <- c("Team Averages", "Offensive Efficiency", "Defensive Efficiency", "Net Rating")
  result 
}