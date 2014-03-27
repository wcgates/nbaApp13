library(datasets)
library(shiny)

shinyServer(function(input, output) {
  
  data <- reactive({#returns rating table for corresponding team
    table<-switch(input$team,
           "Atlanta Hawks" = PlayerOffEff('ATL', input$mins),#match input to correct function call
           "Boston Celtics" = PlayerOffEff('BOS', input$mins),
           "Brooklyn Nets" = PlayerOffEff('BRK', input$mins),
           "Charlotte Bobcats" = PlayerOffEff('CHA', input$mins),
           "Chicago Bulls" = PlayerOffEff('CHI', input$mins),
           "Cleveland Cavaliers" = PlayerOffEff('CLE', input$mins),
           "Dallas Mavericks" = PlayerOffEff('DAL', input$mins),
           "Denver Nuggets" = PlayerOffEff('DEN', input$mins),
           "Detroit Pistons" = PlayerOffEff('DET', input$mins),
           "Golden State Warriors" = PlayerOffEff('GSW', input$mins), 
           "Houston Rockets" = PlayerOffEff('HOU', input$mins),
           "Indiana Pacers" = PlayerOffEff('IND', input$mins),
           "Los Angeles Clippers" = PlayerOffEff('LAC', input$mins),
           "Memphis Grizzlies" = PlayerOffEff('MEM', input$mins),
           "Miami Heat" = PlayerOffEff('MIA', input$mins),
           "Milwaukee Bucks" = PlayerOffEff('MIL', input$mins),
           "Minnesota Timberwolves" = PlayerOffEff('MIN', input$mins),
           "New Orleans Hornets" = PlayerOffEff('NOH', input$mins),
           "New York Knicks" = PlayerOffEff('NYK', input$mins),
           "Oklahoma City Thunder" = PlayerOffEff('OKC', input$mins),
           "Orlando Magic" = PlayerOffEff('ORL', input$mins), 
           "Philadelphia 76ers" = PlayerOffEff('PHI', input$mins),
           "Phoenix Suns" = PlayerOffEff('PHO', input$mins),
           "Portland Trail Blazers" = PlayerOffEff('POR', input$mins), 
           "Sacramento Kings" = PlayerOffEff('SAC', input$mins), 
           "San Antonio Spurs" = PlayerOffEff('SAS', input$mins),
           "Torontor Raptors" = PlayerOffEff('TOR', input$mins),
           "Utah Jazz" = PlayerOffEff('UTA', input$mins),
           "Washington Wizards" = PlayerOffEff('WAS', input$mins),
           )
   
    switch(input$stat,#decide which stat to sort by
          "Minutes" = table[order(table$"Minutes", decreasing=TRUE), ],
          "Offensive Win Shares" = table[order(as.numeric(table$"Offensive Win Shares"), decreasing=TRUE),],
          "OWS/Games Ratio" = table[order(as.numeric(table$"Offensive Win Shares"), decreasing=TRUE),],
          "Floor Percentage" = table[order(table$"Floor Percentage", decreasing=TRUE),], 
          "Offensive Rating" = table[order(table$"Offensive Rating", decreasing=TRUE),], 
    )
  })
      
  lineEmUp <- reactive({#returns lineup data for corresponding team 
    table <- switch(input$team,
    "Atlanta Hawks" = getLineups('ATL'),
    "Boston Celtics" = getLineups('BOS'),
    "Brooklyn Nets" = getLineups('BKN'),
    "Charlotte Bobcats" = getLineups('CHA'),
    "Chicago Bulls" = getLineups('CHI'),
    "Cleveland Cavaliers" = getLineups('CLE'),
    "Dallas Mavericks" = getLineups('DAL'),
    "Denver Nuggets" = getLineups('DEN'),
    "Detroit Pistons" = getLineups('DET'),
    "Golden State Warriors" = getLineups('GSW'), 
    "Houston Rockets" = getLineups('HOU'),
    "Indiana Pacers" = getLineups('IND'),
    "Los Angeles Clippers" = getLineups('LAC'),
    "Memphis Grizzlies" = getLineups('MEM'),
    "Miami Heat" = getLineups('MIA'),
    "Milwaukee Bucks" = getLineups('MIL'),
    "Minnesota Timberwolves" = getLineups('MIN'),
    "New Orleans Hornets" = getLineups('NOH'),
    "New York Knicks" = getLineups('NYK'),
    "Oklahoma City Thunder" = getLineups('OKC'),
    "Orlando Magic" = getLineups('ORL'), 
    "Philadelphia 76ers" = getLineups('PHI'),
    "Phoenix Suns" = getLineups('PHO'),
    "Portland Trail Blazers" = getLineups('POR'), 
    "Sacramento Kings" = getLineups('SAC'), 
    "San Antonio Spurs" = getLineups('SAS'),
    "Torontor Raptors" = getLineups('TOR'),
    "Utah Jazz" = getLineups('UTA'),
    "Washington Wizards" = getLineups('WAS'),  
  )
    
    switch(input$lineupStat,#decide which lineup stat to sort by
    "Minutes" = table[order(as.numeric(table$"Minutes"), decreasing = TRUE),],
    "Offensive Rating" = table[order(table$"Offensive Rating", decreasing=TRUE),], 
    "Defensive Rating" = table[order(table$"Defensive Rating"),], 
    "Net Rating" = table[order(table$"Net Rating", decreasing = TRUE), ],
    "+/-" = table[order(table$"+/-", decreasing=TRUE),]
    )
    
  })
  
  
  
    teamAbbrev <- reactive({#returns abbreviation for full team name
      abb <- switch(input$team,
      "Atlanta Hawks" = 'ATL',
      "Boston Celtics" = 'BOS',
      "Brooklyn Nets" = 'BKN',
      "Charlotte Bobcats" = 'CHA',
      "Chicago Bulls" = 'CHI',
      "Cleveland Cavaliers" = 'CLE',
      "Dallas Mavericks" = 'DAL',
      "Denver Nuggets" = 'DEN',
      "Detroit Pistons" = 'DET',
      "Golden State Warriors" = 'GSW', 
      "Houston Rockets" = 'HOU',
      "Indiana Pacers" = 'IND',
      "Los Angeles Clippers" = 'LAC',
      "Memphis Grizzlies" = 'MEM',
      "Miami Heat" = 'MIA',
      "Milwaukee Bucks" = 'MIL',
      "Minnesota Timberwolves" = 'MIN',
      "New Orleans Hornets" = 'NOH',
      "New York Knicks" = 'NYK',
      "Oklahoma City Thunder" = 'OKC',
      "Orlando Magic" = 'ORL', 
      "Philadelphia 76ers" = 'PHI',
      "Phoenix Suns" = 'PHO',
      "Portland Trail Blazers" = 'POR', 
      "Sacramento Kings" = 'SAC', 
      "San Antonio Spurs" = 'SAS',
      "Torontor Raptors" = 'TOR',
      "Utah Jazz" = 'UTA',
      "Washington Wizards" = 'WAS'  )  
    abb
    }
                          
                           )
    
    roster <- reactive({#returns team roster based on user input
      table <- switch(input$team,
                      "Atlanta Hawks" = getRoster('ATL'),
                      "Boston Celtics" = getRoster('BOS'),
                      "Brooklyn Nets" = getRoster('BKN'),
                      "Charlotte Bobcats" = getRoster('CHA'),
                      "Chicago Bulls" = getRoster('CHI'),
                      "Cleveland Cavaliers" = getRoster('CLE'),
                      "Dallas Mavericks" = getRoster('DAL'),
                      "Denver Nuggets" = getRoster('DEN'),
                      "Detroit Pistons" = getRoster('DET'),
                      "Golden State Warriors" = getRoster('GSW'), 
                      "Houston Rockets" = getRoster('HOU'),
                      "Indiana Pacers" = getRoster('IND'),
                      "Los Angeles Clippers" = getRoster('LAC'),
                      "Memphis Grizzlies" = getRoster('MEM'),
                      "Miami Heat" = getRoster('MIA'),
                      "Milwaukee Bucks" = getRoster('MIL'),
                      "Minnesota Timberwolves" = getRoster('MIN'),
                      "New Orleans Hornets" = getRoster('NOH'),
                      "New York Knicks" = getRoster('NYK'),
                      "Oklahoma City Thunder" = getRoster('OKC'),
                      "Orlando Magic" = getRoster('ORL'), 
                      "Philadelphia 76ers" = getRoster('PHI'),
                      "Phoenix Suns" = getRoster('PHO'),
                      "Portland Trail Blazers" = getRoster('POR'), 
                      "Sacramento Kings" = getRoster('SAC'), 
                      "San Antonio Spurs" = getRoster('SAS'),
                      "Torontor Raptors" = getRoster('TOR'),
                      "Utah Jazz" = getRoster('UTA'),
                      "Washington Wizards" = getRoster('WAS'),  
      )
    
    
  })  
  
  
  twoManUnit <- reactive({#uses input and abbreviation to get two man unit
    table <- twoManUnits(teamAbbrev(), input$player1, input$player2)
  })
  
  output$twoManTable <- renderTable({#calls above function to generate output, provided inputs are selected
    if(!is.null(input$player1) & !is.null(input$player2)){
      twoManUnit() 
    }
  })
  
  threeManUnit <- reactive({#gets three man unit provided inputs are selected
    if(!is.null(input$playerOne) & !is.null(input$playerTwo) & !is.null(input$playerThree)){
    table <- threeManUnits(teamAbbrev(), input$playerOne, input$playerTwo, input$playerThree)  
    }
  })
  
  output$threeManTable <- renderTable({#calls above function to generate output, provided inputs are selected
    threeManUnit() 
  })  

  
  output$choosePlayer1 <- reactiveUI(function() {#generates list of players to choose from as output
    df <- roster()
    selectInput("player1", "Select Player 1:", choices = listroster())                  
  })
  
  output$choosePlayer2 <- reactiveUI(function() {
    df <- roster()
    selectInput("player2", "Select Player 2:", choices = listroster())                  
  })
  
  
  output$choosePlayerOne <- reactiveUI(function() {
    df <- roster()
    selectInput("playerOne", "Select Player 1:", choices = listroster())                  
  })
  
  output$choosePlayerTwo <- reactiveUI(function() {
    df <- roster()
    selectInput("playerTwo", "Select Player 2:", choices = listroster())                  
  })
  
  output$choosePlayerThree <- reactiveUI(function() {
    df <- roster()
    selectInput("playerThree", "Select Player 3:", choices = listroster())                  
  })
  
  roster <- function(){
    table <- switch(input$team,
                    "Atlanta Hawks" = getRoster('ATL'),
                    "Boston Celtics" = getRoster('BOS'),
                    "Brooklyn Nets" = getRoster('BKN'),
                    "Charlotte Bobcats" = getRoster('CHA'),
                    "Chicago Bulls" = getRoster('CHI'),
                    "Cleveland Cavaliers" = getRoster('CLE'),
                    "Dallas Mavericks" = getRoster('DAL'),
                    "Denver Nuggets" = getRoster('DEN'),
                    "Detroit Pistons" = getRoster('DET'),
                    "Golden State Warriors" = getRoster('GSW'), 
                    "Houston Rockets" = getRoster('HOU'),
                    "Indiana Pacers" = getRoster('IND'),
                    "Los Angeles Clippers" = getRoster('LAC'),
                    "Memphis Grizzlies" = getRoster('MEM'),
                    "Miami Heat" = getRoster('MIA'),
                    "Milwaukee Bucks" = getRoster('MIL'),
                    "Minnesota Timberwolves" = getRoster('MIN'),
                    "New Orleans Hornets" = getRoster('NOH'),
                    "New York Knicks" = getRoster('NYK'),
                    "Oklahoma City Thunder" = getRoster('OKC'),
                    "Orlando Magic" = getRoster('ORL'), 
                    "Philadelphia 76ers" = getRoster('PHI'),
                    "Phoenix Suns" = getRoster('PHO'),
                    "Portland Trail Blazers" = getRoster('POR'), 
                    "Sacramento Kings" = getRoster('SAC'), 
                    "San Antonio Spurs" = getRoster('SAS'),
                    "Torontor Raptors" = getRoster('TOR'),
                    "Utah Jazz" = getRoster('UTA'),
                    "Washington Wizards" = getRoster('WAS'),  
    )
    
    #colnames(ros)<-c("Team", "Roster")
    table  
  }
  
  listroster <- function(){#calls roster function, returns list of players 
    df <- data.frame(roster())
    as.character( df$Player[c(1:length(df$Player))] )
  }
  
  fullNameRoster <- function(){
    df <- data()
    as.character( df$Player[c(1:length(df$Player))] )
  }
  
  output$choosePlayer <- reactiveUI(function() {
    #df <- fullNameRoster()
    selectInput("player", "Select Player:", choices = fullNameRoster())                  
  })
  
  output$gsPlot <- renderPlot({#calls plotGS function to generate output
    if(!is.null(input$player)){
    plotGS(teamAbbrev(), input$"player")
    }
    #print(p)
  })
  
  formulaText <- reactive({#title for table
    paste("Player Efficiency Ratings for the", input$team)
  })
  
  
  
  graph <- reactive({ #unused, calls plot function based on input 
    table <- switch(input$team,
           "Atlanta Hawks" = plotOWS('ATL'),
           "Boston Celtics" = plotOWS('BOS'),
           "Brooklyn Nets" = plotOWS('BRK'),
           "Charlotte Bobcats" = plotOWS('CHA'),
           "Chicago Bulls" = plotOWS('CHI'),
           "Cleveland Cavaliers" = plotOWS('CLE'),
           "Dallas Mavericks" = plotOWS('DAL'),
           "Denver Nuggets" = plotOWS('DEN'),
           "Detroit Pistons" = plotOWS('DET'),
           "Golden State Warriors" = plotOWS('GSW'), 
           "Houston Rockets" = plotOWS('HOU'),
           "Indiana Pacers" = plotOWS('IND'),
           "Los Angeles Clippers" = plotOWS('LAC'),
           "Memphis Grizzlies" = plotOWS('MEM'),
           "Miami Heat" = plotOWS('MIA'),
           "Milwaukee Bucks" = plotOWS('MIL'),
           "Minnesota Timberwolves" = plotOWS('MIN'),
           "New Orleans Hornets" = plotOWS('NOH'),
           "New York Knicks" = plotOWS('NYK'),
           "Oklahoma City Thunder" = plotOWS('OKC'),
           "Orlando Magic" = plotOWS('ORL'), 
           "Philadelphia 76ers" = plotOWS('PHI'),
           "Phoenix Suns" = plotOWS('PHO'),
           "Portland Trail Blazers" = plotOWS('POR'), 
           "Sacramento Kings" = plotOWS('SAC'), 
           "San Antonio Spurs" = plotOWS('SAS'),
           "Torontor Raptors" = plotOWS('TOR'),
           "Utah Jazz" = plotOWS('UTA'),
           "Washington Wizards" = plotOWS('WAS'),
           )
    table
  })
  
  output$owsPlot <- renderPlot({#calls plotOws to generate output
    p <- switch(input$gStat, 
    "Offensive Win Shares" = plotOWS(teamAbbrev(), input$mins),
    "Offensive Rating" = plotOR(teamAbbrev(), input$mins),
    "Floor Percentage" = plotFP(teamAbbrev(), input$mins))
    print(p)
  })
  
  output$ab <- renderPrint({
    teamAbbrev()
  })
  
  teamRatings <- reactive({#returns sorted table of team ratings
    table <- sortTeamRatings() 
    switch(input$teamStat,
          
           "Offensive Efficiency" = table[order(table$"Offensive Efficiency", decreasing=TRUE),],
           "Defensive Efficiency" = table[order(table$"Defensive Efficiency"), ], 
           "Net Rating" = table[order(as.numeric(table$"Net Rating"), decreasing=TRUE),]
    )
  })
  
 output$tmAvg <- renderTable({#various outputs
   getTeamAvg(teamAbbrev())
 })
  
  output$tmAvg2 <- renderTable({
    getTeamAvg(teamAbbrev())
  })
  
  output$tmAvg3 <- renderTable({
    getTeamAvg(teamAbbrev())
  })
  
  
  
  output$teams <- renderTable({
    teamRatings()
  })
  
  output$lineups <- renderTable({
    lineEmUp()
  })
  
    
  output$caption <- renderText({
    formulaText()
  })
  
  output$view <- renderTable({
    data()
  })
  
})