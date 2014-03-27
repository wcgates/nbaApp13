library(shiny)

shinyUI(
  
  pageWithSidebar(
  headerPanel("Fanalytics"),
  
  sidebarPanel(
    #allows for player to choose a team
    selectInput("team", "Choose a team:",
                choices = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Bobcats", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "Los Angeles Clippers", "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Hornets", "New York Knicks", "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Torontor Raptors", "Utah Jazz", "Washington Wizards")),
    
    sliderInput("mins", "Minute Ceiling: ",#maximum value for minutes per game to display
                min = 10,
                max = 48,
                value = 48
                ),
    
    br(),
    
    
    
    submitButton("Update view of team metrics"),#updates view based on new inputs
    br()
   
    
    ),
    
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Individual Efficiency Ratings",
        tabsetPanel(#multiple tabs for table and graph
        tabPanel(
          "Table", 
                selectInput("stat", "Sort players by:",#input for which stat to sort
                choices = c("Minutes", "Offensive Win Shares", "OWS/Games Ratio","Floor Percentage", "Offensive Rating")), 
               submitButton("Update view of player metrics"),
               br(),
               tableOutput("view")
        ),
        tabPanel(
          "Graph",
            selectInput("gStat", "Show graph for:",#input for which graph to show
                        choices = c("Offensive Win Shares", "Offensive Rating", "Floor Percentage")
                        ),
            submitButton("Update graph"),
            plotOutput(outputId = "owsPlot")
        )
                
        ) 
      ),
      tabPanel("Player Score Progressions",#tab for date vs. game score plot
               uiOutput("choosePlayer"),#calls server function that generates list of players based on which team is selected
               submitButton("Update player"),
               br(),
               plotOutput(outputId = "gsPlot")
               ),
      tabPanel("Five-man units", 
               selectInput("lineupStat", "Sort lineups by:",#input for which lineup stat to sort table by
                choices = c("Minutes", "Offensive Rating", "Defensive Rating", "Net Rating", "+/-")), submitButton("Update view of lineup metrics"), 
               br(),
               tableOutput("tmAvg"),#team averages for reference
               br(),
               tableOutput("lineups")),#20 most used lineups by minutes
      
      tabPanel("Two-man units",
               uiOutput("choosePlayer1"),#generates list of players to choose from
               uiOutput("choosePlayer2"), 
               
               submitButton("Update view of lineup metrics"),
               br(),
               tableOutput("tmAvg2"),
               br(),
               tableOutput("twoManTable")
               #listroster()
      ),
      
      tabPanel("Three-man units",
               uiOutput("choosePlayerOne"), #generates list of players to choose from
               uiOutput("choosePlayerTwo"),
               uiOutput("choosePlayerThree"),
               submitButton("Update view of lineup metrics"),
               tableOutput("tmAvg3"),
               br(),
               tableOutput("threeManTable") 
            )
    )
  )
)

)   


