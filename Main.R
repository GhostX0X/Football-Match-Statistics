# Install required packages
install.packages("shiny")
install.packages("shinydashboard")

# Load the required libraries
library(shiny)
library(shinydashboard)

# Define the user interface
ui <- dashboardPage(
  dashboardHeader(title = "Football Match Statistics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input", icon = icon("edit")),
      menuItem("Table", tabName = "table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "input",
        fluidRow(
          box(
            title = "Enter Match Statistics",
            width = 6,
            textInput("team1", "Team 1"),
            numericInput("goals1", "Goals Scored (Team 1)", value = 0),
            numericInput("shots1", "Shots Taken (Team 1)", value = 0),
            br(),
            textInput("team2", "Team 2"),
            numericInput("goals2", "Goals Scored (Team 2)", value = 0),
            numericInput("shots2", "Shots Taken (Team 2)", value = 0),
            actionButton("addButton", "Add Statistics"),
            br(),
            actionButton("resetButton", "Reset")
          )
        )
      ),
      tabItem(
        tabName = "table",
        fluidRow(
          box(
            title = "Match Statistics Table",
            width = 6,
            tableOutput("matchStatsTable")
          ),
          box(
            title = "League Standings",
            width = 6,
            tableOutput("leagueStandings")
          )
        ),
        fluidRow(
          box(
            title = "Match Winner",
            width = 6,
            textOutput("winnerOutput")
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Initialize an empty match statistics data frame
  matchStats <- reactiveValues(data = data.frame(
    Team = character(),
    Goals = numeric(),
    Shots = numeric(),
    Points = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Add match statistics to the data frame
  observeEvent(input$addButton, {
    team1 <- input$team1
    goals1 <- input$goals1
    shots1 <- input$shots1
    team2 <- input$team2
    goals2 <- input$goals2
    shots2 <- input$shots2
    
    if (team1 != "" && goals1 >= 0 && shots1 >= 0 && team2 != "" && goals2 >= 0 && shots2 >= 0) {
      if (team1 %in% matchStats$data$Team) {
        matchStats$data$Goals[matchStats$data$Team == team1] <- matchStats$data$Goals[matchStats$data$Team == team1] + goals1
        matchStats$data$Shots[matchStats$data$Team == team1] <- matchStats$data$Shots[matchStats$data$Team == team1] + shots1
      } else {
        newStats1 <- data.frame(
          Team = team1,
          Goals = goals1,
          Shots = shots1,
          Points = 0,
          stringsAsFactors = FALSE
        )
        matchStats$data <- rbind(matchStats$data, newStats1)
      }
      
      if (team2 %in% matchStats$data$Team) {
        matchStats$data$Goals[matchStats$data$Team == team2] <- matchStats$data$Goals[matchStats$data$Team == team2] + goals2
        matchStats$data$Shots[matchStats$data$Team == team2] <- matchStats$data$Shots[matchStats$data$Team == team2] + shots2
      } else {
        newStats2 <- data.frame(
          Team = team2,
          Goals = goals2,
          Shots = shots2,
          Points = 0,
          stringsAsFactors = FALSE
        )
        matchStats$data <- rbind(matchStats$data, newStats2)
      }
      
      # Update points based on match results
      if (goals1 > goals2) {
        matchStats$data$Points[matchStats$data$Team == team1] <- matchStats$data$Points[matchStats$data$Team == team1] + 3
        matchStats$data$Points[matchStats$data$Team == team2] <- matchStats$data$Points[matchStats$data$Team == team2]
      } else if (goals1 < goals2) {
        matchStats$data$Points[matchStats$data$Team == team1] <- matchStats$data$Points[matchStats$data$Team == team1]
        matchStats$data$Points[matchStats$data$Team == team2] <- matchStats$data$Points[matchStats$data$Team == team2] + 3
      } else {
        matchStats$data$Points[matchStats$data$Team == team1] <- matchStats$data$Points[matchStats$data$Team == team1] + 1
        matchStats$data$Points[matchStats$data$Team == team2] <- matchStats$data$Points[matchStats$data$Team == team2] + 1
      }
      
      # Reset input fields
      updateTextInput(session, "team1", value = "")
      updateNumericInput(session, "goals1", value = 0)
      updateNumericInput(session, "shots1", value = 0)
      updateTextInput(session, "team2", value = "")
      updateNumericInput(session, "goals2", value = 0)
      updateNumericInput(session, "shots2", value = 0)
    }
  })
  
  # Reset match statistics
  observeEvent(input$resetButton, {
    matchStats$data <- data.frame(
      Team = character(),
      Goals = numeric(),
      Shots = numeric(),
      Points = numeric(),
      stringsAsFactors = FALSE
    )
  })
  
  # Render the match statistics table
  output$matchStatsTable <- renderTable({
    matchStats$data
  }, rownames = FALSE)
  
  # Calculate and render the league standings
  output$leagueStandings <- renderTable({
    if (nrow(matchStats$data) > 0) {
      teams <- unique(matchStats$data$Team)
      standings <- data.frame(
        Team = teams,
        Matches = sapply(teams, function(team) sum(matchStats$data$Team == team)),
        Wins = sapply(teams, function(team) sum(matchStats$data$Team == team & matchStats$data$Goals > matchStats$data$Goals2)),
        Losses = sapply(teams, function(team) sum(matchStats$data$Team == team & matchStats$data$Goals < matchStats$data$Goals2)),
        Draws = sapply(teams, function(team) sum(matchStats$data$Team == team & matchStats$data$Goals == matchStats$data$Goals2)),
        GoalsFor = sapply(teams, function(team) sum(matchStats$data$Goals[matchStats$data$Team == team])),
        GoalsAgainst = sapply(teams, function(team) sum(matchStats$data$Goals2[matchStats$data$Team == team])),
        Points = sapply(teams, function(team) sum(matchStats$data$Points[matchStats$data$Team == team])),
        stringsAsFactors = FALSE
      )
      standings <- standings[order(-standings$Points, -standings$GoalsFor), ]
      standings
    }
  }, rownames = FALSE)
  
  # Determine and display the winner
  output$winnerOutput <- renderText({
    if (nrow(matchStats$data) > 0) {
      team1_goals <- matchStats$data$Goals[matchStats$data$Team == input$team1]
      team2_goals <- matchStats$data$Goals[matchStats$data$Team == input$team2]
      
      if (sum(team1_goals) > sum(team2_goals)) {
        paste("Winner:", input$team1)
      } else if (sum(team1_goals) < sum(team2_goals)) {
        paste("Winner:", input$team2)
      } else {
        "Match Ended in a Draw"
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
