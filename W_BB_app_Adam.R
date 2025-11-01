library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(RPostgres)
library(DT)
library(dplyr)
library(xgboost)
library(DBI)
library(pool)
library(toastui)
library(ggplot2)
library(lubridate)
library(av)
library(aws.s3)
library(plotly)


options(shiny.sanitize.errors = TRUE)      # Sanitize errors
options(shiny.timeout = 3200)              # Increase the idle timeout to 60 minutes
options(shiny.maxRequestSize = 6*1024^3)  # 6 GB limit



players <- data.frame(
  name = c("Kiara Smith", "Stevi Lockhart", "Gracy Wernli", "Ellie Brueggemann",
           "Aalayah Wilson", "Brooke Coffey", "Alyssa Nielsen", "Mary McGrath",
           "Mya Skoff", "Tina Winn", "Mykayla Cunningham", "Mariah Stewart",
           "Gracie Kelsey", "Justis Odom"),
  number = c(  0, 2, 3, 4,
               5, 12, 15, 21,
               22, 23, 24, 26,
               33, 35),
  position = c("G", "G", "G", "G",
               "G", "G", "F", "G",
               "G", "F", "G", "C",
               "F", "C")
  
)


# UI definition
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      body, .main-header {
        background-color: black !important;
        color: #B5A36A !important;
      }
      .skin-blue .main-header .logo {
        background-color: black !important;
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar {
        background-color: black !important;
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar .sidebar-toggle {
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar .navbar-custom-menu > .dropdown > a {
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar .navbar-nav > li > a {
        color: #B5A36A !important;
      }
      .skin-blue .main-header .navbar .navbar-nav > li > a:hover {
        background-color: #292929 !important;
      }
      
      /* Style for all DataTables table headers */
      .dataTables_wrapper .dataTable thead {
        background-color: #B5A36A; /* Header background color */
        color: #333; /* Header text color */
        font-size: 14px; /* Header font size */
      }
      
      #videoProgressPitching 
      .progress-bar {
      background-color: #B5A36A;
    }
      
    "))
  ),
  div(id = "loginpage", 
      wellPanel(
        textInput("userName", "Username"),
        passwordInput("passwd", "Password"),
        actionButton("login", "Log in", icon = icon("sign-in"))
      )
  ),
  div(id = "homepage", style = "display: none;",
      dashboardPage(
        dashboardHeader(
          title = "ROAR",
          titleWidth = 250
          
        ),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("house")),
            menuItem("Game Charter", tabName = "gameCharter", icon = icon("basketball")),
            menuItem("Game Report", tabName = "gameReport", icon = icon("basketball")),
            menuItem("Player Shot Analysis", tabName = "playerShotAnalysis", icon = icon("bullseye"))
          ),
          tags$style(".sidebar-search-container { display: none; }")
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = "home",
                    fluidPage(
                      titlePanel("ROAR HomePage"),
                      tags$style(HTML("
                        .full-width-img {
                          width: 100%;
                          height: auto;
                          display: block;
                        }
                      ")),
                      tags$img(src = "LionsLogo.png", class = "full-width-img")
                    )
            ),
            
            tabItem(tabName = "gameCharter",
                    
                    
                    uiOutput("setUp"),
                    uiOutput("startingLineups"),
                    uiOutput("charting"),
                    uiOutput("edit")
            ),
            
            tabItem(tabName = "gameReport",
                    fluidPage(
                      titlePanel("Game Report"),
                      selectInput("games", "Select Game", choices = NULL),
                      DTOutput("gameReportTable"), 
                      br(),
                      h3("Opponent Report"),  
                      DTOutput("opponentReportTable") 
                    )
            ),
            tabItem(tabName = "playerShotAnalysis",
                    fluidPage(
                      titlePanel("Player Shot Analysis"),
                      selectInput("selected_game", "Select Game", choices = NULL),
                      fluidRow(
                        column(6, selectInput("playerName", "Select Player Name", 
                                              choices = c("All Players", players$name))),
                        column(6, selectInput("shotType", "Select Shot Type", choices = c("All Shots", "Off the Dribble", "Off the Catch")))
                      ),
                      ),
                      plotOutput("playerCourt")
                    )
            )
          )
        )
      )
  )


# Server logic
server <- function(input, output, session) {
  # Hardcoded credentials (for demonstration purposes)
  credentials <- reactiveValues(
    username = "Lions",
    password = "1176"
  )
  
  observeEvent(input$login, {
    username <- isolate(input$userName)
    password <- isolate(input$passwd)
    
    if (username == credentials$username && password == credentials$password) {
      shinyjs::hide("loginpage")
      shinyjs::show("homepage")
    } else {
      shinyjs::alert("Invalid username or password!")
    }
  })
  
  
  ####################################### GAME CHARTER ########################################
  GCState <- reactiveVal("SetUP")
  
  output$GCState <- renderText({
    GCState()
  })
  
  output$setUp <- renderUI({
    
    if(GCState() == "SetUP"){
      tagList(
        fluidRow(
          column(3, dateInput("gameDate", "Game Date", value = Sys.Date())),
          column(3, selectizeInput("oppo", "Opponent", choices = c("Scrimmage"), options = list(create = TRUE))),
          column(3, selectInput("H_A", "Home or Away", choices = c("Home", "Away", "Scrimmage")))
        )
      )
    }
    
    
    
    
    
    
    
  })
  
  output$startingLineups <- renderUI({
    
    req(GCState() == "SetUP")
    req(input$H_A)
    
    if (input$H_A == "Scrimmage"){
      tagList(
        fluidRow(
          column(3, selectInput("LU_1", "Lindenwood Home Team 1", choices = players$number)),
          column(3, selectInput("Oppo_1", "Lindenwood Away Team 1", choices = players$number))
        ),
        fluidRow(
          column(3, selectInput("LU_2", "Lindenwood Home Team 2", choices = players$number)),
          column(3, selectInput("Oppo_2", "Lindenwood Away Team 2", choices = players$number))
        ),
        fluidRow(
          column(3, selectInput("LU_3", "Lindenwood Home Team 3", choices = players$number)),
          column(3, selectInput("Oppo_3", "Lindenwood Away Team 3", choices = players$number))
        ),
        fluidRow(
          column(3, selectInput("LU_4", "Lindenwood Home Team 4", choices = players$number)),
          column(3, selectInput("Oppo_4", "Lindenwood Away Team 4", choices = players$number))
        ),
        fluidRow(
          column(3, selectInput("LU_5", "Lindenwood Home Team 5", choices = players$number)),
          column(3, selectInput("Oppo_5", "Lindenwood Away Team 5", choices = players$number))
        )
        
      )
    } else {
      tagList(
        fluidRow(
          column(3, selectInput("LU_1", "Lindenwood 1", choices = players$number)),
          column(3, textInput("Oppo_1", "Opposing Team 1", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_2", "Lindenwood 2", choices = players$number)),
          column(3, textInput("Oppo_2", "Opposing Team 2", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_3", "Lindenwood 3", choices = players$number)),
          column(3, textInput("Oppo_3", "Opposing Team 3", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_4", "Lindenwood 4", choices = players$number)),
          column(3, textInput("Oppo_4", "Opposing Team 4", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_5", "Lindenwood 5", choices = players$number)),
          column(3, textInput("Oppo_5", "Opposing Team 5", value = 0))
        ),
        
        actionButton("startGame", "Start Game")
        
      )
    }
  })
  
  gameValues <- reactiveValues(
    HOME_1 = 0,
    HOME_2 = 0, 
    HOME_3 = 0,
    HOME_4 = 0,
    HOME_5 = 0,
    AWAY_1 = 0,
    AWAY_2 = 0,
    AWAY_3 = 0,
    AWAY_4 = 0,
    AWAY_5 = 0,
    opponent = "",
    date = Sys.Date(),
    quarter = 0,
    home_away = ""
  )
  
  observeEvent(input$startGame, {
    # Update game state to "charting"
    GCState("charting")
    
    # Set home and away lineups based on Home/Away/Scrimmage selection
    if (input$H_A %in% c("Home", "Scrimmage")) {
      gameValues$HOME_1 <- input$LU_1
      gameValues$HOME_2 <- input$LU_2
      gameValues$HOME_3 <- input$LU_3
      gameValues$HOME_4 <- input$LU_4
      gameValues$HOME_5 <- input$LU_5
      gameValues$AWAY_1 <- input$Oppo_1
      gameValues$AWAY_2 <- input$Oppo_2
      gameValues$AWAY_3 <- input$Oppo_3
      gameValues$AWAY_4 <- input$Oppo_4
      gameValues$AWAY_5 <- input$Oppo_5
    } else {
      gameValues$HOME_1 <- input$Oppo_1
      gameValues$HOME_2 <- input$Oppo_2
      gameValues$HOME_3 <- input$Oppo_3
      gameValues$HOME_4 <- input$Oppo_4
      gameValues$HOME_5 <- input$Oppo_5
      gameValues$AWAY_1 <- input$LU_1
      gameValues$AWAY_2 <- input$LU_2
      gameValues$AWAY_3 <- input$LU_3
      gameValues$AWAY_4 <- input$LU_4
      gameValues$AWAY_5 <- input$LU_5
    }
    
    # Set game metadata
    gameValues$opponent <- input$oppo
    gameValues$date <- input$gameDate
    gameValues$home_away <- input$H_A
    gameValues$quarter <- 1  # Starting with first quarter
    
    
  })
  
  on_court_lindenwood <- reactive({
    c(
      gameValues$HOME_1,
      gameValues$HOME_2,
      gameValues$HOME_3,
      gameValues$HOME_4,
      gameValues$HOME_5
    )
  })
  on_court_opponent <- reactive({
    c(
      gameValues$AWAY_1,
      gameValues$AWAY_2,
      gameValues$AWAY_3,
      gameValues$AWAY_4,
      gameValues$AWAY_5
    )
  })
  
  output$charting <- renderUI({
    
    if (GCState() == "charting") {
      tagList(
        radioButtons("eventType", "Shot or Rebound or Free Throw", choices = c("Shot", "Rebound", "Free Throw"), inline = TRUE),
        tags$h4("Select Shooter/Rebounder"),
        fluidRow(
          column(6,
                 tags$h5("Lindenwood Players"),
                 radioButtons("eventPerson", NULL, choices = setNames(on_court_lindenwood(), paste( on_court_lindenwood())), inline = TRUE)),
          column(6,
                 tags$h5("Opponent Players"),
                 radioButtons("eventPerson", NULL, choices = setNames(on_court_opponent(), paste( on_court_opponent())), inline = TRUE))),
        conditionalPanel("input.eventType == 'Shot'", 
                         fluidRow(
                           column(4, 
                                  radioButtons("dbocShot", "Shot Off the Catch or Dribble", choices = c("Catch", "Dribble"), inline = TRUE)),
                           column(4, 
                                  radioButtons("shotOutcomeShot", "Make or Miss Shot", choices = c("Make", "Miss"), inline = TRUE)))),
        conditionalPanel("input.eventType == 'Rebound'",  
                         radioButtons("offOrDefRebound", "Defensive or Offensive Rebound", choices = c("Defensive", "Offensive"), inline = TRUE)),
        conditionalPanel("input.eventType == 'Free Throw'", 
                         fluidRow(
                           column(4, 
                                  radioButtons("shotOutcomeFT", "Make or Miss Free Throw", choices = c("Make", "Miss"), inline = TRUE)))),
        plotOutput("court", click = "courtClick"),
        actionButton("submitBtn", "Submit"),
        actionButton("editBtn", "Edit Situation")
      )
    }
  })
  
  
  
  
  output$court <- renderPlot({
    par(mai = c(0, 0, 0, 0))
    plot(1, ylim = c(-30, 30), xlim = c(-50, 50), type = 'n', yaxs = 'i', xaxs = 'i', ylab = '', xlab = '', axes = F, asp = 1)
    
    #out of bounds
    rect(xleft = -47, ybottom = -25,
         xright = 47, ytop = 25,
         border = "black", lwd = 2)
    
    #half-court line
    segments(0, -25, 0, 25, col = "black", lwd = 2)
    
    #center circle
    symbols(x = 0, y = 0, circles = 6, inches = FALSE, add = TRUE, fg = "black", lwd = 2)
    
    #backboard
    segments(-43,3,-43,-3, col = "black", lwd = 2)
    segments(43,3,43,-3, col = "black", lwd = 2)
    
    #paint
    rect(-47, -7.5, -28, 7.5, border = "black", lwd = 2)
    rect(28, -7.5, 47, 7.5, border = "black", lwd = 2)
    
    #free throw cirlces
    curve(sqrt(6^2 - (x + 28)^2), from = -28, to = -22, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(6^2 - (x + 28)^2), from = -28, to = -22, add = TRUE, col = "black", lwd = 2)
    curve(sqrt(6^2 - (x - 28)^2), from = 22, to = 28, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(6^2 - (x - 28)^2), from = 22, to = 28, add = TRUE, col = "black", lwd = 2)
    
    #hoops
    symbols(x = -41.75, y = 0, circles = 1.5/2, inches = FALSE, add = TRUE, fg = "black", lwd = 2)  # Left hoop
    symbols(x = 41.75, y = 0, circles = 1.5/2, inches = FALSE, add = TRUE, fg = "black", lwd = 2)   # Right hoop
    
    
    #3pt lines
    segments(-47, -21.65625, -37, -21.65625, col = "black", lwd = 2)
    segments(-47, 21.65625, -37, 21.65625, col = "black", lwd = 2)
    
    segments(47, -21.65625, 37, -21.65625, col = "black", lwd = 2)
    segments(47, 21.65625, 37, 21.65625, col = "black", lwd =2)
    
    #3pt arcs
    curve(sqrt(22.14583^2 - (x + 41.75)^2), from = -37, to = -19.60417, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(22.14583^2 - (x + 41.75)^2), from = -37, to = -19.60417, add = TRUE, col = "black", lwd = 2)
    
    curve(sqrt(22.14583^2 - (x - 41.75)^2), from = 37, to = 19.60417, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(22.14583^2 - (x - 41.75)^2), from = 37, to = 19.60417, add = TRUE, col = "black", lwd = 2)
    
    #Event Locs
    if (!is.null(GCCourtPoint$temp_court$x)) {
      points(
        GCCourtPoint$temp_court$x,
        GCCourtPoint$temp_court$y,
        col = "green",
        pch = ifelse(input$eventType == "Shot", 19, 
                     ifelse(input$eventType == "Rebound", 15, 17)), cex = 1.5)
    }
    
    if (!is.null(GCCourtPoint$permanent$x)) {
      points(
        GCCourtPoint$permanent$x,
        GCCourtPoint$permanent$y,
        col = "red",
        pch = ifelse(GCCourtPoint$permanent$event == "Shot", 19, 
                     ifelse(GCCourtPoint$permanent$event == "Rebound", 15, 17)), cex = 1.5)
    }
    
    
  }, bg = "transparent")
  
  
  
  observeEvent(input$submitBtn, {
    # Check if temporary points exist
    if (nrow(GCCourtPoint$temp_court) == 0) {
      showNotification("Error: No event location selected on the court.", type = "error", duration = 3)
      return()
    }
    
    # Check if necessary inputs are selected based on event type
    if (is.null(input$eventType)) {
      showNotification("Error: Please select an event type (Shot, Rebound, Free Throw).", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$eventPerson)) {
      showNotification("Error: Please select the Shooter/Rebounder.", type = "error", duration = 3)
      return()
    }
    
    if (input$eventType == "Shot") {
      if (is.null(input$shotOutcomeShot) || is.null(input$dbocShot)) {
        showNotification("Error: Please select both Shot Outcome and Catch/Dribble.", type = "error", duration = 3)
        return()
      }
    } else if (input$eventType == "Rebound") {
      if (is.null(input$offOrDefRebound)) {
        showNotification("Error: Please select Offensive or Defensive rebound.", type = "error", duration = 3)
        return()
      }
    } else if (input$eventType == "Free Throw") {
      if (is.null(input$shotOutcomeFT)) {
        showNotification("Error: Please select Make or Miss for Free Throw.", type = "error", duration = 3)
        return()
      }
    }
    # Validate input
    if (is.null(input$eventType) || is.null(input$eventPerson)) {
      showNotification("Please complete all fields before submitting.", type = "error", duration = 3)
      return()
    }
    
    playerID <- input$eventPerson
    event_team <- ifelse(playerID %in% c(gameValues$HOME_1, gameValues$HOME_2, gameValues$HOME_3, gameValues$HOME_4, gameValues$HOME_5), "Lindenwood", "Opponent")
    print("IN")
    eventX <- round(GCCourtPoint$temp_court$x, 2)
    eventY <- round(GCCourtPoint$temp_court$y, 2)
    Time <- format(Sys.time(), tz = "America/Chicago")
    
    # Clear the temporary points after submitting
    GCCourtPoint$temp_court <- data.frame(x = numeric(0), y = numeric(0))
    
    if (eventX > 0){
      distance <- ((eventX-41.75)^2 + (eventY)^2)^0.5
    } else {
      distance <- ((eventX+41.75)^2 + (eventY)^2)^0.5
    }
    
    
    print(distance)
    event<-input$eventType
    
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    
    shotCategory <- "FG"
    tryCatch({
      if (event == "Shot") {
        shotResult <- input$shotOutcomeShot
        shotType <- input$dbocShot
        # Function to classify shot zones
        event_zone <- function(distance) {
          if (distance < 10) {
            return("short")
          } else if (distance >= 10 & distance <= 20) {
            return("mid")
          } else {
            return("long")
          }
        }
        shot_zone <- event_zone(distance)
        
        # Function for 3pt or not
        if (distance > 22 && eventX != 0) {
          shotCategory <- "Three Point"  
        } else if (eventY == 0) { 
          shotCategory <- "Free Throw"
        } else {
          shotCategory <- "Field Goal" 
        }
        
        
        shotTypeValue <- ifelse(shotCategory == "Three Point", 1, 0)  # 1 for 3-point shot, 0 for other shots
        fgTypeValue <- ifelse(shotCategory == "Free Throw", 0, 1)  # 0 for Free Throw, 1 for FG
        ftValue <- ifelse(shotCategory == "Free Throw", 1, 0)  # 1 for Free Throw, 0 otherwise
        
        
        print(playerID)
        print(eventX)
        print(eventY)
        print(distance)
        print(shotResult)
        print(shotType)
        print(shotTypeValue)
        print(shotCategory)
        print(fgTypeValue)
        print(gameValues$date)
        print(gameValues$opponent)
        print(gameValues$home_away)
        print(gameValues$quarter)
        print(Time)
        print(gameValues$HOME_1)
        print(gameValues$HOME_2)
        print(gameValues$HOME_3)
        print(gameValues$HOME_4)
        print(gameValues$HOME_5)
        print(gameValues$AWAY_1)
        print(gameValues$AWAY_2)
        print(gameValues$AWAY_3)
        print(gameValues$AWAY_4)
        print(gameValues$AWAY_5)
        
        # Insert shot data into database
        insert_query <- glue::glue(
          "
    INSERT INTO w_basketball_game_chart_t(
      event, event_person, event_x, event_y, event_distance, event_zone, made_miss, free_throw, three_pt, dribble_catch, fg, 
      h1, h2, h3, h4, h5, a1, a2, a3, a4, a5, 
      game_date, opponent, home_away, quarter, time_of_day, event_team)
    
    VALUES(
      'Shot', '{playerID}', {eventX}, {eventY}, {distance}, '{shot_zone}', '{shotResult}', '{ftValue}', {shotTypeValue}, '{shotType}', {fgTypeValue},
      {gameValues$HOME_1}, {gameValues$HOME_2}, {gameValues$HOME_3}, {gameValues$HOME_4}, {gameValues$HOME_5}, '{gameValues$AWAY_1}', '{gameValues$AWAY_2}', '{gameValues$AWAY_3}', '{gameValues$AWAY_4}', '{gameValues$AWAY_5}',
      '{gameValues$date}', '{gameValues$opponent}', '{gameValues$home_away}', {gameValues$quarter}, '{Time}', '{event_team}'
    )
    "
        )
        
        print(insert_query)
        
        dbExecute(con, insert_query)
        dbDisconnect(con)
        
        print("Done")
      }
      
      #rebound 
      if (event == "Rebound") {
        reboundResult <- input$offOrDefRebound
        # Function to classify shot zones
        event_zone <- function(distance) {
          if (distance < 10) {
            return("short")
          } else if (distance >= 10 & distance <= 20) {
            return("mid")
          } else {
            return("long")
          }
        }
        rebound_zone <- event_zone(distance)
        
        # Function to classify shot recovery zones
        shot_recovery_zone <- function(distance) {
          if (distance < 10) {
            return("short")
          } else if (distance >= 10 & distance <= 20) {
            return("mid")
          } else {
            return("long")
          }
        }
        recovery_zone <- shot_recovery_zone(distance)
        
        # Print for debugging
        print(playerID)
        print(eventX)
        print(eventY)
        print(distance)
        print(reboundResult)
        print(gameValues$date)
        print(gameValues$opponent)
        print(gameValues$home_away)
        print(gameValues$quarter)
        print(Time)
        print(gameValues$HOME_1)
        print(gameValues$HOME_2)
        print(gameValues$HOME_3)
        print(gameValues$HOME_4)
        print(gameValues$HOME_5)
        print(gameValues$AWAY_1)
        print(gameValues$AWAY_2)
        print(gameValues$AWAY_3)
        print(gameValues$AWAY_4)
        print(gameValues$AWAY_5)
        
        # Insert rebound data into database
        insert_query <- glue::glue(
          "
    INSERT INTO w_basketball_game_chart_t(
      event, event_person, event_x, event_y, event_distance, event_zone, off_def, shot_recovery_zone, 
      h1, h2, h3, h4, h5, a1, a2, a3, a4, a5, 
      game_date, opponent, home_away, quarter, time_of_day, event_team)
    
    VALUES(
      'Rebound', '{playerID}', {eventX}, {eventY}, {distance}, '{rebound_zone}', '{reboundResult}', '{recovery_zone}',
      {gameValues$HOME_1}, {gameValues$HOME_2}, {gameValues$HOME_3}, {gameValues$HOME_4}, {gameValues$HOME_5}, '{gameValues$AWAY_1}', '{gameValues$AWAY_2}', '{gameValues$AWAY_3}', '{gameValues$AWAY_4}', '{gameValues$AWAY_5}',
      '{gameValues$date}', '{gameValues$opponent}', '{gameValues$home_away}', {gameValues$quarter}, '{Time}', '{event_team}'
    )
    "
        )
        
        print(insert_query)
        
        dbExecute(con, insert_query)
        dbDisconnect(con)
        
        print("Done")
        
      }
      
      #free throw 
      if (event == "Free Throw") {  
        distance <- 15  # Fixed distance for free throws
        
        # Shot result (make or miss)
        ftResult <- input$shotOutcomeFT  
        
        # Free throw shotCategory and fg settings
        shotCategory <- "Free Throw"
        shotTypeValue <- 0  # Free throw is not a 3-pointer
        fgTypeValue <- 0  # Free throw doesn't count as a field goal
        ftValue <- 1  # Free throw shot is always 1
        
        
        insert_query <- glue::glue(
          "
    INSERT INTO w_basketball_game_chart_t(
      event, event_person, event_x, event_y, event_distance, made_miss, free_throw, three_pt, fg, 
      h1, h2, h3, h4, h5, a1, a2, a3, a4, a5, 
      game_date, opponent, home_away, quarter, time_of_day, event_team)
    
    VALUES(
      'Free Throw', '{playerID}', {eventX}, {eventY}, {distance}, '{ftResult}', '{ftValue}', {shotTypeValue}, {fgTypeValue},
      {gameValues$HOME_1}, {gameValues$HOME_2}, {gameValues$HOME_3}, {gameValues$HOME_4}, {gameValues$HOME_5}, '{gameValues$AWAY_1}', '{gameValues$AWAY_2}', '{gameValues$AWAY_3}', '{gameValues$AWAY_4}', '{gameValues$AWAY_5}',
      '{gameValues$date}', '{gameValues$opponent}', '{gameValues$home_away}', {gameValues$quarter}, '{Time}', '{event_team}'
    )
    "
        )
        
        print(insert_query)
        dbExecute(con, insert_query)
        dbDisconnect(con)
        
        print("Done")
      }
      showNotification("Event successfully recorded!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification("Error: Could not save data to the database.", type = "error", duration = 3)
    }, finally = {
    })
  })
  
  GCCourtPoint <- reactiveValues(
    temp_court = data.frame(x = numeric(0), y = numeric(0)),
    permanent = data.frame(x = numeric(0), y = numeric(0), event = NULL)
  )
  
  observeEvent(input$courtClick,{
    x <- input$courtClick$x
    y <- input$courtClick$y
    new_point <- data.frame(x = x, y = y)
    GCCourtPoint$temp_court <- new_point
  })
  
  observeEvent(input$editBtn, {
    # Open modal to edit players and quarter
    showModal(modalDialog(
      title = "Edit Situation",
      fluidRow(
        column(6, 
               selectInput("editHome1", "Home Player 1", choices = players$number, selected = gameValues$HOME_1)),
        column(6, 
               textInput("editAway1", "Away Player 1", value = gameValues$AWAY_1))
      ),
      fluidRow(
        column(6, 
               selectInput("editHome2", "Home Player 2", choices = players$number, selected = gameValues$HOME_2)),
        column(6, 
               textInput("editAway2", "Away Player 2", value = gameValues$AWAY_2))
      ),
      fluidRow(
        column(6, 
               selectInput("editHome3", "Home Player 3", choices = players$number, selected = gameValues$HOME_3)),
        column(6, 
               textInput("editAway3", "Away Player 3", value = gameValues$AWAY_3))
      ),
      fluidRow(
        column(6, 
               selectInput("editHome4", "Home Player 4", choices = players$number, selected = gameValues$HOME_4)),
        column(6, 
               textInput("editAway4", "Away Player 4", value = gameValues$AWAY_4))
      ),
      fluidRow(
        column(6, 
               selectInput("editHome5", "Home Player 5", choices = players$number, selected = gameValues$HOME_5)),
        column(6, 
               textInput("editAway5", "Away Player 5", value = gameValues$AWAY_5))
      ),
      fluidRow(
        column(6, 
               selectInput("editQuarter", "Quarter", choices = 1:4, selected = gameValues$quarter))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveChanges", "Save Changes")
      )
    ))
  })
  
  observeEvent(input$saveChanges, {
    gameValues$HOME_1 <- input$editHome1
    gameValues$HOME_2 <- input$editHome2
    gameValues$HOME_3 <- input$editHome3
    gameValues$HOME_4 <- input$editHome4
    gameValues$HOME_5 <- input$editHome5
    gameValues$AWAY_1 <- input$editAway1
    gameValues$AWAY_2 <- input$editAway2
    gameValues$AWAY_3 <- input$editAway3
    gameValues$AWAY_4 <- input$editAway4
    gameValues$AWAY_5 <- input$editAway5
    gameValues$quarter <- input$editQuarter
    
    # Close the modal
    removeModal()
  })
  
  ####################################### GAME REPORTS #################################################
  # Reactive function to fetch unique game data
  unique_games <- reactive({
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    query <- "SELECT DISTINCT game_date, opponent FROM w_basketball_game_chart_t"
    df <- dbGetQuery(con, query)
    dbDisconnect(con)
    df
  })
  
  # Drop down menu for games
  observe({
    game_data <- unique_games()  # Get the list of available games
    game_choices <- paste(game_data$game_date, "vs", game_data$opponent)  
    updateSelectInput(session, 'games', choices = game_choices)
  })
  
  # Render the game report table based on the selected game
  output$gameReportTable <- DT::renderDataTable({
    req(input$games)  # Ensure a game is selected
    
    # Extract selected date and opponent
    game_info <- strsplit(input$games, " vs ", fixed = TRUE)[[1]]
    date <- game_info[1]
    opponent <- game_info[2]
    
    # Connect to database
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    
    date <- dbQuoteLiteral(con, date) 
    opponent <- dbQuoteLiteral(con, opponent)
    
    
    # Query player data with proper team filtering
    query_players <- glue::glue("
      SELECT event_person AS Player, 
          SUM(CASE WHEN event = 'Shot' AND made_miss = 'Make' THEN 1 ELSE 0 END) AS FieldGoalsMade, 
            SUM(CASE WHEN event = 'Shot' AND made_miss = 'Miss' THEN 1 ELSE 0 END) AS FieldGoalsMissed,
            SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END) AS FieldGoalAttempts,
            ROUND(CASE WHEN SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END) > 0 
                  THEN (SUM(CASE WHEN event = 'Shot' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0) / 
                  NULLIF(SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END), 0) ELSE 0 END, 2) AS FieldGoalPercentage,
            SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) AS ThreePtMade,
            SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Miss' THEN 1 ELSE 0 END) AS ThreePtMissed,
            ROUND(CASE  WHEN SUM(CASE WHEN Three_Pt = 1 THEN 1 ELSE 0 END) > 0 
                  THEN (SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0) / 
                  NULLIF(SUM(CASE WHEN Three_Pt = 1 THEN 1 ELSE 0 END), 0) ELSE 0 END, 2) AS ThreePtPercentage,
            ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END)) > 0
                  THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribblePercentage,
            ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END)) > 0
                  THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffCatchPercentage,
            SUM(CASE WHEN event = 'Rebound' AND off_def = 'Offensive' THEN 1 ELSE 0 END) AS OffensiveRebounds,
            SUM(CASE WHEN event = 'Rebound' AND off_def = 'Defensive' THEN 1 ELSE 0 END) AS DefensiveRebounds,
            ROUND(CASE WHEN SUM(CASE WHEN Free_Throw = 1 THEN 1 END) > 0
                  THEN (SUM(CASE WHEN Free_Throw = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN Free_Throw = 1 THEN 1 END), 0)) ELSE 0 END, 2) AS FreeThrowPercentage
      FROM w_basketball_game_chart_t
      WHERE game_date = {date} AND opponent = {opponent} AND (event_team = 'Lindenwood' OR event_team IS NULL)
      GROUP BY event_person
")
    # Get player data
    df_players <- dbGetQuery(con, query_players)
    
    # Query for team data
    query_team <- glue::glue("
      SELECT 'Team Totals' AS Player, 
            SUM(CASE WHEN event = 'Shot' AND made_miss = 'Make' THEN 1 ELSE 0 END) AS FieldGoalsMade, 
            SUM(CASE WHEN event = 'Shot' AND made_miss = 'Miss' THEN 1 ELSE 0 END) AS FieldGoalsMissed,
            SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END) AS FieldGoalAttempts,
            ROUND(CASE WHEN SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END) > 0 
                  THEN (SUM(CASE WHEN event = 'Shot' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0) / 
                  NULLIF(SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END), 0) ELSE 0 END, 2) AS FieldGoalPercentage,
            SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) AS ThreePtMade,
            SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Miss' THEN 1 ELSE 0 END) AS ThreePtMissed,
            ROUND(CASE  WHEN SUM(CASE WHEN Three_Pt = 1 THEN 1 ELSE 0 END) > 0 
                  THEN (SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0) / 
                  NULLIF(SUM(CASE WHEN Three_Pt = 1 THEN 1 ELSE 0 END), 0) ELSE 0 END, 2) AS ThreePtPercentage,
            ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END)) > 0
                  THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribblePercentage,
            ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END)) > 0
                  THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffCatchPercentage,
            SUM(CASE WHEN event = 'Rebound' AND off_def = 'Offensive' THEN 1 ELSE 0 END) AS OffensiveRebounds,
            SUM(CASE WHEN event = 'Rebound' AND off_def = 'Defensive' THEN 1 ELSE 0 END) AS DefensiveRebounds,
            ROUND(CASE WHEN SUM(CASE WHEN Free_Throw = 1 THEN 1 END) > 0
                  THEN (SUM(CASE WHEN Free_Throw = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN Free_Throw = 1 THEN 1 END), 0)) ELSE 0 END, 2) AS FreeThrowPercentage
      FROM w_basketball_game_chart_t
      WHERE game_date = {date} AND opponent = {opponent} 
")
    
    # Get team data
    df_team_totals <- dbGetQuery(con, query_team)
    
    # Combine player and team data
    df_report <- rbind(df_players, df_team_totals)
    
    # Disconnect from database
    dbDisconnect(con)
    
     # Ensure df_report is a valid data frame
  if (nrow(df_report) == 0) {
      return(NULL)  # Avoid rendering an empty table
  }
  
  # Render DataTable and sort by player 
  DT::datatable(df_report, options = list(
    pageLength = -1,  # Show all rows
    dom = 't',  
    ordering = TRUE, 
    order = list(list(0, 'asc')),  # Sort by player ascending
    scrollX = TRUE  # Enable horizontal scrolling
  ),
  rownames = FALSE
  )
})
  # Render the opponent game report table
  output$opponentReportTable <- DT::renderDataTable({
    req(input$games)  # Ensure a game is selected
    
    # Extract selected date and opponent
    game_info <- strsplit(input$games, " vs ", fixed = TRUE)[[1]]
    date <- game_info[1]
    opponent <- game_info[2]
    
    # Connect to database
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    
    date <- dbQuoteLiteral(con, date)
    opponent <- dbQuoteLiteral(con, opponent)
    
    # Query opponent player data
    query_opponent_players <- glue::glue("
     SELECT event_person AS Player, 
          SUM(CASE WHEN event = 'Shot' AND made_miss = 'Make' THEN 1 ELSE 0 END) AS FieldGoalsMade, 
            SUM(CASE WHEN event = 'Shot' AND made_miss = 'Miss' THEN 1 ELSE 0 END) AS FieldGoalsMissed,
            SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END) AS FieldGoalAttempts,
            ROUND(CASE WHEN SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END) > 0 
                  THEN (SUM(CASE WHEN event = 'Shot' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0) / 
                  NULLIF(SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END), 0) ELSE 0 END, 2) AS FieldGoalPercentage,
            SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) AS ThreePtMade,
            SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Miss' THEN 1 ELSE 0 END) AS ThreePtMissed,
            ROUND(CASE  WHEN SUM(CASE WHEN Three_Pt = 1 THEN 1 ELSE 0 END) > 0 
                  THEN (SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0) / 
                  NULLIF(SUM(CASE WHEN Three_Pt = 1 THEN 1 ELSE 0 END), 0) ELSE 0 END, 2) AS ThreePtPercentage,
            ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END)) > 0
                  THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribblePercentage,
            ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END)) > 0
                  THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffCatchPercentage,
            SUM(CASE WHEN event = 'Rebound' AND off_def = 'Offensive' THEN 1 ELSE 0 END) AS OffensiveRebounds,
            SUM(CASE WHEN event = 'Rebound' AND off_def = 'Defensive' THEN 1 ELSE 0 END) AS DefensiveRebounds,
            ROUND(CASE WHEN SUM(CASE WHEN Free_Throw = 1 THEN 1 END) > 0
                  THEN (SUM(CASE WHEN Free_Throw = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN Free_Throw = 1 THEN 1 END), 0)) ELSE 0 END, 2) AS FreeThrowPercentage
      FROM w_basketball_game_chart_t
    WHERE game_date = {date} AND opponent = {opponent} AND event_team != 'Lindenwood'
    GROUP BY event_person
  ")
    
    # Get opponent player data
    df_opponent_players <- dbGetQuery(con, query_opponent_players)
    
    # Query opponent team totals
    query_opponent_team <- glue::glue("
     SELECT 'Team Totals' AS Player, 
            SUM(CASE WHEN event = 'Shot' AND made_miss = 'Make' THEN 1 ELSE 0 END) AS FieldGoalsMade, 
            SUM(CASE WHEN event = 'Shot' AND made_miss = 'Miss' THEN 1 ELSE 0 END) AS FieldGoalsMissed,
            SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END) AS FieldGoalAttempts,
            ROUND(CASE WHEN SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END) > 0 
                  THEN (SUM(CASE WHEN event = 'Shot' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0) / 
                  NULLIF(SUM(CASE WHEN Fg = 1 THEN 1 ELSE 0 END), 0) ELSE 0 END, 2) AS FieldGoalPercentage,
            SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) AS ThreePtMade,
            SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Miss' THEN 1 ELSE 0 END) AS ThreePtMissed,
            ROUND(CASE  WHEN SUM(CASE WHEN Three_Pt = 1 THEN 1 ELSE 0 END) > 0 
                  THEN (SUM(CASE WHEN Three_Pt = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0) / 
                  NULLIF(SUM(CASE WHEN Three_Pt = 1 THEN 1 ELSE 0 END), 0) ELSE 0 END, 2) AS ThreePtPercentage,
            ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END)) > 0
                  THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribblePercentage,
            ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END)) > 0
                  THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffCatchPercentage,
            SUM(CASE WHEN event = 'Rebound' AND off_def = 'Offensive' THEN 1 ELSE 0 END) AS OffensiveRebounds,
            SUM(CASE WHEN event = 'Rebound' AND off_def = 'Defensive' THEN 1 ELSE 0 END) AS DefensiveRebounds,
            ROUND(CASE WHEN SUM(CASE WHEN Free_Throw = 1 THEN 1 END) > 0
                  THEN (SUM(CASE WHEN Free_Throw = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
                  NULLIF(SUM(CASE WHEN Free_Throw = 1 THEN 1 END), 0)) ELSE 0 END, 2) AS FreeThrowPercentage
      FROM w_basketball_game_chart_t
    WHERE game_date = {date} AND opponent = {opponent} AND event_team != 'Lindenwood'
  ")
    
    # Get opponent team data
    df_opponent_team <- dbGetQuery(con, query_opponent_team)
    
    # Combine opponent player and team data
    df_opponent_report <- rbind(df_opponent_players, df_opponent_team)
    
    # Disconnect from database
    dbDisconnect(con)
    
    # Ensure df_opponent_report is a valid data frame
    if (nrow(df_opponent_report) == 0) {
      return(NULL)  # Avoid rendering an empty table
    }
    
    # Render DataTable for opponent report
    DT::datatable(df_opponent_report, options = list(
      pageLength = -1,  # Show all rows
      dom = 't',
      ordering = TRUE,
      order = list(list(0, 'asc')),  # Sort by player name
      scrollX = TRUE  # Enable horizontal scrolling
    ),
    rownames = FALSE
    )
  })
  
  ####################################### Player Shot Analysis ###############################################
  # Reactive function to fetch unique games from the database
  unique_games <- reactive({
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    
    query <- "SELECT DISTINCT game_date, opponent AS opponent_team FROM w_basketball_game_chart_t ORDER BY game_date"
    games_df <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    games_df$game_label <- paste(games_df$game_date, "-", games_df$opponent_team)
    
    return(games_df)
  })
  
  # Update game selection dropdown
  observe({
    game_data <- unique_games()
    
    print("Available Games:")
    print(game_data)
    
    game_choices <- if (nrow(game_data) > 0) c("All Games", game_data$game_label) else "All Games"
    
    updateSelectInput(session, "selected_game", choices = game_choices, selected = "All Games")
  })
  
  # Reactive function to fetch unique shot types
  unique_shot <- reactive({
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    
    query <- "SELECT DISTINCT dribble_catch FROM w_basketball_game_chart_t WHERE event = 'Shot'"
    shot_df <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    shot_choices <- if (nrow(shot_df) > 0) c("All Shots", shot_df$dribble_catch) else "All Shots"
    
    return(shot_choices)
  })
  
  # Update shot type selection dropdown
  observe({
    shot_choices <- unique_shot()
    
    print("Available Shot Types:")
    print(shot_choices)
    
    updateSelectInput(session, "shotType", choices = shot_choices, selected = "All Shots")
  })
  
  # Reactive function to retrieve shot data based on selected filters
  get_shot_data <- reactive({
    req(input$playerName, input$shotType, input$selected_game)
    
    shot_type <- input$shotType
    selected_game <- input$selected_game
    
    if (selected_game != "All Games") {
      selected_game_date <- strsplit(selected_game, " - ")[[1]][1]
      selected_game_date <- as.Date(selected_game_date, format = "%Y-%m-%d")
    } else {
      selected_game_date <- NULL
    }
    
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    
    query <- "
  SELECT event_person AS number, event, event_x, event_y, event_distance, made_miss, game_date, dribble_catch
  FROM w_basketball_game_chart_t
  WHERE event = 'Shot'
  "
    
    df_shots <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    df_shots$number <- as.numeric(df_shots$number)
    
    # Join with players dataset
    df_shots <- df_shots %>%
      inner_join(players, by = "number")
    
    # Apply player filter
    if (input$playerName != "All Players") {
      df_shots <- df_shots %>% filter(name == input$playerName)
    }
    
    # Apply shot type filter
    if (shot_type == "Dribble") {
      df_shots <- df_shots %>% filter(dribble_catch == "Dribble")
    } else if (shot_type == "Catch") {
      df_shots <- df_shots %>% filter(dribble_catch == "Catch")
    }
    
    # Filter by game date
    if (!is.null(selected_game_date)) {
      df_shots <- df_shots %>% filter(game_date == selected_game_date)
    }
    
    # Calculate shooting percentages for each player
    player_shooting_data <- df_shots %>%
      group_by(number) %>%
      summarize(
        field_goals_made = sum(made_miss == "Make"),
        field_goals_attempted = n(),
        field_goal_percentage = round((field_goals_made / field_goals_attempted) * 100, 2),
        
        # 3-point shooting percentage
        three_point_made = sum(made_miss == "Make" & event_distance > 22), # assuming 22 is 3-point range
        three_point_attempted = sum(event_distance > 22),
        three_point_percentage = ifelse(three_point_attempted > 0, round((three_point_made / three_point_attempted) * 100, 2), 0),
        
        # Dribble shooting percentage
        dribble_made = sum(made_miss == "Make" & dribble_catch == "Dribble"),
        dribble_attempted = sum(dribble_catch == "Dribble"),
        dribble_percentage = ifelse(dribble_attempted > 0, round((dribble_made / dribble_attempted) * 100, 2), 0),
        
        # Catch shooting percentage
        catch_made = sum(made_miss == "Make" & dribble_catch == "Catch"),
        catch_attempted = sum(dribble_catch == "Catch"),
        catch_percentage = ifelse(catch_attempted > 0, round((catch_made / catch_attempted) * 100, 2), 0)
      )
    
    # Join shooting percentages to the shots data
    df_shots <- df_shots %>%
      left_join(player_shooting_data, by = "number") %>%
      mutate(shooting_percentage_label = paste(
        name, 
        "FG%:", field_goal_percentage, "%", 
        "| 3pt%:", three_point_percentage, "%", 
        "| Shot off Dribble%:", dribble_percentage, "%", 
        "| Shot off Catch%:", catch_percentage, "%"
      ))
    
    return(df_shots)
  })
  
  # Render the shot analysis court plot
  output$playerCourt <- renderPlot({
    # Get shot data for selected player
    df_shots <- get_shot_data()
    
    # If no shots data, return an empty plot
    if (nrow(df_shots) == 0) {
      return(plot(1, ylim = c(-30, 30), xlim = c(-50, 50), type = 'n', yaxs = 'i', xaxs = 'i', ylab = '', xlab = '', axes = F, asp = 1))
    }
    
    # Fix made_miss to ensure consistent naming
    df_shots$made_miss <- factor(df_shots$made_miss, levels = c("Make", "Miss"))
    
    # Draw the basketball court using base R plotting
    par(mai = c(0, 0, 0, 0))
    plot(1, ylim = c(-30, 30), xlim = c(-50, 50), type = 'n', yaxs = 'i', xaxs = 'i', ylab = '', xlab = '', axes = F, asp = 1)
    
    # Out of bounds
    rect(xleft = -47, ybottom = -25, xright = 47, ytop = 25, border = "black", lwd = 2)
    
    # Half-court line
    segments(0, -25, 0, 25, col = "black", lwd = 2)
    
    # Center circle
    symbols(x = 0, y = 0, circles = 6, inches = FALSE, add = TRUE, fg = "black", lwd = 2)
    
    # Backboard
    segments(-43, 3, -43, -3, col = "black", lwd = 2)
    segments(43, 3, 43, -3, col = "black", lwd = 2)
    
    # Paint area
    rect(-47, -7.5, -28, 7.5, border = "black", lwd = 2)
    rect(28, -7.5, 47, 7.5, border = "black", lwd = 2)
    
    # Free throw circles
    curve(sqrt(6^2 - (x + 28)^2), from = -28, to = -22, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(6^2 - (x + 28)^2), from = -28, to = -22, add = TRUE, col = "black", lwd = 2)
    curve(sqrt(6^2 - (x - 28)^2), from = 22, to = 28, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(6^2 - (x - 28)^2), from = 22, to = 28, add = TRUE, col = "black", lwd = 2)
    
    # Hoops
    symbols(x = -41.75, y = 0, circles = 1.5/2, inches = FALSE, add = TRUE, fg = "black", lwd = 2)  # Left hoop
    symbols(x = 41.75, y = 0, circles = 1.5/2, inches = FALSE, add = TRUE, fg = "black", lwd = 2)   # Right hoop
    
    # 3-point lines
    segments(-47, -21.65625, -37, -21.65625, col = "black", lwd = 2)
    segments(-47, 21.65625, -37, 21.65625, col = "black", lwd = 2)
    
    segments(47, -21.65625, 37, -21.65625, col = "black", lwd = 2)
    segments(47, 21.65625, 37, 21.65625, col = "black", lwd = 2)
    
    # 3-point arcs (fix NaNs by limiting the x range)
    curve(sqrt(22.14583^2 - (x + 41.75)^2), from = -37, to = -19.60417, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(22.14583^2 - (x + 41.75)^2), from = -37, to = -19.60417, add = TRUE, col = "black", lwd = 2)
    
    curve(sqrt(22.14583^2 - (x - 41.75)^2), from = 37, to = 19.60417, add = TRUE, col = "black", lwd = 2)
    curve(-sqrt(22.14583^2 - (x - 41.75)^2), from = 37, to = 19.60417, add = TRUE, col = "black", lwd = 2)
    
    # Add the shot locations as points on the court
    points(df_shots$event_x, df_shots$event_y, col = ifelse(df_shots$made_miss == "Make", "green", "red"), pch = 19, cex = 1.5)
    
    # Add legend for shot outcomes
    legend("topright", legend = c("Make", "Miss"), 
           fill = c("green", "red"), title = "Shot Outcome", bty = "n", cex = 1.0)
    
    # Only add shooting percentages to the legend if a single player is selected
    if (input$playerName != "All Players") {
      legend("topleft", legend = df_shots$shooting_percentage_label[1], 
             fill = "gold", title = "Player Shooting Percentages", bty = "n", cex = 0.9)
    }
  }, bg = "transparent")
}


# Shiny app
shinyApp(ui, server)

