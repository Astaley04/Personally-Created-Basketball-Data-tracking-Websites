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
  name = c("Mekhi Cooper", "Dontrez Williams", "Tidiane d'Almeida", "Jaylon McDaniel", 
           "Milos Nenadic", "Anias Futrell", "Jadis Jones", "Jalen Bouknight",
            "William Campbell", "Daniel Kwon", "Kevin Anthony", "Robert Lewis", 
            "Jeremiah Talton", "Todd Bieg", "Clayton Jackson"),
  number = c(  0, 1, 3, 5,
               7, 8, 10, 11,
               12, 15, 21, 22,
               24, 25, 34),
  position = c("G", "G", "G", "F",
               "C", "G", "G", "G",
               "G", "G", "C", "F",
               "F", "F", "G")
  
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
            menuItem("Lindenwood Shot Analysis", tabName = "LindenwoodShotAnalysis", icon = icon("bullseye")),
            menuItem("Lindenwood Rebound Analysis", tabName = "LindenwoodReboundAnalysis", icon = icon("basketball"))
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
            tabItem(tabName = "LindenwoodShotAnalysis",
                    fluidPage(
                      titlePanel("Lindenwood Shot Analysis"),
                      selectInput("selected_game", "Select Game", choices = NULL),
                      
                      fluidRow(
                        column(6, selectInput("playerName", "Select Player Name", 
                                              choices = c("All Players", players$name))),
                        column(6, selectInput("shotType", "Shot Type:", choices = c("All Shots", "Dribble", "Catch")),
                               uiOutput("dribbleDirectionUI")  # 🟩 NEW: appears only when "Dribble" is selected
                      )),
                      
                      plotOutput("playerCourt"),  # Basketball court plot
                      DTOutput("shootingStatsTable")  # Shooting statistics table
                    )
            ),
            tabItem(
              tabName = "LindenwoodReboundAnalysis",
              fluidPage(
                titlePanel("Lindenwood Rebound Analysis"),
                
                # Game Selection Input (dynamically populated from the server)
                uiOutput("selected_game_ui"),  # Dynamic game selection
                
                # Select Rebound Type Input
                selectInput("rebound_type", "Select Rebound Type", 
                            choices = c("All Rebounds", "Defensive Rebounds", "Offensive Rebounds")),
                
                # Player Selection Input (dynamically populated based on selected game)
                fluidRow(
                  column(6, 
                         selectInput("playerName", "Select Player Name", 
                                     choices = c("All Players", players$name))  # Dynamically populated
                  )
                ),
                
                # Rebound Court Plot
                plotOutput("reboundCourt"),
                
                # Rebounding Stats Table
                DTOutput("reboundingStatsTable")
              )
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
    password = "2239"
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
    half = 0,
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
    gameValues$half <- 1  # Starting with first half
    
    
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
  # ==== DRIBBLE DIRECTION TRACKER ====
  dribbleDirection <- reactiveVal(NULL)
  
  observeEvent(input$dribbleLeft, {
    dribbleDirection("left")
    showNotification("Dribble direction set to LEFT", type = "message")
  })
  
  observeEvent(input$dribbleRight, {
    dribbleDirection("right")
    showNotification("Dribble direction set to RIGHT", type = "message")
  })
  
  output$charting <- renderUI({
    
    if (GCState() == "charting") {
      tagList(
        radioButtons("eventType", "Shot Type or Rebound or Free Throw", choices = c("Shot", "Shot After Rebound", "Rebound", "Free Throw"), inline = TRUE),
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
                                  radioButtons("dbocShot", "Shot Off the Catch or Dribble", 
                                               choices = c("Catch", "Dribble"), inline = TRUE)
                           ),
                           column(4, 
                                  radioButtons("shotOutcomeShot", "Make or Miss Shot", 
                                               choices = c("Make", "Miss"), inline = TRUE)
                           )
                         ),
                         
                         # --- appears only when "Dribble" is selected ---
                         conditionalPanel(
                           condition = "input.dbocShot == 'Dribble'",
                           fluidRow(
                             column(6, 
                                    actionButton("dribbleLeft", "Dribble Left", icon = icon("arrow-left"),
                                                 style = "color: white; background-color: #444; border-color: #B5A36A;")
                             ),
                             column(6, 
                                    actionButton("dribbleRight", "Dribble Right", icon = icon("arrow-right"),
                                                 style = "color: white; background-color: #444; border-color: #B5A36A;")
                             )
                           )
                         )
        ),
        # --- NEW block for Shot After Offensive Rebound ---
        conditionalPanel("input.eventType == 'Shot After Rebound'", 
                         fluidRow(
                           column(4, 
                                  radioButtons("dbocShot", "Shot Off the Catch or Dribble", 
                                               choices = c("Catch", "Dribble"), inline = TRUE)
                           ),
                           column(4, 
                                  radioButtons("shotOutcomeShot", "Make or Miss Shot", 
                                               choices = c("Make", "Miss"), inline = TRUE)
                           )
                         ),
                         # --- appears only when "Dribble" is selected ---
                         conditionalPanel(
                           condition = "input.dbocShot == 'Dribble'",
                           fluidRow(
                             column(6, 
                                    actionButton("dribbleLeft", "Dribble Left", icon = icon("arrow-left"),
                                                 style = "color: white; background-color: #444; border-color: #B5A36A;")
                             ),
                             column(6, 
                                    actionButton("dribbleRight", "Dribble Right", icon = icon("arrow-right"),
                                                 style = "color: white; background-color: #444; border-color: #B5A36A;")
                             )
                           )
                         )
        ),
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
    # === VALIDATION SECTION ===
    
    # Check event type
    if (is.null(input$eventType)) {
      showNotification("Error: Please select a Shot Type (Shot, Shot After Rebound, Rebound, or Free Throw).", type = "error", duration = 4)
      return()
    }
    
    # Check player selection
    if (is.null(input$eventPerson)) {
      showNotification("Error: Please select the player (Shooter/Rebounder).", type = "error", duration = 4)
      return()
    }
    
    # Shot validation
    if (input$eventType == "Shot") {
      if (is.null(input$dbocShot)) {
        showNotification("Error: Please select whether the shot was off the Catch or Dribble.", type = "error", duration = 4)
        return()
      }
      if (is.null(input$shotOutcomeShot)) {
        showNotification("Error: Please select whether the shot was a Make or Miss.", type = "error", duration = 4)
        return()
      }
      if (input$dbocShot == "Dribble" && is.null(dribbleDirection())) {
        showNotification("Error: Please select a dribble direction (Left or Right).", type = "error", duration = 4)
        return()
      }
    }
    
    # Shot After Rebound validation
    if (input$eventType == "Shot After Rebound") {
      if (is.null(input$dbocShot)) {
        showNotification("Error: Please select whether the shot was off the Catch or Dribble.", type = "error", duration = 4)
        return()
      }
      if (is.null(input$shotOutcomeShot)) {
        showNotification("Error: Please select whether the shot was a Make or Miss.", type = "error", duration = 4)
        return()
      }
      if (input$dbocShot == "Dribble" && is.null(dribbleDirection())) {
        showNotification("Error: Please select a dribble direction (Left or Right).", type = "error", duration = 4)
        return()
      }
    }
    
    # Rebound validation
    if (input$eventType == "Rebound" && is.null(input$offOrDefRebound)) {
      showNotification("Error: Please select Offensive or Defensive rebound.", type = "error", duration = 4)
      return()
    }
    
    # Free Throw validation
    if (input$eventType == "Free Throw" && is.null(input$shotOutcomeFT)) {
      showNotification("Error: Please select Make or Miss for Free Throw.", type = "error", duration = 4)
      return()
    }
    
    # Court click check
    if (nrow(GCCourtPoint$temp_court) == 0) {
      showNotification("Error: Please click a location on the court before submitting.", type = "error", duration = 4)
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
        
        shotTypeValue <- ifelse(shotCategory == "Three Point", 1, 0)  # 1 for 3-point shot
        fgTypeValue <- ifelse(shotCategory == "Free Throw", 0, 1)     # FG flag
        ftValue <- ifelse(shotCategory == "Free Throw", 1, 0)         # FT flag
        
        # Determine dribble_direction value for SQL:
        # if shot was 'Dribble' use the reactive dribbleDirection(), otherwise NULL
        dribble_val_sql <- "NULL"
        if (!is.null(shotType) && shotType == "Dribble") {
          val <- dribbleDirection()  # will be "left" or "right" or NULL if not clicked
          if (!is.null(val)) {
            # wrap in single quotes so the SQL string is 'left' or 'right'
            dribble_val_sql <- glue::glue("'{val}'")
          } else {
            # no button clicked — store NULL (or change to "'unknown'" if you prefer)
            dribble_val_sql <- "NULL"
          }
        }
        
        # Debug prints (optional)
        print(playerID); print(eventX); print(eventY); print(distance)
        print(shotResult); print(shotType); print(dribble_val_sql)
        
        # Insert shot data into database including dribble_direction
        insert_query <- glue::glue(
          "
    INSERT INTO m_basketball_game_chart_t(
      event, event_person, event_x, event_y, event_distance, event_zone, made_miss,
      free_throw, three_pt, dribble_catch, dribble_direction, fg,
      h1, h2, h3, h4, h5, a1, a2, a3, a4, a5,
      game_date, opponent, home_away, half, time_of_day, event_team)
    VALUES(
      'Shot', '{playerID}', {eventX}, {eventY}, {distance}, '{shot_zone}', '{shotResult}',
      '{ftValue}', {shotTypeValue}, '{shotType}', {dribble_val_sql}, {fgTypeValue},
      {gameValues$HOME_1}, {gameValues$HOME_2}, {gameValues$HOME_3}, {gameValues$HOME_4}, {gameValues$HOME_5},
      '{gameValues$AWAY_1}', '{gameValues$AWAY_2}', '{gameValues$AWAY_3}', '{gameValues$AWAY_4}', '{gameValues$AWAY_5}',
      '{gameValues$date}', '{gameValues$opponent}', '{gameValues$home_away}', {gameValues$half}, '{Time}', '{event_team}'
    )
    "
        )
        
        print(insert_query)
        dbExecute(con, insert_query)
        dbDisconnect(con)
        print("Done")
        
        # Reset the dribbleDirection after a successful submit so subsequent shots start fresh
        dribbleDirection(NULL)
      }
      # SHOT AFTER REBOUND -----
      if (event == "Shot After Rebound") {
        shotResult <- input$shotOutcomeShot
        shotType <- input$dbocShot
        
        # Classify shot zones
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
        
        # Determine 3pt vs FG
        if (distance > 22 && eventX != 0) {
          shotCategory <- "Three Point"
        } else if (eventY == 0) {
          shotCategory <- "Free Throw"
        } else {
          shotCategory <- "Field Goal"
        }
        
        shotTypeValue <- ifelse(shotCategory == "Three Point", 1, 0)
        fgTypeValue <- ifelse(shotCategory == "Free Throw", 0, 1)
        ftValue <- ifelse(shotCategory == "Free Throw", 1, 0)
        
        # Dribble direction logic
        dribble_val_sql <- "NULL"
        if (!is.null(shotType) && shotType == "Dribble") {
          val <- dribbleDirection()
          if (!is.null(val)) {
            dribble_val_sql <- glue::glue("'{val}'")
          }
        }
        
        # Insert shot after rebound into DB
        insert_query <- glue::glue("
    INSERT INTO m_basketball_game_chart_t(
      event, event_person, event_x, event_y, event_distance, event_zone, made_miss,
      free_throw, three_pt, dribble_catch, dribble_direction, fg,
      h1, h2, h3, h4, h5, a1, a2, a3, a4, a5,
      game_date, opponent, home_away, half, time_of_day, event_team)
    VALUES(
      'Shot After Rebound', '{playerID}', {eventX}, {eventY}, {distance}, '{shot_zone}', '{shotResult}',
      '{ftValue}', {shotTypeValue}, '{shotType}', {dribble_val_sql}, {fgTypeValue},
      {gameValues$HOME_1}, {gameValues$HOME_2}, {gameValues$HOME_3}, {gameValues$HOME_4}, {gameValues$HOME_5},
      '{gameValues$AWAY_1}', '{gameValues$AWAY_2}', '{gameValues$AWAY_3}', '{gameValues$AWAY_4}', '{gameValues$AWAY_5}',
      '{gameValues$date}', '{gameValues$opponent}', '{gameValues$home_away}', {gameValues$half}, '{Time}', '{event_team}'
    )
  ")
        
        print(insert_query)
        dbExecute(con, insert_query)
        dbDisconnect(con)
        print("Shot After Rebound saved.")
        
        # Reset dribble direction
        dribbleDirection(NULL)
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
        print(gameValues$half)
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
    INSERT INTO m_basketball_game_chart_t(
      event, event_person, event_x, event_y, event_distance, event_zone, off_def, shot_recovery_zone, 
      h1, h2, h3, h4, h5, a1, a2, a3, a4, a5, 
      game_date, opponent, home_away, half, time_of_day, event_team)
    
    VALUES(
      'Rebound', '{playerID}', {eventX}, {eventY}, {distance}, '{rebound_zone}', '{reboundResult}', '{recovery_zone}',
      {gameValues$HOME_1}, {gameValues$HOME_2}, {gameValues$HOME_3}, {gameValues$HOME_4}, {gameValues$HOME_5}, '{gameValues$AWAY_1}', '{gameValues$AWAY_2}', '{gameValues$AWAY_3}', '{gameValues$AWAY_4}', '{gameValues$AWAY_5}',
      '{gameValues$date}', '{gameValues$opponent}', '{gameValues$home_away}', {gameValues$half}, '{Time}', '{event_team}'
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
    INSERT INTO m_basketball_game_chart_t(
      event, event_person, event_x, event_y, event_distance, made_miss, free_throw, three_pt, fg, 
      h1, h2, h3, h4, h5, a1, a2, a3, a4, a5, 
      game_date, opponent, home_away, half, time_of_day, event_team)
    
    VALUES(
      'Free Throw', '{playerID}', {eventX}, {eventY}, {distance}, '{ftResult}', '{ftValue}', {shotTypeValue}, {fgTypeValue},
      {gameValues$HOME_1}, {gameValues$HOME_2}, {gameValues$HOME_3}, {gameValues$HOME_4}, {gameValues$HOME_5}, '{gameValues$AWAY_1}', '{gameValues$AWAY_2}', '{gameValues$AWAY_3}', '{gameValues$AWAY_4}', '{gameValues$AWAY_5}',
      '{gameValues$date}', '{gameValues$opponent}', '{gameValues$home_away}', {gameValues$half}, '{Time}', '{event_team}'
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
               selectInput("editHalf", "Half", choices = 1:2, selected = gameValues$half))
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
    gameValues$half <- input$editHalf
    
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
    query <- "SELECT DISTINCT game_date, opponent FROM m_basketball_game_chart_t"
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

        -- 🔹 Shots off dribble percentages
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribblePercentage,

        -- 🔹 Shots off dribble right
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribbleRightPercentage,

        -- 🔹 Shots off dribble left
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribbleLeftPercentage,

        -- 🔹 Shots off catch
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffCatchPercentage,

        -- 🔹 NEW: Shots after rebound (count + percentage)
        SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END) AS ShotsAfterReboundAttempts,
        ROUND(CASE WHEN SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END) > 0
              THEN (SUM(CASE WHEN event = 'Shot after rebound' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsAfterReboundPercentage,

        -- 🔹 Rebounds and FTs
        SUM(CASE WHEN event = 'Rebound' AND off_def = 'Offensive' THEN 1 ELSE 0 END) AS OffensiveRebounds,
        SUM(CASE WHEN event = 'Rebound' AND off_def = 'Defensive' THEN 1 ELSE 0 END) AS DefensiveRebounds,
        ROUND(CASE WHEN SUM(CASE WHEN Free_Throw = 1 THEN 1 END) > 0
              THEN (SUM(CASE WHEN Free_Throw = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN Free_Throw = 1 THEN 1 END), 0)) ELSE 0 END, 2) AS FreeThrowPercentage
  FROM m_basketball_game_chart_t
  WHERE game_date = {date} AND opponent = {opponent} 
    AND (event_team = 'Lindenwood' OR event_team IS NULL)
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

        -- 🔹 Shots off dribble percentages
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribblePercentage,

        -- 🔹 Shots off dribble right
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribbleRightPercentage,

        -- 🔹 Shots off dribble left
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribbleLeftPercentage,

        -- 🔹 Shots off catch
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffCatchPercentage,

        -- 🔹 NEW: Shots after rebound (count + percentage)
        SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END) AS ShotsAfterReboundAttempts,
        ROUND(CASE WHEN SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END) > 0
              THEN (SUM(CASE WHEN event = 'Shot after rebound' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsAfterReboundPercentage,

        -- 🔹 Rebounds and FTs
        SUM(CASE WHEN event = 'Rebound' AND off_def = 'Offensive' THEN 1 ELSE 0 END) AS OffensiveRebounds,
        SUM(CASE WHEN event = 'Rebound' AND off_def = 'Defensive' THEN 1 ELSE 0 END) AS DefensiveRebounds,
        ROUND(CASE WHEN SUM(CASE WHEN Free_Throw = 1 THEN 1 END) > 0
              THEN (SUM(CASE WHEN Free_Throw = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN Free_Throw = 1 THEN 1 END), 0)) ELSE 0 END, 2) AS FreeThrowPercentage
  FROM m_basketball_game_chart_t
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

        -- 🔹 Shots off dribble percentages
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribblePercentage,

        -- 🔹 Shots off dribble right
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribbleRightPercentage,

        -- 🔹 Shots off dribble left
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribbleLeftPercentage,

        -- 🔹 Shots off catch
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffCatchPercentage,

        -- 🔹 NEW: Shots after rebound (count + percentage)
        SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END) AS ShotsAfterReboundAttempts,
        ROUND(CASE WHEN SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END) > 0
              THEN (SUM(CASE WHEN event = 'Shot after rebound' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsAfterReboundPercentage,

        -- 🔹 Rebounds and FTs
        SUM(CASE WHEN event = 'Rebound' AND off_def = 'Offensive' THEN 1 ELSE 0 END) AS OffensiveRebounds,
        SUM(CASE WHEN event = 'Rebound' AND off_def = 'Defensive' THEN 1 ELSE 0 END) AS DefensiveRebounds,
        ROUND(CASE WHEN SUM(CASE WHEN Free_Throw = 1 THEN 1 END) > 0
              THEN (SUM(CASE WHEN Free_Throw = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN Free_Throw = 1 THEN 1 END), 0)) ELSE 0 END, 2) AS FreeThrowPercentage
  FROM m_basketball_game_chart_t
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

        -- 🔹 Shots off dribble percentages
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribblePercentage,

        -- 🔹 Shots off dribble right
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Right' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribbleRightPercentage,

        -- 🔹 Shots off dribble left
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Dribble' AND dribble_direction = 'Left' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffDribbleLeftPercentage,

        -- 🔹 Shots off catch
        ROUND(CASE WHEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END)) > 0
              THEN (SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot' AND dribble_catch = 'Catch' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsOffCatchPercentage,

        -- 🔹 NEW: Shots after rebound (count + percentage)
        SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END) AS ShotsAfterReboundAttempts,
        ROUND(CASE WHEN SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END) > 0
              THEN (SUM(CASE WHEN event = 'Shot after rebound' AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN event = 'Shot after rebound' THEN 1 ELSE 0 END), 0)) ELSE 0 END, 2) AS ShotsAfterReboundPercentage,

        -- 🔹 Rebounds and FTs
        SUM(CASE WHEN event = 'Rebound' AND off_def = 'Offensive' THEN 1 ELSE 0 END) AS OffensiveRebounds,
        SUM(CASE WHEN event = 'Rebound' AND off_def = 'Defensive' THEN 1 ELSE 0 END) AS DefensiveRebounds,
        ROUND(CASE WHEN SUM(CASE WHEN Free_Throw = 1 THEN 1 END) > 0
              THEN (SUM(CASE WHEN Free_Throw = 1 AND made_miss = 'Make' THEN 1 ELSE 0 END) * 100.0 /
              NULLIF(SUM(CASE WHEN Free_Throw = 1 THEN 1 END), 0)) ELSE 0 END, 2) AS FreeThrowPercentage
  FROM m_basketball_game_chart_t
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
  ####################################### LINDENWOOD SHOT ANALYSIS ###############################################
  # Reactive function to fetch unique games from the database
  unique_games <- reactive({
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    
    query <- "SELECT DISTINCT game_date, opponent AS opponent_team FROM m_basketball_game_chart_t ORDER BY game_date"
    games_df <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    games_df$game_label <- paste(games_df$game_date, "-", games_df$opponent_team)
    return(games_df)
  })
  
  # Update game selection dropdown
  observe({
    game_data <- unique_games()
    game_choices <- if (nrow(game_data) > 0) c("All Games", game_data$game_label) else "All Games"
    updateSelectInput(session, "selected_game", choices = game_choices, selected = "All Games")
  })
  
  # Reactive function to fetch unique shot types (without adding extras to dropdown)
  unique_shot <- reactive({
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    
    query <- "SELECT DISTINCT dribble_catch FROM m_basketball_game_chart_t WHERE event = 'Shot' and event = 'Shot After Rebound'"
    shot_df <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    if (nrow(shot_df) > 0) {
      c("All Shots", unique(shot_df$dribble_catch))
    } else {
      c("All Shots")
    }
  })
  
  # Update shot type dropdown
  observe({
    shot_choices <- unique_shot()
    updateSelectInput(session, "shotType", choices = shot_choices, selected = "All Shots")
  })
  
  # Show dribble direction dropdown when Dribble selected
  observe({
    if (input$shotType == "Dribble") {
      output$dribbleDirectionUI <- renderUI({
        selectInput(
          "dribbleDirection",
          "Dribble Direction:",
          choices = c("All", "Off Right", "Off Left"),
          selected = "All"
        )
      })
    } else {
      output$dribbleDirectionUI <- renderUI(NULL)
    }
  })
  
  # === SHOT DATA ===
  get_shot_data <- reactive({
    req(input$playerName, input$shotType, input$selected_game)
    
    shot_type <- input$shotType
    selected_game <- input$selected_game
    selected_game_date <- if (selected_game != "All Games") as.Date(strsplit(selected_game, " - ")[[1]][1]) else NULL
    
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    
    query <- "
    SELECT event_person AS number, event, event_x, event_y, event_distance, made_miss,
           game_date, dribble_catch, dribble_direction
    FROM m_basketball_game_chart_t
    WHERE event IN ('Shot', 'Shot After Rebound')
  "
    
    df_shots <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    df_shots$number <- as.numeric(df_shots$number)
    df_shots <- df_shots %>% inner_join(players, by = "number")
    
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
    
    # Apply dribble direction
    if (shot_type == "Dribble" && !is.null(input$dribbleDirection)) {
      if (input$dribbleDirection == "Off Right") {
        df_shots <- df_shots %>% filter(dribble_direction == "right")
      } else if (input$dribbleDirection == "Off Left") {
        df_shots <- df_shots %>% filter(dribble_direction == "left")
      }
    }
    
    # Filter by game date
    if (!is.null(selected_game_date)) {
      df_shots <- df_shots %>% filter(game_date == selected_game_date)
    }
    
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
  }, bg = "transparent")
  
  # Table to shooting percentages
  output$shootingStatsTable <- renderDT({
    df_shots <- get_shot_data()
    if (nrow(df_shots) == 0) return(NULL)
    
    
    # === Individual Player Stats ===
    shooting_stats <- df_shots %>%
      group_by(number, name) %>%
      summarise(
        # 1. FG%
        field_goal_percentage = round(sum(made_miss == "Make") / n() * 100, 2),
        
        # 2. Shots Off Catch%
        catch_percentage = ifelse(
          sum(dribble_catch == "Catch") > 0,
          round(sum(made_miss == "Make" & dribble_catch == "Catch") /
                  sum(dribble_catch == "Catch") * 100, 2),
          NA
        ),
        
        # 3. Shots Off Dribble%
        dribble_percentage = ifelse(
          sum(dribble_catch == "Dribble") > 0,
          round(sum(made_miss == "Make" & dribble_catch == "Dribble") /
                  sum(dribble_catch == "Dribble") * 100, 2),
          NA
        ),
        
        # 4. Shots Off Dribble Right%
        off_dribble_right_percentage = ifelse(
          sum(dribble_catch == "Dribble" & dribble_direction == "right") > 0,
          round(sum(made_miss == "Make" & dribble_catch == "Dribble" &
                      dribble_direction == "right") /
                  sum(dribble_catch == "Dribble" & dribble_direction == "right") * 100, 2),
          NA
        ),
        
        # 5. Shots Off Dribble Left%
        off_dribble_left_percentage = ifelse(
          sum(dribble_catch == "Dribble" & dribble_direction == "left") > 0,
          round(sum(made_miss == "Make" & dribble_catch == "Dribble" &
                      dribble_direction == "left") /
                  sum(dribble_catch == "Dribble" & dribble_direction == "left") * 100, 2),
          NA
        ),
        
        # 6. Shots After Rebound%
        rebound_percentage = ifelse(
          sum(event == "Shot After Rebound") > 0,
          round(sum(made_miss == "make" & event == "Shot After Rebound") /
                  sum(event == "Shot After Rebound") * 100, 2),
          NA
        ),
        .groups = "drop"
      ) %>%
      arrange(number)
    
    # === Lindenwood Total Row ===
    team_total <- df_shots %>%
      summarise(
        number = NA_real_,
        name = "Lindenwood Total",
        field_goal_percentage = round(sum(made_miss == "Make", na.rm = TRUE) / n() * 100, 2),
        catch_percentage = ifelse(
          sum(dribble_catch == "Catch", na.rm = TRUE) > 0,
          round(sum(made_miss == "Make" & dribble_catch == "Catch", na.rm = TRUE) /
                  sum(dribble_catch == "Catch", na.rm = TRUE) * 100, 2),
          NA
        ),
        dribble_percentage = ifelse(
          sum(dribble_catch == "Dribble", na.rm = TRUE) > 0,
          round(sum(made_miss == "Make" & dribble_catch == "Dribble", na.rm = TRUE) /
                  sum(dribble_catch == "Dribble", na.rm = TRUE) * 100, 2),
          NA
        ),
        off_dribble_right_percentage = ifelse(
          sum(dribble_catch == "Dribble" & dribble_direction == "right", na.rm = TRUE) > 0,
          round(sum(made_miss == "Make" & dribble_catch == "Dribble" &
                      dribble_direction == "right", na.rm = TRUE) /
                  sum(dribble_catch == "Dribble" & dribble_direction == "right", na.rm = TRUE) * 100, 2),
          NA
        ),
        off_dribble_left_percentage = ifelse(
          sum(dribble_catch == "Dribble" & dribble_direction == "left", na.rm = TRUE) > 0,
          round(sum(made_miss == "Make" & dribble_catch == "Dribble" &
                      dribble_direction == "left", na.rm = TRUE) /
                  sum(dribble_catch == "Dribble" & dribble_direction == "left", na.rm = TRUE) * 100, 2),
          NA
        ),
        rebound_percentage = ifelse(
          sum(event == "Shot After Rebound", na.rm = TRUE) > 0,
          round(sum(made_miss == "Make" & event == "Shot After Rebound", na.rm = TRUE) /
                  sum(event == "Shot After Rebound", na.rm = TRUE) * 100, 2),
          NA
        )
      )
    
    # Combine player + team total
    shooting_stats <- bind_rows(shooting_stats, team_total)
    
    datatable(
      shooting_stats,
      options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE),
      rownames = FALSE,
      colnames = c(
        "Number", "Player",
        "FG%", "Shot Off Catch%", "Shot Off Dribble%",
        "Shot Off Dribble Right%", "Shot Off Dribble Left%", "Shot After Rebound%"
      )
    )
  })
  ####################################### LINDENWOOD REBOUND ANALYSIS ################################################  
  # Reactive function to fetch unique games from the database
  unique_games <- reactive({
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    
    query <- "SELECT DISTINCT game_date, opponent AS opponent_team FROM m_basketball_game_chart_t ORDER BY game_date"
    games_df <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    games_df$game_label <- paste(games_df$game_date, "-", games_df$opponent_team)
    
    return(games_df)
  })
  
  # Render the game selection input dynamically
  output$selected_game_ui <- renderUI({
    games_df <- unique_games()
    
    if (nrow(games_df) > 0) {
      game_choices <- games_df$game_label
      selectInput("selected_game", "Select Game", choices = c("All Games", game_choices))
    } else {
      selectInput("selected_game", "Select Game", choices = "No Games Available")
    }
  })
  
  # Render the player selection input dynamically based on selected game
  output$selected_player_ui <- renderUI({
    selected_game <- input$selected_game
    df_rebounds <- get_rebound_data()
    
    if (selected_game != "All Games") {
      players <- unique(df_rebounds$number)  # Get unique players for selected game
    } else {
      players <- unique(df_rebounds$number)  # Get all players
    }
    
    player_choices <- c("All Players", as.character(players))
    
    selectInput("selected_player", "Select Player", choices = player_choices)
  })
  
  # Reactive function to fetch rebound data and include player names and numbers
  get_rebound_data <- reactive({
    req(input$selected_game, input$rebound_type, input$playerName)  # Ensure playerName is included as a required input
    
    selected_game <- input$selected_game
    selected_player_name <- input$playerName  # Get the selected player's name
    
    # Get the player number based on the selected player's name
    if (selected_player_name != "All Players") {
      selected_player_number <- players %>%
        filter(name == selected_player_name) %>%
        pull(number)  # Get the player number
    }
    
    # Get the game date
    selected_game_date <- NULL
    if (selected_game != "All Games") {
      selected_game_date <- strsplit(selected_game, " - ")[[1]][1]
      selected_game_date <- as.Date(selected_game_date, format = "%Y-%m-%d")
    }
    
    con <- dbConnect(RPostgres::Postgres(),
                     dbname = "ps1",
                     user = "pythoncon",
                     password = "password",
                     host = "18.217.248.114",
                     port = "5432")
    
    # Modify query to filter by 'Lindenwood' team or NULL event_team
    query <- "SELECT event_person AS number, event, event_x, event_y, off_def, game_date, event_team
            FROM m_basketball_game_chart_t 
            WHERE event = 'Rebound' AND (event_team = 'Lindenwood' OR event_team IS NULL)"
    
    df_rebounds <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    # Check for NA values in game_date column and filter out
    df_rebounds <- df_rebounds %>%
      filter(!is.na(game_date))
    
    # Filter by selected game date if specified
    if (!is.null(selected_game_date)) {
      df_rebounds <- df_rebounds %>% filter(game_date == selected_game_date)
    }
    
    # Filter by rebound type
    if (input$rebound_type == "Defensive Rebounds") {
      df_rebounds <- df_rebounds %>% filter(off_def == "Defensive")
    } else if (input$rebound_type == "Offensive Rebounds") {
      df_rebounds <- df_rebounds %>% filter(off_def == "Offensive")
    }
    
    # If "All Players" is selected, do not filter by player number
    if (selected_player_name != "All Players") {
      df_rebounds <- df_rebounds %>% filter(number == selected_player_number)
    }
    
    # Convert number columns to character for both dataframes to avoid type mismatch
    df_rebounds$number <- as.character(df_rebounds$number)
    players$number <- as.character(players$number)  # Assuming `players` is a dataframe with 'number' and 'name' columns
    
    # Join with the players table to get player names
    df_rebounds <- df_rebounds %>%
      left_join(players, by = "number")  # Now they both have the same data type for 'number'
    
    return(df_rebounds)
  })
  
  
  
  
  # Rebounding court plot rendering
  output$reboundCourt <- renderPlot({
    df_rebounds <- get_rebound_data()
    
    if (nrow(df_rebounds) == 0) {
      return(plot(1, ylim = c(-30, 30), xlim = c(-50, 50), type = 'n', yaxs = 'i', xaxs = 'i', ylab = '', xlab = '', axes = F, asp = 1))
    }
    
    # Fix made_miss to ensure consistent naming
    df_rebounds$off_def <- factor(df_rebounds$off_def, levels = c("Defensive", "Offensive"))
    
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
    # Plot rebound points
    points(df_rebounds$event_x, df_rebounds$event_y, col = ifelse(df_rebounds$off_def == "Defensive", "black", "gold3"), pch = 19, cex = 1.5)
    
    # Add legend for rebound types
    legend("topright", legend = c("Defensive Rebound", "Offensive Rebound"), 
           fill = c("black", "gold3"), title = "Rebound Type", bty = "n", cex = 1.0)
  }, bg = "transparent")
 
  
  # Rebounding stats table rendering
  output$reboundingStatsTable <- renderDT({
    df_rebounds <- get_rebound_data()  # Fetch rebound data from the reactive function
    
    if (nrow(df_rebounds) == 0) return(NULL)
    
    # Aggregate rebound statistics per player
    rebound_stats <- df_rebounds %>%
      group_by(number, name) %>%
      summarize(
        total_rebounds = n(),
        offensive_rebounds = sum(off_def == "Offensive"),
        defensive_rebounds = sum(off_def == "Defensive"),
        .groups = "drop"  # Ensure no grouping remains
      )
    
    # Ensure 'number' is treated as numeric for sorting
    rebound_stats$number <- as.numeric(as.character(rebound_stats$number))
    
    # Handle "All Players" case by calculating team totals
    if (input$playerName == "All Players") {
      # Calculate team totals
      team_totals <- rebound_stats %>%
        summarise(
          number = NA_real_,  # Ensure that 'number' is numeric (NA_real_ is a numeric NA)
          name = "Team Total",
          offensive_rebounds = sum(offensive_rebounds, na.rm = TRUE),
          defensive_rebounds = sum(defensive_rebounds, na.rm = TRUE),
          total_rebounds = sum(total_rebounds, na.rm = TRUE)
        )
      
      # Append team totals to the player statistics
      rebound_stats <- bind_rows(rebound_stats, team_totals)
    }
    
    # Sort by player number and ensure 'Team Total' is at the bottom
    rebound_stats <- rebound_stats %>%
      arrange(ifelse(is.na(number), 1, number))  # Sort by player number and push 'NA' (team total) to the bottom
    
    # Render the table
    datatable(rebound_stats, 
              options = list(dom = 't', 
                             paging = FALSE, 
                             searching = FALSE, 
                             ordering = TRUE,  # Enable ordering by column
                             order = list(list(0, 'asc'))),  # Default order: sort by player number
              rownames = FALSE, 
              colnames = c("Player Number", "Player Name", "Total Rebounds", "Offensive Rebounds", "Defensive Rebounds"))
  })
  
}


# Shiny app
shinyApp(ui, server)