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
library(ggforce)


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
            menuItem("Game Charter", tabName = "gameCharter", icon = icon("basketball"))
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
    username = "",
    password = ""
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

  
  ####################################### Game Charter ########################################
  
  GCState <- reactiveVal("SetUP")
  
  output$GCState <- renderText({
    GCState()
  })
  
  
  
  output$setUp <- renderUI({
    
    if(GCState() == "SetUP"){
      tagList(
        fluidRow(
          column(3, dateInput("gameDate", "Game Date", value = Sys.Date())),
          column(3, selectInput("oppo", "Opponent", choices = c("Scrimmage", "MonStars", "Jim"))),
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
          column(3, numericInput("Oppo_1", "Opposing Team 1", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_2", "Lindenwood 2", choices = players$number)),
          column(3, numericInput("Oppo_2", "Opposing Team 2", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_3", "Lindenwood 3", choices = players$number)),
          column(3, numericInput("Oppo_3", "Opposing Team 3", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_4", "Lindenwood 4", choices = players$number)),
          column(3, numericInput("Oppo_4", "Opposing Team 4", value = 0))
        ),
        fluidRow(
          column(3, selectInput("LU_5", "Lindenwood 5", choices = players$number)),
          column(3, numericInput("Oppo_5", "Opposing Team 5", value = 0))
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
    gameValues$quarter <- 1  # Starting with quarter 1
    
    
  })
  
  on_court <- reactive({
    c(
      gameValues$HOME_1,
      gameValues$HOME_2,
      gameValues$HOME_3,
      gameValues$HOME_4,
      gameValues$HOME_5,
      gameValues$AWAY_1,
      gameValues$AWAY_2,
      gameValues$AWAY_3,
      gameValues$AWAY_4,
      gameValues$AWAY_5
    )
  })
  
  output$charting <- renderUI({
    
    if(GCState() == "charting"){
      tagList(
        radioButtons("eventType", "Shot", choices = c("Shot")),
        radioButtons("eventPerson", "Shooter", choices = on_court(), inline = TRUE),
        plotOutput("court", click = "courtClick"),
        # Conditional UI for Shot events
        conditionalPanel(
          condition = "input.eventType == 'Shot'",
          radioButtons("shotOutcome", "Outcome of Shot", choices = c("Made Shot", "Missed Shot"))
        ),
        
        # Conditional UI for Missed Shot events
        conditionalPanel(
          condition = "input.eventType == 'Shot' && input.shotOutcome == 'Missed Shot'",
          h4("Who rebounded the missed shot?"),
          radioButtons("reboundPlayer", "Rebounder", choices = on_court(), inline = TRUE),
          radioButtons("reboundOutcome", "Rebound Outcome", 
                       choices = c("Offensive Rebound", "Defensive Rebound"))
        
        )
      )
    }
  })
  
  
  #output$court <- renderPlot({
    #par(mai = c(0, 0, 0, 0))
    #plot(1, ylim = c(-30, 30), xlim = c(-50, -50), type = 'n', yaxs = 'i', xaxs = 'i', ylab = '', xlab = '', axes = F, asp = 1)
    #rect(xleft = -47, ybottom = -25,
     #    xright = 47, ytop = 25,
      #   border = "black", lwd = 2)
   # segments(0, -25, 0, 25, col = "black", lwd = 2)
   
   # Plot the court in your renderPlot
  output$court <- renderPlot({
    ggplot(data=data.frame(x=1,y=1),aes(x,y))+
      #outside box:
      geom_path(data=data.frame(x=c(-47,47,47,-47,-47),y=c(-25,-25,25,25,-25)))+
      #halfcourt line:
      geom_path(data=data.frame(x=c(0,0),y=c(-25,25)))+
      #halfcourt semicircle
      geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
      geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
      #solid FT semicircle above FT line
      geom_path(data=data.frame(y=c(-6000:(-1)/1000,1:6000/1000),x=c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
      geom_path(data=data.frame(y=c(-6000:(-1)/1000,1:6000/1000),x=-c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
      #dashed FT semicircle below FT line
      geom_path(data=data.frame(y=c(-6000:(-1)/1000,1:6000/1000),x=c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed')+
      geom_path(data=data.frame(y=c(-6000:(-1)/1000,1:6000/1000),x=-c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed')+
      #key
      geom_path(data=data.frame(y=c(-8,-8,8,8,-8),x=c(47,28,28,47,47)))+
      geom_path(data=data.frame(y=-c(-8,-8,8,8,-8),x=-c(47,28,28,47,47)))+
      #box inside the key
      geom_path(data=data.frame(y=c(-6,-6,6,6,-6),x=c(47,28,28,47,47)))+
      geom_path(data=data.frame(y=c(-6,-6,6,6,-6),x=-c(47,28,28,47,47)))+
      #restricted area semicircle
      geom_path(data=data.frame(y=c(-4000:(-1)/1000,1:4000/1000),x=c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
      geom_path(data=data.frame(y=c(-4000:(-1)/1000,1:4000/1000),x=-c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
      #rim
      geom_path(data=data.frame(y=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),x=c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
      geom_path(data=data.frame(y=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),x=-c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
      #backboard
      geom_path(data=data.frame(y=c(-3,3),x=c(43,43)),lineend='butt')+
      geom_path(data=data.frame(y=c(-3,3),x=-c(43,43)),lineend='butt')+
      #three-point line
      geom_path(data=data.frame(y=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),x=c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y))+
      geom_path(data=data.frame(y=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),x=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y))+
      coord_fixed()
      })
}


# Shiny app
shinyApp(ui, server)
