library(shiny)
library(shinyjs)

predict_winner <- function(minute, t1_kills, t2_kills, first_blood, first_tower, 
                           t1_towers, t2_towers) {
  return("Team 1")
}

predictor_ui <- tabPanel(
  "Predictor",
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    fluidRow(
      column(5, div("GAME CONCLUSION PREDICTOR", style = "background-color: rgba(0, 50, 78, 0.9); color: var(--lol-gold); padding: 10px 16px; border-radius: var(--border-radius-main); text-align: center; font-weight: bold; font-size: 1.3em; border: 1px solid var(--lol-gold); box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3); display: flex; align-items: center; justify-content: center; min-height: 60px;")),
      column(2, div(style = "display: flex; justify-content: center;", tags$img(src="lol-logo-.png", width = "100px", style="display: flex; ", alt="Logo"))),
      column(5, div("BASED ON EARLY GAME EVENTS", style = "background-color: rgba(0, 50, 78, 0.9); color: var(--lol-gold); padding: 10px 16px; border-radius: var(--border-radius-main); text-align: center; font-weight: bold; font-size: 1.3em; border: 1px solid var(--lol-gold); box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3); display: flex; align-items: center; justify-content: center; min-height: 60px;")),
    ),
    
    fluidRow(
      column(10, offset = 1,
             div(style = "text-align: center; background-color: rgba(0, 50, 78, 0.9); padding: 20px 30px 10px 30px; border-radius: var(--border-radius-main); margin-top: 15px; margin-bottom: 15px; border: 1px solid var(--lol-gold); box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);",
                 h4("Minute", style="color: var(--lol-gold);"),
                 sliderInput("minute", label=NULL, min = 5, max = 15, value = 10, step = 1, width = "100%")
             )
      )
    ),
    
    fluidRow(
      style = "background-color: rgba(0, 50, 78, 0.9); padding: 20px 30px 10px 30px; border-radius: var(--border-radius-main); border: 1px solid var(--lol-gold); margin-top: 15px; margin-bottom: 30px; margin-left: 0px; margin-right: 0px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3); text-align: center;",
      column(
        width = 6,
        align = "center",
        div("First Player Kill", style = "color: var(--lol-gold); text-align: center; font-size: 1.4em; margin-bottom: 15px; font-weight: bold;"),
        div(
          class = "custom-radio-group",
          radioButtons(
            "firstKill",
            label = NULL,
            choices = c("Team 1", "None", "Team 2"),
            selected = "None",
            inline = TRUE
          )
        )
      ),
      column(
        width = 6,
        align = "center",
        div("First Tower Kill", style = "color: var(--lol-gold); text-align: center; font-size: 1.4em; margin-bottom: 15px; font-weight: bold;"),
        div(
          class = "custom-radio-group",
          radioButtons(
            "firstTower",
            label = NULL,
            choices = c("Team 1", "None", "Team 2"),
            selected = "None",
            inline = TRUE
          )
        )
      )
    ),
    
    fluidRow(
      column(6,
             div(class="team-panel",
                 h4("TEAM 1"),
                 div(style = "margin-bottom: 25px;",
                     tags$label("Player kills", `for`="t1_kills", class="control-label", 
                                style="color: var(--lol-gold); display: block;"),
                     numericInput("t1_kills", label=NULL, value = 0, min = 0, width="100%")
                 ),
                 div(
                   tags$label("Tower kills", `for`="t1_towers", class="control-label", 
                              style="color: var(--lol-gold); display: block;"),
                   numericInput("t1_towers", label=NULL, value = 0, min = 0, width="100%")
                 )
             )
      ),
      
      column(6,
             div(class="team-panel",
                 h4("TEAM 2"),
                 div(style = "margin-bottom: 25px;",
                     tags$label("Player kills", `for`="t2_kills", class="control-label", 
                                style="color: var(--lol-gold); display: block;"),
                     numericInput("t2_kills", label=NULL, value = 0, min = 0, width="100%")
                 ),
                 div(
                   tags$label("Tower kills", `for`="t2_towers", class="control-label", 
                              style="color: var(--lol-gold); display: block;"),
                   numericInput("t2_towers", label=NULL, value = 0, min = 0, width="100%")
                 )
             )
      )
    ),
    fluidRow(
      column(8, offset = 2, align = "center",
             uiOutput("prediction_display_wrapper")
      )
    ),
    fluidRow(
      column(8, offset = 2, align = "center",
             div(style = "display: flex; align-items: center; justify-content: center; height: 100%; margin-top: 25px; margin-bottom: 25px;",
                 actionButton("predict_button", class="predict_button", "PREDICT")
             )
      )
    )
  )
)


data_upload_ui <- tabPanel(
  "Data Upload",
  fluidRow(
    column(8, offset = 2,
           div(style = "background-color: rgba(0, 50, 78, 0.9); color: var(--lol-gold); padding: 20px; border-radius: 15px; border: 1px solid var(--lol-gold); margin-bottom: 25px;",
               fileInput("file", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               verbatimTextOutput("summary")
           )
    )
  ),
  fluidRow(
    column(8, offset = 2,
           div(class="slider-container",
               h4("Parameters", style="color: var(--lol-gold);"),
               h5("Parameter 1", style="color: var(--lol-gold); text-align: left;"),
               sliderInput("parameter1", label=NULL, min = 0, max = 100, value = 50, step = 1, width = "100%"),
               h5("Parameter 2", style="color: var(--lol-gold); text-align: left;"),
               sliderInput("parameter2", label=NULL, min = 0, max = 100, value = 50, step = 1, width = "100%"),
               h5("Parameter 3", style="color: var(--lol-gold); text-align: left;"),
               sliderInput("parameter3", label=NULL, min = 0, max = 100, value = 50, step = 1, width = "100%"),
           )
    )
  ),
  fluidRow(
    column(8, offset = 2, align = "center",
           div(style = "display: flex; align-items: center; justify-content: center; height: 100%; margin-top: 25px; margin-bottom: 25px;",
               actionButton("train_button", "TRAIN")
           )
    )
  )
)

eda_ui <- tabPanel(
  "Exploratory Data Analysis",
  fluidRow(
    column(8, offset = 2,
           div(style = "background-color: rgba(0, 50, 78, 0.9); padding: 20px; border-radius: 15px; border: 1px solid var(--lol-gold);",
               plotOutput("plot")
           )
    )
  )
)

ui <- tags$div(
  style = "width: 100%; padding: 0; margin: 0;",
  navbarPage(
    title = "Game Conclusion Predictor",
    id = "navbar",
    predictor_ui,
    data_upload_ui,
    eda_ui,
    header = tags$style(HTML("
      body {
        padding: 0;
        margin: 0;
      }
      #navbar {
        margin: 0;
      }
      .navbar {
        margin-bottom: 0;
        border-radius: 0;
        background-color: var(--lol-dark-blue);
        border: none;
      }
      .navbar-default .navbar-nav > li > a {
        color: var(--lol-gold);
      }
      .navbar-default .navbar-brand {
        color: var(--lol-gold);
      }
      .tab-content {
        padding-top: 20px;
      }
    ")),
    windowTitle = "Game Conclusion Predictor",
    collapsible = TRUE,
    theme = "default",
    inverse = TRUE
  )
)

server <- function(input, output, session) {
  output$summary <- renderPrint({
    req(input$file)
    summary(read.csv(input$file$datapath))
  })

  prediction_result <- reactiveVal(NULL)
  view_state <- reactiveVal("input")
  
  output$prediction_display_wrapper <- renderUI({
    if (is.null(prediction_result())) {
      return(NULL)
    }
    prediction_result <- prediction_result()
    predicted_winner <- prediction_result
    
    tags$div(
      style = "text-align: center; background-color: rgba(0, 50, 78, 0.9); padding: 20px 30px 10px 30px; border-radius: var(--border-radius-main); margin-top: 15px; margin-bottom: 15px; border: 1px solid var(--lol-gold); box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);",
      p("Prediction:", class="prediction-headline"),
      h2(paste(predicted_winner, "is predicted to win."), class="prediction-text"),
      h4(ifelse(predicted_winner == "Team 1", 
                "If you're playing as Team 1, you should keep playing!", 
                "If you're playing as Team 2, you should keep playing!")),
      h4(ifelse(predicted_winner == "Team 1", 
                "If you're playing as Team 2, you should forfeit!",
                "If you're playing as Team 1, you should forfeit!")),
      br(),
      actionButton("reset_button", "Reset")
    )
  })
  
  observeEvent(input$predict_button, {
    pred <- predict_winner(
      minute = input$minute,
      t1_kills = input$t1_kills,
      t2_kills = input$t2_kills,
      first_blood = input$firstKill,
      first_tower = input$firstTower,
      t1_towers = input$t1_towers,
      t2_towers = input$t2_towers
    )
    prediction_result(pred)
    view_state("output")
  })
  
  observeEvent(input$train_button, {
    
  })
  
  observeEvent(input$reset_button, {
    prediction_result(NULL)
    view_state("input")
  })
  
  output$plot <- renderPlot({
    hist(rnorm(100))
  })
}

shinyApp(ui = ui, server = server)
