library(shiny)
library(shinyjs)

predict_winner <- function(minute, t1_kills, t2_kills, first_blood, first_tower, 
                           t1_towers, t2_towers) {
  return("Team 1")
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  conditionalPanel(
    condition = "output.show_input_panel",
    fluidRow(
      column(5, div(class="header-box", "GAME CONCLUSION PREDICTOR")),
      column(2, div(class="header-logo", tags$img(src="lol-logo-.png", width = "100px", style="display: flex; ", alt="Logo"))),
      column(5, div(class="header-box", "BASED ON EARLY GAME EVENTS"))
    ),
    
    fluidRow(
      column(10, offset = 1,
             div(class="slider-container",
                 h4("Minute", style="color: var(--lol-gold);"),
                 sliderInput("minute", label=NULL, min = 5, max = 15, value = 10, step = 1, width = "100%")
             )
      )
    ),
    
    fluidRow(
      class = "slider-container",
      style = "background-color: rgba(0, 50, 78, 0.9); border-radius: 15px; border: 1px solid var(--lol-gold); margin-bottom: 30px; margin-left: 0px; margin-right: 0px;",
      column(
        width = 6,
        align = "center",
        div(class = "custom-radio-title", "First Player Kill"),
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
        div(class = "custom-radio-title", "First Tower Kill"),
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
                 div(class="mb-4",
                     tags$label("Player kills", `for`="t1_kills", class="control-label", 
                                style="color: var(--lol-gold); margin-bottom: 8px; display: block;"),
                     numericInput("t1_kills", label=NULL, value = 0, min = 0, width="100%")
                 ),
                 div(
                   tags$label("Tower kills", `for`="t1_towers", class="control-label", 
                              style="color: var(--lol-gold); margin-bottom: 8px; display: block;"),
                   numericInput("t1_towers", label=NULL, value = 0, min = 0, width="100%")
                 )
             )
      ),
      
      column(6,
             div(class="team-panel",
                 h4("TEAM 2"),
                 div(class="mb-4",
                     tags$label("Player kills", `for`="t2_kills", class="control-label", 
                                style="color: var(--lol-gold); margin-bottom: 8px; display: block;"),
                     numericInput("t2_kills", label=NULL, value = 0, min = 0, width="100%")
                 ),
                 div(
                   tags$label("Tower kills", `for`="t2_towers", class="control-label", 
                              style="color: var(--lol-gold); margin-bottom: 8px; display: block;"),
                   numericInput("t2_towers", label=NULL, value = 0, min = 0, width="100%")
                 )
             )
      )
    ),
    fluidRow(
      column(12,
             div(id="predict_button_container",
                 actionButton("predict_button", "PREDICT")
             )
      ),
    )
  ),
  
  conditionalPanel(
    condition = "output.show_output_panel",
    fluidRow(
      column(8, offset = 2, align = "center",
             uiOutput("prediction_display"),
             br(),
             actionButton("reset_button", "New Prediction")
      )
    )
  ),
)

server <- function(input, output, session) {
  prediction_result <- reactiveVal(NULL)
  view_state <- reactiveVal("input")
  
  output$show_input_panel <- reactive({
    view_state() == "input"
  })
  outputOptions(output, "show_input_panel", suspendWhenHidden = FALSE)
  
  output$show_output_panel <- reactive({
    view_state() == "output"
  })
  outputOptions(output, "show_output_panel", suspendWhenHidden = FALSE)
  
  observeEvent(input$predict_button, {
    pred <- predict_winner(
      minute = input$minute,
      t1_kills = input$t1_kills,
      t2_kills = input$t2_kills,
      first_blood = input$first_blood,
      first_tower = input$first_tower,
      t1_towers = input$t1_towers,
      t2_towers = input$t2_towers
    )
    prediction_result(pred)
    view_state("output")
  })
  
  observeEvent(input$reset_button, {
    prediction_result(NULL)
    view_state("input")
  })
  
  output$prediction_display <- renderUI({
    req(prediction_result())
    predicted_winner <- prediction_result()
    
    tags$div(
      p("Prediction:", class="prediction-headline"),
      h2(paste(predicted_winner, "is predicted to win."), class="prediction-text"),
      h4(ifelse(predicted_winner == "Team 1", 
                 "If you're playing as Team 1, you should keep playing!", 
                 "If you're playing as Team 2, you should keep playing!")),
      h4(ifelse(predicted_winner == "Team 1", 
                 "If you're playing as Team 2, you should forfeit!",
                 "If you're playing as Team 1, you should forfeit!")),
    )
  })
}

shinyApp(ui = ui, server = server)
