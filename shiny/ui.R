library(shiny)
library(shinyjs)

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

presentaion_ui <- tabPanel(
	"Presentation",
	fluidPage(
		div(style = "background-color: rgba(0, 50, 78, .95); color: var(--lol-gold); padding: 20px 0 25px 0; border-radius: 15px; border: 1px solid var(--lol-gold); display: flex; flex-direction: column; gap: 8px",
			fluidRow(
				tags$h2("Predicting outcome of League of Legends game by early game", style = "text-align: center; margin-bottom: 20px;"),
				column(3, offset = 2,
					tags$h3("LoL - League of Legends", style = "margin-top: 20px; color: var(--lol-gold);"),
					tags$li("League of Legends is a 5v5 mutlplayer online game"),
					tags$h3("Our Goal", style = "margin-top: 20px; color: var(--lol-gold);"),
					tags$li("Predict outcome of a game using only data from early game.")),
				column(5, offset = 2, tags$img(src = "lol-map.ppm", width = "400px", alt = "LoL map"))),

			fluidRow(
				column(3, offset = 2,
					tags$h3("Dataset", style = "margin-top: 20px; color: var(--lol-gold);"),
					tags$ul(
						tags$li("Data recorded from professional League of Legends games"),
						tags$li("Player kills, tower destructions, dragon kills..."),
						tags$li("Created a \"snapshot\" of early game")
					)),
				column(5, plotOutput("diff_distribution")),
				)

		),
	  )
)

ui <- tags$div(
  style = "width: 100%; padding: 0; margin: 0;",
  navbarPage(
    title = "Game Conclusion Predictor",
    id = "navbar",
    presentaion_ui,
    predictor_ui,
    data_upload_ui,
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

