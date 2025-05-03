library(shiny)
library(shinyjs)
library(ggplot2)
library(patchwork)
library(dplyr)
library(magrittr)
source("../dataset.R")

predict_winner <- function(minute, t1_kills, t2_kills, first_blood, first_tower, 
                           t1_towers, t2_towers) {
  return("Team 1")
}


color1 <- "plum"
color2 <- "#A0DDA1"
N <- 12 * 60

server <- function(input, output, session) {

  dataset <- reactive({
	metadata <- read_csv("../data/game_metadata.csv", show_col_types = F)
	player_stats <- read_csv("../data/game_players_stats.csv", show_col_types = F)
	events <- read_csv("../data/game_events.csv", show_col_types = F)
	champs <- read_csv("../data/260225_LoL_champion_data.csv", show_col_types = F) %>%
		rename(name = `...1`) 
	dataset <- early_game_dataset(player_stats, metadata, events, champs, N)
  })


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

  output$diff_distribution <- renderPlot({
	dataset() %>%
		select(ends_with("_diff")) %>%
		colnames() %>%
		map(~ {
			col_sym <- sym(.x)
			ggplot(dataset(), aes(x = !!col_sym)) +
				geom_histogram(
					binwidth = 1,
					center = 0,
					fill = color1,
					color = "black",
					alpha = 0.8
				) +
				labs(x = .x, y = "Number of games") +
				theme_minimal()
		}) %>%
		wrap_plots(ncol = 2)
  });
}

