library(shiny)
library(shinyjs)
library(ggplot2)
library(patchwork)
library(dplyr)
library(magrittr)
library(scales)
library(caret)
library(rpart)
library(rpart.plot)
library(yardstick)
library(ranger)
library(adabag)
source("../dataset.R")
source("../model_comparison.R")

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
		champs <- suppressMessages(read_csv("../data/260225_LoL_champion_data.csv", show_col_types = F) %>%
			rename(name = `...1`)) 
		early_game_dataset(player_stats, metadata, events, champs, N)
	});

	train_test_split <- reactive({
		req(dataset())
	 	set.seed(42069)
	 	train_indices <- createDataPartition(dataset()$team1_won, p = 0.75, list = FALSE, times = 1)
	 	train_data <- dataset()[train_indices, ] %>% mutate(team1_won = factor(team1_won))
	 	test_data <- dataset()[-train_indices, ] %>% mutate(team1_won = factor(team1_won))
		
		list(train_data = train_data, test_data = test_data)
	})

	 decition_tree_model <- reactive({
		req(train_test_split())
		train_data <- train_test_split()$train_data

		formula <- team1_won ~ kill_diff + dragon_diff + first_herald
		tree_model <- rpart(
			formula = formula,     
			data = train_data,     
			method = "class",       
			cp = 0.001,             
		)

		list(model = tree_model)
	 });

	rf_model <- reactive({
		req(train_test_split())
		train_data <- train_test_split()$train_data

		set.seed(42069)
		formula <- team1_won ~ kill_diff + dragon_diff + rift_herald_diff +
			tower_diff + first_blood + first_dragon + first_herald +
			first_tower

		rf <- ranger(formula = formula, data = train_data)

		rf_with_prob <- ranger(formula = formula, data = train_data, probability = TRUE)

		list(model = rf, with_prob = rf_with_prob)
	})

	adaboost_model <- reactive({
		req(train_test_split())
		train_data <- train_test_split()$train_data

		set.seed(42069)
		ada_formula <- team1_won ~ kill_diff + dragon_diff + rift_herald_diff +
		  tower_diff + first_blood + first_dragon + first_herald +
		  first_tower

		model_file_path <- "../models/adaboost_tuned.rds"

		if (file.exists(model_file_path)) {
			ada_model <- readRDS(model_file_path)
		} else {
			stop("adaboost model not found")
		}

		list(model = ada_model)
	})



  output$summary <- renderPrint({
    req(input$file)
    summary(read.csv(input$file$datapath))
  });

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
  });
  
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
  });
  
  observeEvent(input$reset_button, {
    prediction_result(NULL)
    view_state("input")
  });
  
  train_result <- reactiveVal(NULL)
  
  output$train_display_wrapper <- renderUI({
    if (is.null(train_result())) {
      return(NULL)
    }
    train_result <- train_result()
    
    tags$div(
      style = "text-align: center; background-color: rgba(0, 50, 78, 0.9); color: var(--lol-gold); padding: 20px 30px 10px 30px; border-radius: var(--border-radius-main); margin-top: 15px; margin-bottom: 15px; border: 1px solid var(--lol-gold); box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);",
      h4("Training summary", style = "font-weight: bold; margin-bottom: 25px;"),
      div(
        style = "width: 100%; display: flex; justify-content: space-between; margin-bottom: 10px;",
        div("Training data percentage:", style = "width: 80%; text-align: left;"),
        div(paste(input$train_test_split, "%"), style = "color: cornsilk;"),
      ),
      div(
        style = "width: 100%; display: flex; justify-content: space-between; margin-bottom: 10px;",
        div("Testing data percentage:", style = "width: 80%; text-align: left;"),
        div(paste(100 - input$train_test_split, "%"), style = "color: cornsilk;"),
      ),
      div(
        style = "width: 100%; display: flex; justify-content: space-between; margin-bottom: 10px;",
        div("Number of trees in the forest:", style = "width: 80%; text-align: left;"),
        div(input$num_trees, style = "color: cornsilk;"),
      ),
      div(
        style = "width: 100%; display: flex; justify-content: space-between; margin-bottom: 10px;",
        div("Number of variables randomly sampled at each split:", style = "width: 80%; text-align: left;"),
        div(input$mtry, style = "color: cornsilk;"),
      ),
      div(
        style = "width: 100%; display: flex; justify-content: space-between; margin-bottom: 10px;",
        div("Minimum number of observations in a terminal node:", style = "width: 80%; text-align: left;"),
        div(input$min_node_size, style = "color: cornsilk;"),
      ),
      plotOutput("decision_tree_plot"),
      br(),
      actionButton("reset_train_button", "Reset")
    )
  })
  
  observeEvent(input$train_button, {
    # Train model here and put it into train_result
    train_result(list(
      train_test_split = input$train_test_split,
      num_trees = input$num_trees,
      mtry = input$mtry,
      min_node_size = input$min_node_size
    ))
  })
  
  observeEvent(input$reset_train_button, {
    train_result(NULL)
  })
  
  output$plot <- renderPlot({
    hist(rnorm(100))
  });

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
		wrap_plots(ncol = 2) +
		plot_annotation(title = "Distribution of diffs (team 1 - team 2)")
  });

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
		wrap_plots(ncol = 2) +
		plot_annotation(title = "Distribution of diffs (team 1 - team 2)")
  });

	output$killdiff_dist <- renderPlot({
		dataset() %>%
            ggplot(aes(x = factor(team1_won), y = kill_diff, fill = factor(team1_won))) +
            geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 1.5) +
            scale_x_discrete(labels = c("0" = "Team 2 Wins", "1" = "Team 1 Wins")) +
            scale_fill_manual(
                values = c("0" = color2, "1" = color1),
                guide = "none"
            ) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
            labs(
                title = "Distribution of kill_diff by game outcome",
                x = "Game outcome",
                y = "kill_diff (team1 - team2)") +
            theme_minimal()
  });

	output$champdiff_distribution <- renderPlot({
		dataset() %>%
			select(ends_with("_champdiff")) %>%
			colnames() %>%
			map(~ {
				col_sym <- sym(.x)
				
				dataset() %>%
					ggplot(aes(x = factor(team1_won), y = !!col_sym, fill = factor(team1_won))) +
					geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 1.5) +
					scale_x_discrete(labels = c("0" = "Team 2 Wins", "1" = "Team 1 Wins")) +
					scale_fill_manual(
						values = c("0" = color2, "1" = color1),
						guide = "none"
					) +
					geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
					labs(
						title = paste("Distribution of", .x, "by game outcome"),
						x = "Game outcome",
						y = .x
					) +
					theme_minimal()
			}) %>%
			wrap_plots(ncol = 2)
  })

	output$first_dist <- renderPlot({
		dataset() %>%
			select(starts_with("first_")) %>%
			colnames() %>%
			map(~ {
				col_sym <- sym(.x)
				
				dataset() %>%
					ggplot(aes(x = !!col_sym)) +
					geom_bar(aes(fill = !!col_sym), stat = "count", alpha = 0.8) +
					geom_text(
						aes(label = after_stat(count)),
						stat = "count",
						vjust = -0.5
					) +
					scale_fill_manual(
						values = c("team1" = color1, "team2" = color2, "none" = "grey"),
						guide = "none") +
					labs(
						 x = paste(.x, "achieved by"),
						 y = NULL) +
					theme_minimal()
			}) %>%
			wrap_plots(ncol = 4)
	})

	output$first_gamewin_dist <- renderPlot({
		dataset() %>%
			select(starts_with("first_")) %>%
			colnames() %>%
			map(~ {
				col_sym <- sym(.x)
				
				dataset() %>%
					mutate(team1_won = factor(
						team1_won,
						levels = c(0, 1),
						labels = c("team 2 wins", "team 1 wins")
					)) %>%
					count(!!col_sym, team1_won, name = "n") %>%
					group_by(!!col_sym) %>%
					mutate(prop = n / sum(n)) %>%
					ungroup() %>%
					ggplot(aes(x = !!col_sym, y = prop, fill = team1_won)) +
					geom_col(position = position_dodge(preserve = "single")) +
					geom_text(
						aes(label = percent(prop, accuracy = .1)),
						position = position_dodge(width = 1),
						vjust = -0.5,
						size = 4) +
					scale_y_continuous(labels = percent_format(accuracy = .1)) +
					scale_fill_manual(values = c("team 1 wins" = color1, "team 2 wins" = color2)) +
					labs(
						 x = paste(.x, "achieved by"),
						 y = "Proportion of games", fill = "Game outcome") +
					theme_minimal()
			}) %>%
			wrap_plots(ncol = 4, guides = "collect") &
			theme(legend.position = "bottom")
	})


	output$first_times_winrates <- renderPlot({
		first_to_times <- list(
			first_blood = "time_first_blood",
			first_dragon = "time_first_dragon",
			first_herald = "time_first_herald",
			first_tower = "time_first_tower")

		first_to_times %>%
			imap(~ {
				first_col_sym <- sym(.y)
				time_col_sym <- sym(.x)
				
				dataset() %>%
					filter(!!first_col_sym != "none") %>%
					mutate(
						time_bin = cut(
							!!time_col_sym,
							breaks = seq(0, N, by = 60),
							include.lowest = TRUE,
							right = FALSE),
						first_team_won = case_when(
							!!first_col_sym == "team1" & team1_won == 1 ~ TRUE,
							!!first_col_sym == "team2" & team1_won == 0 ~ TRUE,
							TRUE ~ FALSE)) %>%
					group_by(time_bin) %>%
					summarise(
						n_games = n(),
						win_rate = mean(first_team_won, na.rm = TRUE)) %>%
					filter(!is.na(time_bin)) %>%
					ggplot(aes(x = time_bin, y = win_rate)) +
					geom_col(fill = color1, alpha = 0.8) +
					geom_text(aes(label = percent(win_rate, accuracy = 0.1)), vjust = -0.5) +
					scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
					labs(
						title = paste("Win rate for team securing", .y),
						x = paste("Time bin when", .y, "occured"),
					) +
					theme_minimal()
			}) %>%
			wrap_plots(ncol = 1)
	})


	output$diff_first_rel_winrate <- renderPlot({
		direct_rels <- list(
			first_blood = "kill_diff",
			first_dragon = "dragon_diff",
			first_herald = "rift_herald_diff",
			first_tower = "tower_diff")

		direct_rels %>%
			imap(~ {
				first_col_sym <- sym(.y)
				diff_col_sym <- sym(.x)
				
				dataset() %>%
					mutate(diff_sign = case_when(
						!!diff_col_sym > 0 ~ "team1 lead",
						!!diff_col_sym < 0 ~ "team2 lead",
						TRUE ~ "diff zero"
					)) %>%
					mutate(team1_won = factor(
						team1_won,
						levels = c(0, 1),
						labels = c("team 2 wins", "team 1 wins")
					)) %>%
					count(!!first_col_sym, diff_sign, team1_won, name = "n") %>%
					group_by(!!first_col_sym, diff_sign) %>%
					mutate(prop = n / sum(n)) %>%
					ungroup() %>%
					ggplot(aes(x = !!first_col_sym, y = prop, fill = team1_won)) +
					geom_col(position = position_dodge(preserve = "single")) +
					geom_text(
						aes(label = percent(prop, accuracy = .1)),
						position = position_dodge(width = 0.9),
						vjust = -0.5,
						size = 3) + 
					facet_wrap(~diff_sign) +
					scale_y_continuous(labels = percent_format(accuracy = .1)) +
					scale_fill_manual(values = c("team 1 wins" = color1, "team 2 wins" = color2)) +
					labs(
						title = paste("Win rate by", .y),
						x = paste(.y, "achieved by"),
						y = "Proportion of games",
						fill = "Game outcome") +
					theme_minimal(base_size = 12) +
					theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
			}) %>%
			wrap_plots(ncol = 1)
	})

	output$decision_tree_plot <- renderPlot({
		tree_model <- decition_tree_model()$model
		plot_decision_tree(tree_model)
	})

	output$decision_tree_roc <- renderPlot({
		tree_model <- decition_tree_model()$model
		test_data <- train_test_split()$test_data
		roc(tree_model, test_data)
	})

	output$rf_roc <- renderPlot({
		rf_model_prob <- rf_model()$with_prob
		test_data <- train_test_split()$test_data
		roc_rf(rf_model_prob, test_data)
	})

	output$ada_roc <- renderPlot({
		ada_model <- adaboost_model()$model
		test_data <- train_test_split()$test_data
		roc(ada_model, test_data)
	})

	output$model_comparison_table <- renderTable({
		tibble(
			Model = c("Decision tree", "Random forest", "AdaBoost"),
			Accuracy = c(0.669, 0.680, 0.681),
			Sensitivity = c(0.750, 0.689, 0.7245),
			Specificity = c(0.577, 0.6712, 0.6327),
			AUC = c(0.710, 0.745, 0.748))
		}, digits = 3)
}

