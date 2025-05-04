plot_decision_tree <- function(tree_model) {
  rpart.plot(
    tree_model,
    box.palette = c(color2, color1),
    main = "Predicting winner of a game by early game",
  )
}

compare_conf_matrices <- function(conf1, conf2, names) {
  overall1 <- conf1$overall
  byClass1 <- conf1$byClass
  overall2 <- conf2$overall
  byClass2 <- conf2$byClass
  comparison_df <- data.frame(
    Model = names,
    Accuracy = c(overall1['Accuracy'], overall2['Accuracy']),
    Sensitivity = c(byClass1['Sensitivity'], byClass2['Sensitivity']),
    Specificity = c(byClass1['Specificity'], byClass2['Specificity']),
    Balanced_Accuracy = c(
      byClass1['Balanced Accuracy'],
      byClass2['Balanced Accuracy']
    ),
    check.names = FALSE
  ) %>%
    mutate(across(where(is.numeric), ~ percent(x = ., accuracy = 0.01))) %>%
    print()
}

roc <- function(model, data) {
  probabilities <- predict(model, newdata = data, type = "prob")
  data %>%
    select(team1_won) %>%
    bind_cols(probabilities) %>%
    plot_roc()
}

roc_rf <- function(model, data) {
  probabilities <- predict(model, data = data)
  data %>%
    select(team1_won) %>%
    bind_cols(probabilities$predictions) %>%
    plot_roc()
}

plot_roc <- function(predictions_with_probs) {
  curve <- roc_curve(
    predictions_with_probs,
    truth = team1_won,
    `1`,
    event_level = "second"
  )
  auc_value <- roc_auc(
    predictions_with_probs,
    truth = team1_won,
    `1`,
    event_level = "second"
  )

  autoplot(curve) +
    annotate(
      "text",
      x = 0.75,
      y = 0.05,
      label = paste("AUC =", format(auc_value$.estimate, digits = 3)),
      hjust = 0,
      vjust = 0,
      size = 4
    ) +
    theme_minimal()
}
