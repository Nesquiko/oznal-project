#' Filters games that have complete data for both teams and returns
#' the game id, team1 id, team2 id and target team1 won (0 if not, 1 if yes).
teams_in_games <- function(player_stats_df) {
    player_stats_df %>%
        group_by(game_id) %>%
        summarise(
            n_players = n_distinct(player_id),
            n_distinct_teams = n_distinct(team_id),
            team1_id = min(team_id, na.rm = T),
            team2_id = max(team_id, na.rm = T),
            # first() is used because all players on the same team have the same win status
            team1_won = first(win[team_id == team1_id]),
            .groups = "drop"
        ) %>%
        # filter out those games that don't have stats for both whole teams,
        # example game_id 13358 has only one team
        filter(n_players == 10, n_distinct_teams == 2) %>%
        mutate(team1_won = as.integer(team1_won)) %>%
        select(game_id, team1_id, team2_id, team1_won)
}

#' Goes through all the events of event_type and counts them for each game and
#' team. Also finds out if an event happened to team1 or team2.
event_counts_by_type <- function(events, event_types, player_team_lookup, games) {
    events %>%
        filter(event_type %in% event_types) %>%
        left_join(player_team_lookup, by = c("game_id", "killer_id" = "player_id")) %>%
        left_join(games, by = "game_id") %>%
        mutate(event_team = case_when(
            team_id == team1_id ~ "team1",
            team_id == team2_id ~ "team2",
            TRUE ~ "other"
        )) %>%
        filter(event_team != "other") %>%
        count(game_id, event_type, event_team, name = "n") %>%
        pivot_wider(
            names_from = c(event_type, event_team),
            values_from = n,
            values_fill = 0
        )
}

#' Calculates the differences of kills, dragon kills, rift herald kills,
#' towers destroyed and grub kills between the two teams. If a diff is positive,
#' then team 1 has more, if negative, team 2 has more.
calculate_diffs <- function(games, event_counts) {
    games %>%
        select(game_id) %>%
        left_join(event_counts, by = "game_id") %>%
        mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
        mutate(
            kill_diff = player_kill_team1 - player_kill_team2,
            dragon_diff = drake_kill_team1 - drake_kill_team2,
            rift_herald_diff = rift_herald_kill_team1 - rift_herald_kill_team2,
            tower_diff = tower_kill_team1 - tower_kill_team2,
        ) %>%
        select(game_id, kill_diff, dragon_diff, rift_herald_diff, tower_diff)
}

#' Finds the first event of a event type in games. There can be multiple events
#' at the same timestamp, and if this happens then no teams is declared as having
#' the first event.
find_first_event <- function(event_data, lookup_data, games_data, event_type_name) {
    event_data %>%
        # find minimal timestamps for each game and even type
        filter(event_type == event_type_name) %>%
        group_by(game_id) %>%
        summarise(timestamp = min(timestamp, na.rm = TRUE), .groups = "drop") %>%
        # join events at the minimum timestamp, there can be multiple
        left_join(
            event_data %>% filter(event_type == event_type_name) %>% select(game_id, timestamp, killer_id),
            by = c("game_id", "timestamp"),
            relationship = "one-to-many"
        ) %>%
        # count events at the min ts, if there are multiple then set `killer_id` to NA
        # so that in later join these games will be filtered out
        group_by(game_id) %>%
        summarise(
            n_events_at_min = n(),
            killer_id = if_else(n() == 1, first(killer_id), NA),
            .groups = "drop"
        ) %>%
        filter(!is.na(killer_id)) %>%
        left_join(lookup_data, by = c("game_id", "killer_id" = "player_id")) %>%
        left_join(games_data %>% select(game_id, team1_id, team2_id), by = "game_id") %>%
        mutate(first_event_team = case_when(
            n_events_at_min > 1 ~ "none",
            team_id == team1_id ~ "team1",
            team_id == team2_id ~ "team2",
            TRUE ~ "none"
        )) %>%
        select(game_id, first_event_team)
}

champ_types <- function(games, player_stats, champs) {
    player_stats %>%
        filter(game_id %in% games$game_id) %>%
        select(game_id, team_id, champion_name) %>%
        left_join(champs, by = c("champion_name" = "name")) %>%
        left_join(games %>% select(game_id, team1_id, team2_id), by = "game_id") %>%
        mutate(team_label = case_when(
            team_id == team1_id ~ "team1",
            team_id == team2_id ~ "team2")) %>%
        count(game_id, team_label, herotype, name = "n") %>%
        pivot_wider(
            names_from = c(team_label, herotype),
            values_from = n,
            values_fill = 0,
            names_glue = "{team_label}_{herotype}") %>%
		mutate(
			fighter_champdiff = team1_Fighter - team2_Fighter,
			mage_champdiff = team1_Mage - team2_Mage,
			assassin_champdiff = team1_Assassin - team2_Assassin,
			marksman_champdiff = team1_Marksman - team2_Marksman,
			tank_champdiff = team1_Tank - team2_Tank,
			support_champdiff = team1_Support - team2_Support) %>%
		select(game_id, fighter_champdiff, mage_champdiff, assassin_champdiff,
			   marksman_champdiff, tank_champdiff, support_champdiff)
}


#' From the raw data creates the early game state dataset. It either creates or
#' loads a cached early game state dataset based on timestamp.
early_game_dataset <- function(player_stats_df, metadata_df, events_df,
                               champs_df, ts, cache_dir = "./data/",
                               cache_base_name = "processed-dataset_ts") {
    cache_file_name <- paste0(cache_base_name, ts, ".csv")
    cache_path <- file.path(cache_dir, cache_file_name)

    if (file.exists(cache_path)) {
        cached_data <- read_csv(cache_path, show_col_types = FALSE)
        cached_data <- cached_data %>%
            mutate(
                first_blood = factor(first_blood),
                first_dragon = factor(first_dragon),
                first_herald = factor(first_herald),
                team1_won = as.integer(team1_won)
            ) %>%
            mutate(across(ends_with("_diff"), as.numeric))

        return(cached_data)
    }

    games <- teams_in_games(player_stats_df)

    player_team_lookup <- player_stats_df %>%
        filter(game_id %in% games$game_id) %>%
        select(game_id, player_id, team_id)

    events <- events_df %>%
        filter(game_id %in% games$game_id & timestamp <= ts)

    event_types_to_count <- c("player_kill", "drake_kill", "rift_herald_kill", "tower_kill")
    event_counts <- event_counts_by_type(events, event_types_to_count, player_team_lookup, games)
    diffs <- calculate_diffs(games, event_counts)

    first_bloods <- find_first_event(events, player_team_lookup, games, "player_kill") %>%
        rename(first_blood = first_event_team)

    first_dragons <- find_first_event(events, player_team_lookup, games, "drake_kill") %>%
        rename(first_dragon = first_event_team)

    first_heralds <- find_first_event(events, player_team_lookup, games, "rift_herald_kill") %>%
        rename(first_herald = first_event_team)

    first_towers <- find_first_event(events, player_team_lookup, games, "tower_kill") %>%
        rename(first_tower = first_event_team)

    champs <- champ_types(games, player_stats_df, champs_df)

    dataset <- games %>%
        left_join(diffs, by = "game_id") %>%
        left_join(first_bloods, by = "game_id") %>%
        left_join(first_dragons, by = "game_id") %>%
        left_join(first_heralds, by = "game_id") %>%
        left_join(first_towers, by = "game_id") %>%
        left_join(champs, by = "game_id") %>%
        mutate(
            first_blood = factor(replace_na(first_blood, "none")),
            first_dragon = factor(replace_na(first_dragon, "none")),
            first_tower = factor(replace_na(first_tower, "none")),
            first_herald = factor(replace_na(first_herald, "none"))
        )


    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
    }
    write_csv(dataset, cache_path)

    dataset
}
