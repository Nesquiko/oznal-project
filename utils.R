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
			.groups = "drop") %>%
		# filter out those games that don't have stats for both whole teams,
		# example game_id 13358 has only one team
		filter(n_players == 10, n_distinct_teams == 2) %>%
		mutate(team1_won = as.integer(team1_won)) %>%
		select(game_id, team1_id, team2_id, team1_won)
}

event_counts_by_type <- function(events, event_types, player_team_lookup, games) {
	events %>%
		filter(event_type %in% event_types) %>%
		left_join(player_team_lookup, by = c("game_id", "killer_id" = "player_id")) %>%
		left_join(games, by = "game_id") %>%
		mutate(event_team = case_when(
			team_id == team1_id ~ "team1",
			team_id == team2_id ~ "team2",
			TRUE ~ "other")) %>%
		filter(event_team != "other") %>%
		count(game_id, event_type, event_team, name = "n") %>%
		pivot_wider(
			names_from = c(event_type, event_team),
			values_from = n,
			values_fill = 0)
}

calculate_diffs <- function(games, event_counts) {
	games %>%
		select(game_id) %>%
		left_join(event_counts, by = "game_id") %>%
		mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
		mutate(
			kill_diff = player_kill_team1 - player_kill_team2,
			dragon_diff = dragon_kill_team1 - dragon_kill_team2,
			rift_herald_diff = rift_herald_kill_team1 - rift_herald_kill_team2,
			tower_diff = tower_kill_team1 - tower_kill_team2,
			grub_diff = voidgrub_kill_team1 - voidgrub_kill_team2) %>%
		select(game_id, kill_diff, dragon_diff, rift_herald_diff, tower_diff, grub_diff)
}

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
			relationship = "one-to-many") %>%
		# count events at the min ts, if there are multiple then set `killer_id` to NA
		# so that in later join these games will be filtered out
		group_by(game_id) %>%
		summarise(
			n_events_at_min = n(),
			killer_id = if_else(n() == 1, first(killer_id), NA),
			.groups = "drop") %>%
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


early_game_dataset <- function(player_stats_df, metadata_df, events_df, ts) {
	games <- teams_in_games(player_stats_df)
	
	player_team_lookup <- player_stats_df %>%
		filter(game_id %in% games$game_id) %>%
		select(game_id, player_id, team_id)
	
	events <- events_df %>%
		filter(game_id %in% games$game_id & timestamp <= ts)
 
	event_types_to_count <- c("player_kill", "dragon_kill", "rift_herald_kill", "tower_kill", "voidgrub_kill" )
	event_counts <- event_counts_by_type(events, event_types_to_count, player_team_lookup, games)

	diffs <- calculate_diffs(games, event_counts)
	
	first_bloods <- find_first_event(events, player_team_lookup, games, "player_kill") %>%
		rename(first_blood = first_event_team)
	first_dragons <- find_first_event(events, player_team_lookup, games, "dragon_kill") %>%
		rename(first_dragon = first_event_team)
	first_heralds <- find_first_event(events, player_team_lookup, games, "rift_herald_kill") %>%
		rename(first_herald = first_event_team)
	
	games %>%
		left_join(diffs, by = "game_id") %>%
		left_join(first_bloods, by = "game_id") %>%
		left_join(first_dragons, by = "game_id") %>%
		left_join(first_heralds, by = "game_id") %>%
		mutate(
			first_blood = factor(replace_na(first_blood, "none")),
			first_dragon = factor(replace_na(first_dragon, "none")),
			first_herald = factor(replace_na(first_herald, "none")))
}
